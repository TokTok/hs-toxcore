/* SPDX-License-Identifier: GPL-3.0-or-later
 * Copyright © 2026 The TokTok team.
 */

/*
 * A c-toxcore peer for interoperability tests.
 *
 * It adds the friends given with --friend without sending friend requests,
 * sends friend requests to the Tox IDs given with --add, accepts friend
 * requests with --accept-requests,
 * prints its DHT key, UDP port and public key, then echoes every message it
 * receives back to the sender.  With --dht-nodes it also runs that many more
 * Tox instances, connected to it, as a local DHT and onion network.  With
 * --no-udp it reaches the network only through the --tcp-relay.  With
 * --file-dir it saves the files it receives there and sends them back.
 * Custom lossless and lossy packets go back with the same id.  With --av it
 * answers calls and sends their audio and video back, and with --av-call it
 * also calls friends when they come online.
 */
#ifndef _POSIX_C_SOURCE
#define _POSIX_C_SOURCE 200809L
#endif

#include <netdb.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/socket.h>
#include <time.h>
#include <unistd.h>

#include <tox/tox.h>
#include <tox/toxav.h>

/* "echo: " and the message, cut to fit, without the terminating NUL. */
static size_t make_echo(char *echo, size_t size, const uint8_t *message, size_t length) {
  const int n = snprintf(echo, size, "echo: %.*s", (int)length, (const char *)message);
  return n < 0 ? 0 : (size_t)n < size ? (size_t)n : size - 1;
}

static bool hex_to_bin(const char *hex, uint8_t *bin, size_t length) {
  if (strlen(hex) != length * 2) {
    return false;
  }

  for (size_t i = 0; i < length; ++i) {
    unsigned int byte;

    if (sscanf(hex + i * 2, "%2x", &byte) != 1) {
      return false;
    }

    bin[i] = (uint8_t)byte;
  }

  return true;
}

static void print_hex(const char *label, const uint8_t *bin, size_t length) {
  if (label[0] != '\0') {
    printf("%s ", label);
  }

  for (size_t i = 0; i < length; ++i) {
    printf("%02X", bin[i]);
  }

  printf("\n");
}

/* Print a group's chat id, or why we have none. */
static void print_chat_id(const char *label, const Tox *tox, uint32_t group) {
  uint8_t                   chat_id[TOX_GROUP_CHAT_ID_SIZE];
  Tox_Err_Group_State_Query err;

  if (tox_group_get_chat_id(tox, group, chat_id, &err)) {
    print_hex(label, chat_id, sizeof(chat_id));
  } else {
    printf("%s_FAILED %s\n", label, tox_err_group_state_query_to_string(err));
  }
}

static void log_cb(Tox *tox, Tox_Log_Level level, const char *file, uint32_t line, const char *func,
                   const char *message, void *user_data) {
  const bool *verbose = (const bool *)user_data;

  if (level >= TOX_LOG_LEVEL_WARNING || *verbose) {
    fprintf(stderr, "[%s] %s:%u %s: %s\n", tox_log_level_to_string(level), file, line, func,
            message);
  }
}

static const char *group_new_name    = NULL;
static bool        group_created     = false;
static uint32_t    group_number      = 0;
static const char *group_public_name = NULL;
static const char *group_join_id     = NULL;
static bool        group_moderate    = false;

static void free_friend_files(uint32_t friend_number);

/* With --conference-new, friends get an invite to a new conference when they
 * connect: an audio one with --av, which we send silence to. */
static const char *conference_new_title = NULL;
static bool        conference_created   = false;
static uint32_t    conference_number    = 0;
static bool        conference_heard     = false;

static ToxAV *av      = NULL;
static bool   av_call = false;

enum { AV_AUDIO_BIT_RATE = 48, AV_VIDEO_BIT_RATE = 1000 };

/* The last frame of each kind, sent back after toxav_iterate, which holds the
 * lock that sending takes. */
static struct {
  bool     audio, video;
  uint32_t audio_friend, video_friend;
  int16_t  pcm[5760 * 2];
  size_t   sample_count;
  uint8_t  channels;
  uint32_t sampling_rate;
  uint8_t *planes;
  uint16_t width, height;
} echo_frame;

static void av_call_cb(ToxAV *toxav, uint32_t friend_number, bool audio_enabled, bool video_enabled,
                       void *user_data) {
  printf("AV_CALL %u %d %d\n", friend_number, audio_enabled, video_enabled);
  Toxav_Err_Answer err;
  toxav_answer(toxav, friend_number, AV_AUDIO_BIT_RATE, AV_VIDEO_BIT_RATE, &err);
  printf("AV_ANSWER %u %d\n", friend_number, err);
}

static void av_call_state_cb(ToxAV *toxav, uint32_t friend_number, uint32_t state, void *user_data) {
  printf("AV_STATE %u %u\n", friend_number, state);
}

static void av_audio_cb(ToxAV *toxav, uint32_t friend_number, const int16_t pcm[], size_t sample_count,
                        uint8_t channels, uint32_t sampling_rate, void *user_data) {
  printf("AV_AUDIO %u %zu %u %u\n", friend_number, sample_count, channels, sampling_rate);

  if (sample_count * channels > sizeof(echo_frame.pcm) / sizeof(echo_frame.pcm[0])) {
    return;
  }

  memcpy(echo_frame.pcm, pcm, sample_count * channels * sizeof(pcm[0]));
  echo_frame.audio         = true;
  echo_frame.audio_friend  = friend_number;
  echo_frame.sample_count  = sample_count;
  echo_frame.channels      = channels;
  echo_frame.sampling_rate = sampling_rate;
}

static void copy_plane(uint8_t *dst, const uint8_t *src, int32_t stride, uint16_t width, uint16_t height) {
  for (uint16_t row = 0; row < height; ++row) {
    memcpy(dst + (size_t)row * width, src + (size_t)row * (size_t)abs(stride), width);
  }
}

static void av_video_cb(ToxAV *toxav, uint32_t friend_number, uint16_t width, uint16_t height,
                        const uint8_t *y, const uint8_t *u, const uint8_t *v, int32_t ystride,
                        int32_t ustride, int32_t vstride, void *user_data) {
  printf("AV_VIDEO %u %u %u\n", friend_number, width, height);
  const uint16_t chroma_width  = (uint16_t)((width + 1) / 2);
  const uint16_t chroma_height = (uint16_t)((height + 1) / 2);
  const size_t   luma          = (size_t)width * height;
  const size_t   chroma        = (size_t)chroma_width * chroma_height;
  uint8_t       *planes        = realloc(echo_frame.planes, luma + 2 * chroma);

  if (planes == NULL) {
    return;
  }

  copy_plane(planes, y, ystride, width, height);
  copy_plane(planes + luma, u, ustride, chroma_width, chroma_height);
  copy_plane(planes + luma + chroma, v, vstride, chroma_width, chroma_height);
  echo_frame.planes       = planes;
  echo_frame.video        = true;
  echo_frame.video_friend = friend_number;
  echo_frame.width        = width;
  echo_frame.height       = height;
}

static void send_echo_frames(void) {
  if (echo_frame.audio) {
    toxav_audio_send_frame(av, echo_frame.audio_friend, echo_frame.pcm, echo_frame.sample_count,
                           echo_frame.channels, echo_frame.sampling_rate, NULL);
    echo_frame.audio = false;
  }

  if (echo_frame.video) {
    const size_t luma   = (size_t)echo_frame.width * echo_frame.height;
    const size_t chroma = (size_t)((echo_frame.width + 1) / 2) * ((echo_frame.height + 1) / 2);
    toxav_video_send_frame(av, echo_frame.video_friend, echo_frame.width, echo_frame.height,
                           echo_frame.planes, echo_frame.planes + luma, echo_frame.planes + luma + chroma,
                           NULL);
    echo_frame.video = false;
  }
}

static void conference_audio_cb(void *tox, Tox_Conference_Number conference, Tox_Conference_Peer_Number peer,
                               const int16_t pcm[], uint32_t samples, uint8_t channels, uint32_t sample_rate,
                               void *user_data) {
  if (!conference_heard) {
    printf("CONFERENCE_AUDIO %u\n", conference);
    conference_heard = true;
  }
}

static void invite_to_conference(Tox *tox, uint32_t friend_number) {
  if (!conference_created) {
    int32_t created = -1;

    if (av != NULL) {
      created = toxav_add_av_groupchat(tox, conference_audio_cb, NULL);
    } else {
      Tox_Err_Conference_New err;
      const uint32_t number = tox_conference_new(tox, &err);
      created = err == TOX_ERR_CONFERENCE_NEW_OK ? (int32_t)number : -1;
    }

    if (created < 0) {
      printf("CONFERENCE_NEW_FAILED\n");
      return;
    }

    conference_created = true;
    conference_number  = (uint32_t)created;
    tox_conference_set_title(tox, conference_number, (const uint8_t *)conference_new_title,
                             strlen(conference_new_title), NULL);
    printf("CONFERENCE_NEW %u\n", conference_number);
  }

  Tox_Err_Conference_Invite err;
  tox_conference_invite(tox, friend_number, conference_number, &err);
  printf("CONFERENCE_INVITED %u %s\n", conference_number, tox_err_conference_invite_to_string(err));
}

/* With --group-new, friends get an invite to a new private group when they
 * connect. */
static void friend_connection_status_cb(Tox *tox, uint32_t friend_number,
                                        Tox_Connection connection_status, void *user_data) {
  printf("FRIEND_CONNECTION %u %s\n", friend_number, tox_connection_to_string(connection_status));

  if (av_call && connection_status != TOX_CONNECTION_NONE) {
    Toxav_Err_Call err;
    toxav_call(av, friend_number, AV_AUDIO_BIT_RATE, AV_VIDEO_BIT_RATE, &err);
    printf("AV_CALLING %u %d\n", friend_number, err);
  }

  /* toxcore ends the transfers of a friend who goes offline. */
  if (connection_status == TOX_CONNECTION_NONE) {
    free_friend_files(friend_number);
  }

  if (conference_new_title != NULL && connection_status != TOX_CONNECTION_NONE) {
    invite_to_conference(tox, friend_number);
  }

  if (group_new_name == NULL || connection_status == TOX_CONNECTION_NONE) {
    return;
  }

  if (group_created) {
    Tox_Err_Group_Invite_Friend err_invite;
    tox_group_invite_friend(tox, group_number, friend_number, &err_invite);
    printf("GROUP_INVITED %u %s\n", group_number,
           tox_err_group_invite_friend_to_string(err_invite));
    return;
  }

  const char       *nick = "C peer";
  Tox_Err_Group_New err_new;
  const uint32_t    group =
      tox_group_new(tox, TOX_GROUP_PRIVACY_STATE_PRIVATE, (const uint8_t *)group_new_name,
                    strlen(group_new_name), (const uint8_t *)nick, strlen(nick), &err_new);

  if (err_new != TOX_ERR_GROUP_NEW_OK) {
    printf("GROUP_NEW_FAILED %s\n", tox_err_group_new_to_string(err_new));
    return;
  }

  group_created = true;
  group_number  = group;
  print_chat_id("GROUP_CHAT_ID", tox, group);
  const char *topic = "C topic";
  tox_group_set_topic(tox, group, (const uint8_t *)topic, strlen(topic), NULL);
  Tox_Err_Group_Invite_Friend err_invite;
  tox_group_invite_friend(tox, group, friend_number, &err_invite);
  printf("GROUP_INVITED %u %s\n", group, tox_err_group_invite_friend_to_string(err_invite));
}

static void group_invite_cb(Tox *tox, uint32_t friend_number, const uint8_t *invite_data,
                            size_t length, const uint8_t *group_name, size_t group_name_length,
                            void *user_data) {
  const char                 *nick = "C peer";
  Tox_Err_Group_Invite_Accept err;
  const uint32_t              group = tox_group_invite_accept(
      tox, friend_number, invite_data, length, (const uint8_t *)nick, strlen(nick), NULL, 0, &err);
  printf("GROUP_INVITE %.*s %s %u\n", (int)group_name_length, (const char *)group_name,
         tox_err_group_invite_accept_to_string(err), group);
}

static void group_self_join_cb(Tox *tox, uint32_t group_number, void *user_data) {
  printf("GROUP_SELF_JOIN %u\n", group_number);
}

static void group_peer_join_cb(Tox *tox, uint32_t group_number, uint32_t peer_id, void *user_data) {
  uint8_t name[TOX_MAX_NAME_LENGTH];
  tox_group_peer_get_name(tox, group_number, peer_id, name, NULL);
  const size_t length = tox_group_peer_get_name_size(tox, group_number, peer_id, NULL);
  printf("GROUP_PEER_JOIN %u %.*s\n", group_number, (int)length, (const char *)name);

  /* With --group-moderate, the founder tries every setting and role on the peer, then kicks it. */
  if (!group_moderate ||
      tox_group_self_get_role(tox, group_number, NULL) != TOX_GROUP_ROLE_FOUNDER) {
    return;
  }

  printf("GROUP_SET %d %d %d %d\n", tox_group_set_peer_limit(tox, group_number, 50, NULL),
         tox_group_set_voice_state(tox, group_number, TOX_GROUP_VOICE_STATE_MODERATOR, NULL),
         tox_group_set_topic_lock(tox, group_number, TOX_GROUP_TOPIC_LOCK_DISABLED, NULL),
         tox_group_set_password(tox, group_number, (const uint8_t *)"secret", 6, NULL));
  const Tox_Group_Role roles[] = {TOX_GROUP_ROLE_MODERATOR, TOX_GROUP_ROLE_OBSERVER,
                                  TOX_GROUP_ROLE_USER};

  for (size_t i = 0; i < sizeof(roles) / sizeof(roles[0]); ++i) {
    Tox_Err_Group_Set_Role err;
    tox_group_set_role(tox, group_number, peer_id, roles[i], &err);
    printf("GROUP_SET_ROLE %s\n", tox_err_group_set_role_to_string(err));
  }

  Tox_Err_Group_Kick_Peer err_kick;
  tox_group_kick_peer(tox, group_number, peer_id, &err_kick);
  printf("GROUP_KICK %s\n", tox_err_group_kick_peer_to_string(err_kick));
}

/* Enum names without their common prefix. */
static const char *short_name(const char *name, const char *prefix) {
  return strncmp(name, prefix, strlen(prefix)) == 0 ? name + strlen(prefix) : name;
}

static void group_moderation_cb(Tox *tox, uint32_t group_number, uint32_t source_peer_id,
                                uint32_t target_peer_id, Tox_Group_Mod_Event mod_type,
                                void *user_data) {
  printf("GROUP_MODERATION %u %s\n", group_number,
         short_name(tox_group_mod_event_to_string(mod_type), "TOX_GROUP_MOD_EVENT_"));
}

static void group_peer_limit_cb(Tox *tox, uint32_t group_number, uint32_t peer_limit,
                                void *user_data) {
  printf("GROUP_PEER_LIMIT %u %u\n", group_number, peer_limit);
}

static void group_voice_state_cb(Tox *tox, uint32_t group_number, Tox_Group_Voice_State voice_state,
                                 void *user_data) {
  printf("GROUP_VOICE %u %s\n", group_number,
         short_name(tox_group_voice_state_to_string(voice_state), "TOX_GROUP_VOICE_STATE_"));
}

static void group_topic_lock_cb(Tox *tox, uint32_t group_number, Tox_Group_Topic_Lock topic_lock,
                                void *user_data) {
  printf("GROUP_TOPIC_LOCK %u %s\n", group_number,
         short_name(tox_group_topic_lock_to_string(topic_lock), "TOX_GROUP_TOPIC_LOCK_"));
}

static void group_password_cb(Tox *tox, uint32_t group_number, const uint8_t *password,
                              size_t length, void *user_data) {
  printf("GROUP_PASSWORD %u %.*s\n", group_number, (int)length, (const char *)password);
}

static void group_peer_exit_cb(Tox *tox, uint32_t group_number, uint32_t peer_id,
                               Tox_Group_Exit_Type exit_type, const uint8_t *name,
                               size_t name_length, const uint8_t *part_message,
                               size_t part_message_length, void *user_data) {
  printf("GROUP_PEER_EXIT %u %.*s %s\n", group_number, (int)name_length, (const char *)name,
         tox_group_exit_type_to_string(exit_type));
}

static void group_topic_cb(Tox *tox, uint32_t group_number, uint32_t peer_id, const uint8_t *topic,
                           size_t length, void *user_data) {
  printf("GROUP_TOPIC %u %.*s\n", group_number, (int)length, (const char *)topic);
}

static void group_join_fail_cb(Tox *tox, uint32_t group_number, Tox_Group_Join_Fail fail_type,
                               void *user_data) {
  printf("GROUP_JOIN_FAIL %u %s\n", group_number, tox_group_join_fail_to_string(fail_type));
}

/* Echo group messages from others back to the group, but not echoes. */
static void group_message_cb(Tox *tox, uint32_t group_number, uint32_t peer_id,
                             Tox_Message_Type type, const uint8_t *message, size_t length,
                             uint32_t message_id, void *user_data) {
  printf("GROUP_MESSAGE %u %.*s\n", group_number, (int)length, (const char *)message);

  if (length >= 5 && memcmp(message, "echo:", 5) == 0) {
    return;
  }

  char echo[TOX_MAX_MESSAGE_LENGTH];
  tox_group_send_message(tox, group_number, type, (const uint8_t *)echo,
                         make_echo(echo, sizeof(echo), message, length), NULL);
}

/* And private ones privately. */
static void group_private_message_cb(Tox *tox, uint32_t group_number, uint32_t peer_id,
                                     Tox_Message_Type type, const uint8_t *message, size_t length,
                                     uint32_t message_id, void *user_data) {
  printf("GROUP_PRIVATE_MESSAGE %u %.*s\n", group_number, (int)length, (const char *)message);

  if (length >= 5 && memcmp(message, "echo:", 5) == 0) {
    return;
  }

  char echo[TOX_MAX_MESSAGE_LENGTH];
  tox_group_send_private_message(tox, group_number, peer_id, type, (const uint8_t *)echo,
                                 make_echo(echo, sizeof(echo), message, length), NULL);
}

/* Echo custom group packets the same way, losslessly. */
static void group_custom_packet_cb(Tox *tox, uint32_t group_number, uint32_t peer_id,
                                   const uint8_t *data, size_t length, void *user_data) {
  printf("GROUP_CUSTOM %u %.*s\n", group_number, (int)length, (const char *)data);

  if (length >= 5 && memcmp(data, "echo:", 5) == 0) {
    return;
  }

  char echo[TOX_MAX_CUSTOM_PACKET_SIZE];
  tox_group_send_custom_packet(tox, group_number, true, (const uint8_t *)echo,
                               make_echo(echo, sizeof(echo), data, length), NULL);
}

/* The packet starts with its id, which the echo keeps. */
static size_t make_packet_echo(uint8_t *echo, size_t size, const uint8_t *data, size_t length) {
  echo[0] = data[0];
  return 1 + make_echo((char *)echo + 1, size - 1, data + 1, length - 1);
}

static void friend_lossless_packet_cb(Tox *tox, uint32_t friend_number, const uint8_t *data,
                                      size_t length, void *user_data) {
  printf("LOSSLESS_PACKET %u %.*s\n", friend_number, (int)length - 1, (const char *)data + 1);

  if (length >= 6 && memcmp(data + 1, "echo:", 5) == 0) {
    return;
  }

  uint8_t echo[TOX_MAX_CUSTOM_PACKET_SIZE];
  tox_friend_send_lossless_packet(tox, friend_number, echo,
                                  make_packet_echo(echo, sizeof(echo), data, length), NULL);
}

static void friend_lossy_packet_cb(Tox *tox, uint32_t friend_number, const uint8_t *data,
                                   size_t length, void *user_data) {
  printf("LOSSY_PACKET %u %.*s\n", friend_number, (int)length - 1, (const char *)data + 1);

  if (length >= 6 && memcmp(data + 1, "echo:", 5) == 0) {
    return;
  }

  uint8_t echo[TOX_MAX_CUSTOM_PACKET_SIZE];
  tox_friend_send_lossy_packet(tox, friend_number, echo,
                               make_packet_echo(echo, sizeof(echo), data, length), NULL);
}

static void friend_name_cb(Tox *tox, uint32_t friend_number, const uint8_t *name, size_t length,
                           void *user_data) {
  printf("FRIEND_NAME %u %.*s\n", friend_number, (int)length, (const char *)name);
}

static void friend_request_cb(Tox *tox, const uint8_t *public_key, const uint8_t *message,
                              size_t length, void *user_data) {
  print_hex("FRIEND_REQUEST", public_key, TOX_PUBLIC_KEY_SIZE);
  printf("FRIEND_REQUEST_MESSAGE %.*s\n", (int)length, (const char *)message);
  tox_friend_add_norequest(tox, public_key, NULL);
}

static void friend_message_cb(Tox *tox, uint32_t friend_number, Tox_Message_Type type,
                              const uint8_t *message, size_t length, void *user_data) {
  printf("MESSAGE %u %.*s\n", friend_number, (int)length, (const char *)message);
  tox_friend_send_message(tox, friend_number, type, message, length, NULL);
}

/* A file we receive into memory, then send back. */
/* Files are kept in memory, so this is as big as they get. */
#define MAX_FILE_SIZE (64 * 1024 * 1024)

typedef struct File_Buffer {
  bool     used;
  bool     outgoing;
  uint32_t friend_number;
  uint32_t file_number;
  uint32_t kind;
  char     name[TOX_MAX_FILENAME_LENGTH + 1];
  uint8_t *data;
  uint64_t size;
  /* Requested bytes that toxcore had no room for yet. */
  uint64_t unsent_from;
  uint64_t unsent_to;
  /* toxcore asks for chunks of one size, except the last. */
  size_t chunk_size;
} File_Buffer;

static File_Buffer files[16];
static const char *file_dir = NULL;

static File_Buffer *find_file(uint32_t friend_number, uint32_t file_number, bool outgoing) {
  for (size_t i = 0; i < sizeof(files) / sizeof(files[0]); ++i) {
    if (files[i].used && files[i].outgoing == outgoing && files[i].friend_number == friend_number &&
        files[i].file_number == file_number) {
      return &files[i];
    }
  }

  return NULL;
}

static void free_file(File_Buffer *f) {
  free(f->data);
  f->data = NULL;
  f->used = false;
}

static void free_friend_files(uint32_t friend_number) {
  for (size_t i = 0; i < sizeof(files) / sizeof(files[0]); ++i) {
    if (files[i].used && files[i].friend_number == friend_number) {
      free_file(&files[i]);
    }
  }
}

static void file_recv_control_cb(Tox *tox, uint32_t friend_number, uint32_t file_number,
                                 Tox_File_Control control, void *user_data) {
  if (control != TOX_FILE_CONTROL_CANCEL) {
    return;
  }

  File_Buffer *f = find_file(friend_number, file_number, false);

  if (f == NULL) {
    f = find_file(friend_number, file_number, true);
  }

  if (f != NULL) {
    printf("FILE_CANCELLED %s\n", f->name);
    free_file(f);
  }
}

static void file_recv_cb(Tox *tox, uint32_t friend_number, uint32_t file_number, uint32_t kind,
                         uint64_t file_size, const uint8_t *filename, size_t filename_length,
                         void *user_data) {
  /* A name that is not a plain file name, or a size we cannot hold, is refused. */
  const bool bad_name = filename_length == 0 || memchr(filename, '/', filename_length) != NULL ||
                        (filename_length <= 2 && memcmp(filename, "..", filename_length) == 0);

  for (size_t i = 0;
       !bad_name && file_size <= MAX_FILE_SIZE && i < sizeof(files) / sizeof(files[0]); ++i) {
    if (!files[i].used) {
      File_Buffer *f = &files[i];
      f->data        = (uint8_t *)malloc(file_size > 0 ? file_size : 1);

      if (f->data == NULL) {
        break;
      }

      f->used          = true;
      f->outgoing      = false;
      f->friend_number = friend_number;
      f->file_number   = file_number;
      f->kind          = kind;
      f->size          = file_size;
      f->unsent_from   = 0;
      f->unsent_to     = 0;
      f->chunk_size    = 0;
      snprintf(f->name, sizeof(f->name), "%.*s", (int)filename_length, (const char *)filename);
      tox_file_control(tox, friend_number, file_number, TOX_FILE_CONTROL_RESUME, NULL);
      return;
    }
  }

  tox_file_control(tox, friend_number, file_number, TOX_FILE_CONTROL_CANCEL, NULL);
}

static void file_recv_chunk_cb(Tox *tox, uint32_t friend_number, uint32_t file_number,
                               uint64_t position, const uint8_t *data, size_t length,
                               void *user_data) {
  File_Buffer *f = find_file(friend_number, file_number, false);

  if (f == NULL) {
    return;
  }

  if (length > 0) {
    if (position + length <= f->size) {
      memcpy(f->data + position, data, length);
    }

    return;
  }

  char path[4096];
  snprintf(path, sizeof(path), "%s/%s", file_dir, f->name);
  FILE *out = fopen(path, "wb");

  if (out != NULL) {
    fwrite(f->data, 1, f->size, out);
    fclose(out);
  }

  printf("FILE_RECEIVED %s %lu\n", f->name, (unsigned long)f->size);

  char echo_name[TOX_MAX_FILENAME_LENGTH + 1];
  snprintf(echo_name, sizeof(echo_name), "echo-%s", f->name);
  Tox_Err_File_Send err;
  const uint32_t    n = tox_file_send(tox, friend_number, f->kind, f->size, NULL,
                                      (const uint8_t *)echo_name, strlen(echo_name), &err);

  if (err != TOX_ERR_FILE_SEND_OK) {
    printf("FILE_SEND_FAILED %s\n", tox_err_file_send_to_string(err));
    free_file(f);
    return;
  }

  f->outgoing    = true;
  f->file_number = n;
  snprintf(f->name, sizeof(f->name), "%s", echo_name);
}

/* A chunk toxcore refuses for a full queue must be sent again before later
 * ones, which it refuses until then. */
static void send_unsent_chunks(Tox *tox, File_Buffer *f) {
  while (f->unsent_from < f->unsent_to) {
    const uint64_t          length = f->unsent_to - f->unsent_from < f->chunk_size
                                         ? f->unsent_to - f->unsent_from
                                         : f->chunk_size;
    Tox_Err_File_Send_Chunk err;
    tox_file_send_chunk(tox, f->friend_number, f->file_number, f->unsent_from,
                        f->data + f->unsent_from, length, &err);

    if (err == TOX_ERR_FILE_SEND_CHUNK_SENDQ) {
      return;
    }

    f->unsent_from += length;
  }
}

static void file_chunk_request_cb(Tox *tox, uint32_t friend_number, uint32_t file_number,
                                  uint64_t position, size_t length, void *user_data) {
  File_Buffer *f = find_file(friend_number, file_number, true);

  if (f == NULL) {
    return;
  }

  if (length == 0) {
    printf("FILE_SENT %s\n", f->name);
    free_file(f);
    return;
  }

  if (f->unsent_from == f->unsent_to) {
    f->unsent_from = position;
  }

  f->unsent_to = position + length;

  if (length > f->chunk_size) {
    f->chunk_size = length;
  }

  send_unsent_chunks(tox, f);
}

static void conference_invite_cb(Tox *tox, uint32_t friend_number, Tox_Conference_Type type,
                                 const uint8_t *cookie, size_t length, void *user_data) {
  Tox_Err_Conference_Join err;
  const uint32_t conference = tox_conference_join(tox, friend_number, cookie, length, &err);

  if (err == TOX_ERR_CONFERENCE_JOIN_OK) {
    printf("CONFERENCE_JOINED %u\n", conference);
  } else {
    printf("CONFERENCE_JOIN_FAILED %s\n", tox_err_conference_join_to_string(err));
  }
}

static void conference_title_cb(Tox *tox, uint32_t conference_number, uint32_t peer_number,
                                const uint8_t *title, size_t length, void *user_data) {
  printf("CONFERENCE_TITLE %u %.*s\n", conference_number, (int)length, (const char *)title);
}

/* Echo messages from others back to the conference, but not echoes. */
static void conference_message_cb(Tox *tox, uint32_t conference_number, uint32_t peer_number,
                                  Tox_Message_Type type, const uint8_t *message, size_t length,
                                  void *user_data) {
  if (tox_conference_peer_number_is_ours(tox, conference_number, peer_number, NULL)) {
    return;
  }

  printf("CONFERENCE_MESSAGE %u %.*s\n", conference_number, (int)length, (const char *)message);

  if (length >= 5 && memcmp(message, "echo:", 5) == 0) {
    return;
  }

  char echo[TOX_MAX_MESSAGE_LENGTH];
  tox_conference_send_message(tox, conference_number, type, (const uint8_t *)echo,
                              make_echo(echo, sizeof(echo), message, length), NULL);
}

/* 20 ms of silence for the audio conference, every 20 ms. */
static void send_conference_silence(Tox *tox) {
  enum { SAMPLES = 960, FRAME_NS = 20000000 };
  static struct timespec last;
  struct timespec        now;
  clock_gettime(CLOCK_MONOTONIC, &now);

  if (!conference_created ||
      (now.tv_sec - last.tv_sec) * 1000000000L + (now.tv_nsec - last.tv_nsec) < FRAME_NS) {
    return;
  }

  last = now;
  static const int16_t silence[SAMPLES];
  toxav_group_send_audio(tox, conference_number, silence, SAMPLES, 1, 48000);
}

static void usage(const char *program) {
  fprintf(
      stderr,
      "Usage: %s [--port PORT] [--friend PUBLIC_KEY]... [--add TOX_ID]... [--accept-requests]\n"
      "          [--dht-nodes N] [--tcp-relay-port PORT] [--tcp-relay HOST:PORT:KEY]\n"
      "          [--no-udp] [--bootstrap HOST:PORT:KEY] [--load SAVE_FILE] [--save SAVE_FILE]\n"
      "          [--print-id] [--local-discovery] [--file-dir DIR] [--ipv6] [--group-new NAME]\n"
      "          [--group-public NAME] [--group-join CHAT_ID]\n"
      "          [--group-moderate] [--conference-new TITLE] [--av] [--av-call] [--verbose]\n",
      program);
}

static bool        local_discovery = false;
static bool        ipv6            = false;
static const char *localhost       = "127.0.0.1";

/* Written next to the file and renamed over it, so a killed peer leaves a whole save. */
static void write_save(const Tox *tox, const char *path) {
  const size_t size = tox_get_savedata_size(tox);
  uint8_t     *data = (uint8_t *)malloc(size);

  if (data == NULL) {
    return;
  }

  tox_get_savedata(tox, data);
  char tmp[4096];
  snprintf(tmp, sizeof(tmp), "%s.new", path);
  FILE *file = fopen(tmp, "wb");

  if (file != NULL) {
    const bool written = fwrite(data, 1, size, file) == size;

    if (fclose(file) == 0 && written) {
      rename(tmp, path);
    }
  }

  free(data);
}

static Tox *new_tox(uint16_t port, uint16_t tcp_port, bool udp, const uint8_t *savedata,
                    size_t savedata_length, bool *verbose) {
  Tox_Options *options = tox_options_new(NULL);

  if (options == NULL) {
    return NULL;
  }

  tox_options_set_ipv6_enabled(options, ipv6);
  tox_options_set_local_discovery_enabled(options, local_discovery);
  /* toxcore's default ports are shared by every test running at once, so we
   * pick from our own range unless told which port to use. */
  static uint16_t port_base = 0;

  if (port_base == 0) {
    port_base = (uint16_t)(20000 + ((uint32_t)getpid() * 7919u + (uint32_t)time(NULL)) % 40000);
  }

  tox_options_set_start_port(options, port != 0 ? port : port_base);
  tox_options_set_end_port(options, port != 0 ? port : port_base + 100);
  tox_options_set_tcp_port(options, tcp_port);
  tox_options_set_udp_enabled(options, udp);
  tox_options_set_experimental_groups_persistence(options, true);
  tox_options_set_log_callback(options, log_cb);
  tox_options_set_log_user_data(options, verbose);

  if (savedata != NULL) {
    tox_options_set_savedata_type(options, TOX_SAVEDATA_TYPE_TOX_SAVE);
    tox_options_set_savedata_data(options, savedata, savedata_length);
  }

  Tox_Err_New err_new;
  Tox        *tox = tox_new(options, &err_new);
  tox_options_free(options);

  if (tox == NULL) {
    fprintf(stderr, "Failed to create Tox instance: %s\n", tox_err_new_to_string(err_new));
  }

  return tox;
}

/* Read a whole file, or return NULL. */
static uint8_t *read_file(const char *path, size_t *length) {
  FILE *file = fopen(path, "rb");

  if (file == NULL) {
    return NULL;
  }

  uint8_t *data = NULL;
  *length       = 0;
  uint8_t chunk[4096];
  size_t  n;

  while ((n = fread(chunk, 1, sizeof(chunk), file)) > 0) {
    uint8_t *grown = (uint8_t *)realloc(data, *length + n);

    if (grown == NULL) {
      free(data);
      fclose(file);
      return NULL;
    }

    data = grown;
    memcpy(data + *length, chunk, n);
    *length += n;
  }

  fclose(file);
  return data;
}

/* Whether something listens on the host and TCP port. */
static bool tcp_port_open(const char *host, uint16_t port) {
  char service[8];
  snprintf(service, sizeof(service), "%u", port);
  struct addrinfo hints;
  memset(&hints, 0, sizeof(hints));
  hints.ai_socktype = SOCK_STREAM;
  struct addrinfo *addrs;

  if (getaddrinfo(host, service, &hints, &addrs) != 0) {
    return false;
  }

  bool open = false;

  for (const struct addrinfo *a = addrs; a != NULL && !open; a = a->ai_next) {
    const int sock = socket(a->ai_family, a->ai_socktype, a->ai_protocol);

    if (sock >= 0) {
      open = connect(sock, a->ai_addr, a->ai_addrlen) == 0;
      close(sock);
    }
  }

  freeaddrinfo(addrs);
  return open;
}

/* HOST:PORT:KEY, with an IPv6 host in brackets. */
static bool parse_node(const char *spec, char *host, size_t host_size, uint16_t *port,
                       uint8_t *key) {
  const char *host_end   = spec[0] == '[' ? strchr(spec, ']') : strchr(spec, ':');
  const char *host_start = spec[0] == '[' ? spec + 1 : spec;

  if (host_end == NULL || (size_t)(host_end - host_start) >= host_size) {
    return false;
  }

  const char *rest = spec[0] == '[' ? host_end + 1 : host_end;
  char        key_hex[65];

  if (sscanf(rest, ":%hu:%64s", port, key_hex) != 2 ||
      !hex_to_bin(key_hex, key, TOX_PUBLIC_KEY_SIZE)) {
    return false;
  }

  memcpy(host, host_start, (size_t)(host_end - host_start));
  host[host_end - host_start] = '\0';
  return true;
}

/* Bootstrap each of the two instances from the other. */
static void connect_toxes(Tox *a, Tox *b) {
  uint8_t key[TOX_PUBLIC_KEY_SIZE];
  tox_self_get_dht_id(b, key);
  tox_bootstrap(a, localhost, tox_self_get_udp_port(b, NULL), key, NULL);
  tox_self_get_dht_id(a, key);
  tox_bootstrap(b, localhost, tox_self_get_udp_port(a, NULL), key, NULL);
}

int main(int argc, char **argv) {
  setvbuf(stdout, NULL, _IOLBF, 0);

  uint16_t    port    = 0;
  bool        verbose = false;
  uint8_t     friends[16][TOX_PUBLIC_KEY_SIZE];
  size_t      friend_count = 0;
  uint8_t     requests[16][TOX_ADDRESS_SIZE];
  size_t      request_count   = 0;
  bool        accept_requests = false;
  bool        use_av          = false;
  uint16_t    tcp_relay_port  = 0;
  Tox        *dht_nodes[32];
  size_t      dht_node_count     = 0;
  bool        udp                = true;
  bool        print_id           = false;
  const char *save_file          = NULL;
  time_t      last_save          = 0;
  uint8_t    *savedata           = NULL;
  size_t      savedata_length    = 0;
  char        relay_host[64]     = "";
  uint16_t    relay_port         = 0;
  char        bootstrap_host[64] = "";
  uint16_t    bootstrap_port     = 0;
  uint8_t     bootstrap_key[TOX_PUBLIC_KEY_SIZE];
  uint8_t     relay_key[TOX_PUBLIC_KEY_SIZE];

  for (int i = 1; i < argc; ++i) {
    if (strcmp(argv[i], "--port") == 0 && i + 1 < argc) {
      port = (uint16_t)atoi(argv[++i]);
    } else if (strcmp(argv[i], "--friend") == 0 && i + 1 < argc && friend_count < 16) {
      if (!hex_to_bin(argv[++i], friends[friend_count], TOX_PUBLIC_KEY_SIZE)) {
        fprintf(stderr, "Invalid public key: %s\n", argv[i]);
        return 1;
      }

      ++friend_count;
    } else if (strcmp(argv[i], "--add") == 0 && i + 1 < argc && request_count < 16) {
      if (!hex_to_bin(argv[++i], requests[request_count], TOX_ADDRESS_SIZE)) {
        fprintf(stderr, "Invalid Tox ID: %s\n", argv[i]);
        return 1;
      }

      ++request_count;
    } else if (strcmp(argv[i], "--tcp-relay-port") == 0 && i + 1 < argc) {
      tcp_relay_port = (uint16_t)atoi(argv[++i]);
    } else if (strcmp(argv[i], "--tcp-relay") == 0 && i + 1 < argc) {
      if (!parse_node(argv[++i], relay_host, sizeof(relay_host), &relay_port, relay_key)) {
        fprintf(stderr, "Invalid TCP relay: %s\n", argv[i]);
        return 1;
      }
    } else if (strcmp(argv[i], "--bootstrap") == 0 && i + 1 < argc) {
      if (!parse_node(argv[++i], bootstrap_host, sizeof(bootstrap_host), &bootstrap_port,
                      bootstrap_key)) {
        fprintf(stderr, "Invalid bootstrap node: %s\n", argv[i]);
        return 1;
      }
    } else if (strcmp(argv[i], "--no-udp") == 0) {
      udp = false;
    } else if (strcmp(argv[i], "--load") == 0 && i + 1 < argc) {
      savedata = read_file(argv[++i], &savedata_length);

      if (savedata == NULL) {
        fprintf(stderr, "Cannot read %s\n", argv[i]);
        return 1;
      }
    } else if (strcmp(argv[i], "--save") == 0 && i + 1 < argc) {
      save_file = argv[++i];
    } else if (strcmp(argv[i], "--group-new") == 0 && i + 1 < argc) {
      group_new_name = argv[++i];
    } else if (strcmp(argv[i], "--conference-new") == 0 && i + 1 < argc) {
      conference_new_title = argv[++i];
    } else if (strcmp(argv[i], "--group-public") == 0 && i + 1 < argc) {
      group_public_name = argv[++i];
    } else if (strcmp(argv[i], "--group-moderate") == 0) {
      group_moderate = true;
    } else if (strcmp(argv[i], "--group-join") == 0 && i + 1 < argc) {
      group_join_id = argv[++i];
    } else if (strcmp(argv[i], "--file-dir") == 0 && i + 1 < argc) {
      file_dir = argv[++i];
    } else if (strcmp(argv[i], "--ipv6") == 0) {
      ipv6      = true;
      localhost = "::1";
    } else if (strcmp(argv[i], "--local-discovery") == 0) {
      local_discovery = true;
    } else if (strcmp(argv[i], "--print-id") == 0) {
      print_id = true;
    } else if (strcmp(argv[i], "--av") == 0) {
      use_av = true;
    } else if (strcmp(argv[i], "--av-call") == 0) {
      use_av  = true;
      av_call = true;
    } else if (strcmp(argv[i], "--accept-requests") == 0) {
      accept_requests = true;
    } else if (strcmp(argv[i], "--dht-nodes") == 0 && i + 1 < argc) {
      dht_node_count = (size_t)atoi(argv[++i]);

      if (dht_node_count > 32) {
        fprintf(stderr, "At most 32 DHT nodes\n");
        return 1;
      }
    } else if (strcmp(argv[i], "--verbose") == 0) {
      verbose = true;
    } else {
      usage(argv[0]);
      return strcmp(argv[i], "--help") == 0 ? 0 : 1;
    }
  }

  Tox *tox = new_tox(port, 0, udp, savedata, savedata_length, &verbose);
  free(savedata);

  if (tox == NULL) {
    return 1;
  }

  if (print_id) {
    uint8_t key[TOX_PUBLIC_KEY_SIZE];
    tox_self_get_public_key(tox, key);
    print_hex("PUBLIC_KEY", key, sizeof(key));
    uint8_t name[TOX_MAX_NAME_LENGTH];
    tox_self_get_name(tox, name);
    printf("NAME %.*s\n", (int)tox_self_get_name_size(tox), (const char *)name);

    /* A loaded profile numbers its friends from 0. */
    for (uint32_t i = 0; i < tox_self_get_friend_list_size(tox); ++i) {
      tox_friend_get_public_key(tox, i, key, NULL);
      print_hex("FRIEND", key, sizeof(key));
    }

    const uint32_t group_count = tox_group_get_group_list_size(tox);
    uint32_t *groups = (uint32_t *)malloc((group_count > 0 ? group_count : 1) * sizeof(uint32_t));

    if (groups != NULL) {
      tox_group_get_group_list(tox, groups);

      for (uint32_t i = 0; i < group_count; ++i) {
        print_chat_id("GROUP", tox, groups[i]);
      }

      free(groups);
    }

    tox_kill(tox);
    return 0;
  }

  for (size_t i = 0; i < dht_node_count; ++i) {
    dht_nodes[i] = new_tox(0, i == 0 ? tcp_relay_port : 0, true, NULL, 0, &verbose);

    if (dht_nodes[i] == NULL) {
      return 1;
    }

    connect_toxes(tox, dht_nodes[i]);

    if (i > 0) {
      connect_toxes(dht_nodes[i - 1], dht_nodes[i]);
    }
  }

  if (bootstrap_port != 0) {
    tox_bootstrap(tox, bootstrap_host, bootstrap_port, bootstrap_key, NULL);
  }

  const char *name = "C peer";
  tox_self_set_name(tox, (const uint8_t *)name, strlen(name), NULL);

  for (size_t i = 0; i < friend_count; ++i) {
    Tox_Err_Friend_Add err_add;
    tox_friend_add_norequest(tox, friends[i], &err_add);

    if (err_add != TOX_ERR_FRIEND_ADD_OK) {
      fprintf(stderr, "Failed to add friend: %s\n", tox_err_friend_add_to_string(err_add));
      tox_kill(tox);
      return 1;
    }
  }

  const char *request_message = "Hello from C";

  for (size_t i = 0; i < request_count; ++i) {
    Tox_Err_Friend_Add err_add;
    tox_friend_add(tox, requests[i], (const uint8_t *)request_message, strlen(request_message),
                   &err_add);

    if (err_add != TOX_ERR_FRIEND_ADD_OK) {
      fprintf(stderr, "Failed to add friend: %s\n", tox_err_friend_add_to_string(err_add));
      tox_kill(tox);
      return 1;
    }
  }

  if (accept_requests) {
    tox_callback_friend_request(tox, friend_request_cb);
  }

  tox_callback_friend_connection_status(tox, friend_connection_status_cb);
  tox_callback_friend_name(tox, friend_name_cb);
  tox_callback_friend_message(tox, friend_message_cb);
  tox_callback_friend_lossless_packet(tox, friend_lossless_packet_cb);
  tox_callback_friend_lossy_packet(tox, friend_lossy_packet_cb);

  if (use_av) {
    Toxav_Err_New err_av;
    av = toxav_new(tox, &err_av);

    if (av == NULL) {
      fprintf(stderr, "Failed to start toxav: %d\n", err_av);
      tox_kill(tox);
      return 1;
    }

    toxav_callback_call(av, av_call_cb, NULL);
    toxav_callback_call_state(av, av_call_state_cb, NULL);
    toxav_callback_audio_receive_frame(av, av_audio_cb, NULL);
    toxav_callback_video_receive_frame(av, av_video_cb, NULL);
  }
  tox_callback_conference_invite(tox, conference_invite_cb);
  tox_callback_conference_title(tox, conference_title_cb);
  tox_callback_conference_message(tox, conference_message_cb);
  tox_callback_group_invite(tox, group_invite_cb);
  tox_callback_group_self_join(tox, group_self_join_cb);
  tox_callback_group_peer_join(tox, group_peer_join_cb);
  tox_callback_group_peer_exit(tox, group_peer_exit_cb);
  tox_callback_group_topic(tox, group_topic_cb);
  tox_callback_group_join_fail(tox, group_join_fail_cb);
  tox_callback_group_message(tox, group_message_cb);
  tox_callback_group_private_message(tox, group_private_message_cb);
  tox_callback_group_moderation(tox, group_moderation_cb);
  tox_callback_group_peer_limit(tox, group_peer_limit_cb);
  tox_callback_group_voice_state(tox, group_voice_state_cb);
  tox_callback_group_topic_lock(tox, group_topic_lock_cb);
  tox_callback_group_password(tox, group_password_cb);
  tox_callback_group_custom_packet(tox, group_custom_packet_cb);

  if (file_dir != NULL) {
    tox_callback_file_recv(tox, file_recv_cb);
    tox_callback_file_recv_control(tox, file_recv_control_cb);
    tox_callback_file_recv_chunk(tox, file_recv_chunk_cb);
    tox_callback_file_chunk_request(tox, file_chunk_request_cb);
  }

  const char *nick = "C peer";

  if (group_public_name != NULL) {
    Tox_Err_Group_New err_new;
    const uint32_t    group =
        tox_group_new(tox, TOX_GROUP_PRIVACY_STATE_PUBLIC, (const uint8_t *)group_public_name,
                      strlen(group_public_name), (const uint8_t *)nick, strlen(nick), &err_new);

    if (err_new != TOX_ERR_GROUP_NEW_OK) {
      printf("GROUP_NEW_FAILED %s\n", tox_err_group_new_to_string(err_new));
    } else {
      print_chat_id("GROUP_CHAT_ID", tox, group);
    }
  }

  if (group_join_id != NULL) {
    uint8_t chat_id[TOX_GROUP_CHAT_ID_SIZE];

    if (!hex_to_bin(group_join_id, chat_id, sizeof(chat_id))) {
      fprintf(stderr, "Invalid chat id: %s\n", group_join_id);
      tox_kill(tox);
      return 1;
    }

    Tox_Err_Group_Join err_join;
    const uint32_t     group =
        tox_group_join(tox, chat_id, (const uint8_t *)nick, strlen(nick), NULL, 0, &err_join);
    printf("GROUP_JOIN %s %u\n", tox_err_group_join_to_string(err_join), group);
  }

  uint8_t key[TOX_PUBLIC_KEY_SIZE];
  tox_self_get_dht_id(tox, key);
  print_hex("DHT_ID", key, sizeof(key));
  printf("UDP_PORT %u\n", tox_self_get_udp_port(tox, NULL));
  tox_self_get_public_key(tox, key);
  print_hex("PUBLIC_KEY", key, sizeof(key));
  for (size_t i = 0; i < dht_node_count; ++i) {
    tox_self_get_dht_id(dht_nodes[i], key);
    printf(ipv6 ? "DHT_NODE [::1]:%u:" : "DHT_NODE 127.0.0.1:%u:",
           tox_self_get_udp_port(dht_nodes[i], NULL));
    print_hex("", key, sizeof(key));
  }

  if (tcp_relay_port != 0 && dht_node_count > 0) {
    tox_self_get_dht_id(dht_nodes[0], key);
    printf(ipv6 ? "TCP_RELAY [::1]:%u:" : "TCP_RELAY 127.0.0.1:%u:", tcp_relay_port);
    print_hex("", key, sizeof(key));
  }

  uint8_t address[TOX_ADDRESS_SIZE];
  tox_self_get_address(tox, address);
  print_hex("TOX_ID", address, sizeof(address));

  bool relay_added = relay_port == 0;

  while (true) {
    /* toxcore drops a relay it cannot reach, so wait for it to listen. */
    if (!relay_added && tcp_port_open(relay_host, relay_port)) {
      Tox_Err_Bootstrap err;
      tox_add_tcp_relay(tox, relay_host, relay_port, relay_key, &err);
      printf("TCP_RELAY_ADDED %s\n", tox_err_bootstrap_to_string(err));
      relay_added = true;
    }

    for (size_t i = 0; i < sizeof(files) / sizeof(files[0]); ++i) {
      if (files[i].used && files[i].outgoing) {
        send_unsent_chunks(tox, &files[i]);
      }
    }

    tox_iterate(tox, NULL);
    uint32_t interval = tox_iteration_interval(tox);

    if (av != NULL) {
      toxav_iterate(av);
      send_echo_frames();
      send_conference_silence(tox);

      if (toxav_iteration_interval(av) < interval) {
        interval = toxav_iteration_interval(av);
      }
    }

    if (save_file != NULL && time(NULL) != last_save) {
      write_save(tox, save_file);
      last_save = time(NULL);
    }

    for (size_t i = 0; i < dht_node_count; ++i) {
      tox_iterate(dht_nodes[i], NULL);

      if (tox_iteration_interval(dht_nodes[i]) < interval) {
        interval = tox_iteration_interval(dht_nodes[i]);
      }
    }

    struct timespec pause;
    pause.tv_sec  = 0;
    pause.tv_nsec = (long)interval * 1000 * 1000;
    nanosleep(&pause, NULL);
  }
}

#!/usr/bin/env bash
# Run the c-toxcore peer with a local network of C DHT nodes and the Haskell
# node on localhost.  The two find each other through the onion, and a message
# from the Haskell node must come back from the C peer's echo.
#
# MODE is how they become friends: "direct" adds each other without a friend
# request, "hs-request" and "c-request" have the Haskell or the C side send a
# friend request that the other accepts.  "tcp" is "direct" with the Haskell
# node sending nothing over UDP, so it connects through a C TCP relay.
# "hs-relay" is the other way around: the C peer reaches the network only
# through a TCP relay run by the Haskell node.  "restart" is "direct", then
# the Haskell node stops and comes back with only its save file.  "file" is
# "direct" with the Haskell node sending a file that the C peer sends back,
# and "file-tcp" the same through a C TCP relay.  "ipv6" is "direct" over
# IPv6, where the C peer also joins the Haskell node's relay.  Group modes
# send custom packets as well.  "conference" is "direct" with the Haskell
# node inviting the C peer to a conference, whose messages it echoes.
# "conference3" adds a second C peer to it. "c-conference" has the C peer
# invite the Haskell node to its conference, and "c-av-conference" to its
# audio conference, which the Haskell node joins for its text while the C
# peer sends audio. "conference-reload" is "conference", then the Haskell
# node comes back from its key, its friend and the conference's own file.  "group" has the C peer invite
# the Haskell node to a private NGC group, "hs-group" the other way around.
# "group3" has the Haskell node invite two C peers, which must meet.
# "group3-c" has a C peer invite the Haskell node and another C peer.
# "public-group" has the Haskell node join a public group the C peer founded,
# found only by its chat id through the onion. "hs-public-group" has a second
# C peer join one the Haskell node founded. "group-restart" is "group", then
# the Haskell node comes back with only its save file and rejoins.
# "group-topic" is "group" with the Haskell node trying to set the topic,
# which a C founder's group locks to moderators, and "hs-group-topic" is
# "hs-group" with the Haskell founder setting it for the C peer to see.
# "group-private" is "group" with the Haskell node saying something privately
# to the C peer, which answers privately. "group-leave" is "group" with the Haskell node leaving after the echo,
# which the C peer must hear, and "group-leave-tcp" the same over TCP only.
# "group-reload" is "group-restart" with the Haskell node keeping the group in
# a file of its own and starting again from its key, its friend and that file,
# as clients with their own storage do.
# "group-tcp" and "hs-group-tcp" are "group" and "hs-group" with the Haskell
# node sending nothing over UDP, and so are "public-group-tcp" and
# "hs-public-group-tcp". "group-mod" has the C founder change every group
# setting and the Haskell node's role, then kick it, "hs-group-mod" the other
# way around. "custom" is "direct" with the Haskell node sending custom lossless
# and lossy packets that the C peer echoes.
set -eu

C_PEER="$1"
HS_NODE="$2"
MODE="$3"
OWN_WORK=
if [ -n "${TEST_TMPDIR:-}" ]; then
  WORK="$TEST_TMPDIR"
else
  WORK=$(mktemp -d)
  OWN_WORK=1
fi
SEED=42
MESSAGE="hello from haskell"

C_PID=
C2_PID=
HS_PID=
cleanup() {
  kill $C_PID $C2_PID $HS_PID 2>/dev/null || true
  # They may still be writing saves there.
  wait 2>/dev/null || true
  if [ -n "$OWN_WORK" ]; then
    rm -r "$WORK"
  fi
}
trap cleanup EXIT

# wait_for FILE PATTERN SECONDS
wait_for() {
  for _ in $(seq $(($3 * 10))); do
    if grep -q "$2" "$1"; then
      return 0
    fi
    sleep 0.1
  done
  return 1
}

# wait_count FILE PATTERN COUNT SECONDS
wait_count() {
  for _ in $(seq $(($4 * 10))); do
    if [ "$(grep -c "$2" "$1")" -ge "$3" ]; then
      return 0
    fi
    sleep 0.1
  done
  return 1
}

fail() {
  echo "FAIL: $1"
  for log in "$WORK"/*.log; do
    echo "=== $log"
    tail -n 50 "$log"
  done
  exit 1
}

"$HS_NODE" --no-local-discovery --seed "$SEED" --print-id >"$WORK/hs-id.log"
HS_PK=$(sed -n 's/^PUBLIC_KEY //p' "$WORK/hs-id.log")
HS_ID=$(sed -n 's/^TOX_ID //p' "$WORK/hs-id.log")
HS_DHT=$(sed -n 's/^DHT_ID //p' "$WORK/hs-id.log")

# Whether nothing listens on the TCP port.
port_free() {
  ! (exec 3<>"/dev/tcp/127.0.0.1/$1") 2>/dev/null
}

RELAY_PORT=$((20000 + RANDOM % 40000))
until port_free "$RELAY_PORT" && port_free $((RELAY_PORT + 1)); do
  RELAY_PORT=$((20000 + RANDOM % 40000))
done
HS_RELAY_PORT=$((RELAY_PORT + 1))

case "$MODE" in
  direct | tcp | restart | conference | conference-reload | conference3 | custom) C_FRIEND=(--friend "$HS_PK") ;;
  c-conference) C_FRIEND=(--friend "$HS_PK" --conference-new "C conference") ;;
  c-av-conference) C_FRIEND=(--friend "$HS_PK" --conference-new "C audio conference" --av) ;;
  ipv6) C_FRIEND=(--friend "$HS_PK" --ipv6 --tcp-relay "[::1]:$HS_RELAY_PORT:$HS_DHT") ;;
  group | group-restart | group-reload | group-leave | group-topic | group-private | group-leave-tcp | group-tcp) C_FRIEND=(--friend "$HS_PK" --group-new "C group") ;;
  group-mod) C_FRIEND=(--friend "$HS_PK" --group-new "C group" --group-moderate) ;;
  hs-group-mod) C_FRIEND=(--friend "$HS_PK") ;;
  group3-c) C_FRIEND=(--friend "$HS_PK" --group-new "C group" --accept-requests) ;;
  hs-group | hs-group-topic | group3 | hs-group-tcp) C_FRIEND=(--friend "$HS_PK") ;;
  hs-public-group | hs-public-group-tcp) C_FRIEND=() ;;
  public-group | public-group-tcp) C_FRIEND=(--group-public "C public group") ;;
  file | file-tcp)
    mkdir -p "$WORK/c-files"
    C_FRIEND=(--friend "$HS_PK" --file-dir "$WORK/c-files")
    ;;
  hs-relay) C_FRIEND=(--friend "$HS_PK" --no-udp --tcp-relay "127.0.0.1:$HS_RELAY_PORT:$HS_DHT") ;;
  hs-request) C_FRIEND=(--accept-requests) ;;
  c-request) C_FRIEND=(--add "$HS_ID") ;;
  *) fail "unknown mode $MODE" ;;
esac

"$C_PEER" --dht-nodes 8 --tcp-relay-port "$RELAY_PORT" "${C_FRIEND[@]}" >"$WORK/c-peer.log" 2>"$WORK/c-peer-stderr.log" &
C_PID=$!
wait_for "$WORK/c-peer.log" '^TOX_ID ' 10 || fail "C peer did not start"

C_DHT=$(sed -n 's/^DHT_ID //p' "$WORK/c-peer.log")
C_PORT=$(sed -n 's/^UDP_PORT //p' "$WORK/c-peer.log")
C_PK=$(sed -n 's/^PUBLIC_KEY //p' "$WORK/c-peer.log")
C_ID=$(sed -n 's/^TOX_ID //p' "$WORK/c-peer.log")
C_NODE="127.0.0.1:$C_PORT:$C_DHT"

if [ "$MODE" = file ] || [ "$MODE" = file-tcp ]; then
  head -c 300000 /dev/urandom >"$WORK/data.bin"
  mkdir -p "$WORK/hs-files"
fi

case "$MODE" in
  direct) HS_FRIEND=(--friend "$C_PK" --bootstrap "$C_NODE") ;;
  custom) HS_FRIEND=(--friend "$C_PK" --bootstrap "$C_NODE" --custom "hello custom") ;;
  restart) HS_FRIEND=(--friend "$C_PK" --bootstrap "$C_NODE" --save "$WORK/hs.tox" --conference "hello conference") ;;
  conference) HS_FRIEND=(--friend "$C_PK" --bootstrap "$C_NODE" --conference "hello conference") ;;
  c-conference | c-av-conference) HS_FRIEND=(--friend "$C_PK" --bootstrap "$C_NODE" --conference-join "hello conference") ;;
  conference-reload)
    HS_FRIEND=(--friend "$C_PK" --bootstrap "$C_NODE" --save "$WORK/hs.tox" --chats "$WORK/chats" --conference "hello conference") ;;
  group) HS_FRIEND=(--friend "$C_PK" --bootstrap "$C_NODE" --group "hello group") ;;
  group-private) HS_FRIEND=(--friend "$C_PK" --bootstrap "$C_NODE" --group "hello group" --group-private "psst") ;;
  group-topic) HS_FRIEND=(--friend "$C_PK" --bootstrap "$C_NODE" --group "hello group" --group-topic "Haskell topic") ;;
  group-restart) HS_FRIEND=(--friend "$C_PK" --bootstrap "$C_NODE" --save "$WORK/hs.tox" --group "hello group") ;;
  group-leave) HS_FRIEND=(--friend "$C_PK" --bootstrap "$C_NODE" --group "hello group" --group-leave) ;;
  group-reload) HS_FRIEND=(--friend "$C_PK" --bootstrap "$C_NODE" --save "$WORK/hs.tox" --chats "$WORK/chats" --group "hello group") ;;
  group3-c)
    "$C_PEER" --add "$C_ID" --bootstrap "$C_NODE" >"$WORK/c-peer-2.log" 2>"$WORK/c-peer-2-stderr.log" &
    C2_PID=$!
    wait_for "$WORK/c-peer-2.log" "^GROUP_SELF_JOIN " 20 || fail "second C peer did not join the group"
    HS_FRIEND=(--friend "$C_PK" --bootstrap "$C_NODE" --group "hello group")
    ;;
  public-group)
    wait_for "$WORK/c-peer.log" '^GROUP_CHAT_ID ' 10 || fail "C peer did not found the group"
    HS_FRIEND=(--bootstrap "$C_NODE" --group "hello group"
      --group-join "$(sed -n 's/^GROUP_CHAT_ID //p' "$WORK/c-peer.log")")
    ;;
  hs-public-group) HS_FRIEND=(--bootstrap "$C_NODE" --group "hello group" --group-public "Haskell public group") ;;
  group-mod) HS_FRIEND=(--friend "$C_PK" --bootstrap "$C_NODE" --group "hello group") ;;
  hs-group-mod) HS_FRIEND=(--friend "$C_PK" --bootstrap "$C_NODE" --group "hello group" --group-new "Haskell group" --group-moderate) ;;
  hs-group) HS_FRIEND=(--friend "$C_PK" --bootstrap "$C_NODE" --group "hello group" --group-new "Haskell group") ;;
  hs-group-topic) HS_FRIEND=(--friend "$C_PK" --bootstrap "$C_NODE" --group "hello group" --group-new "Haskell group" --group-topic "Haskell topic") ;;
  group3)
    "$C_PEER" --friend "$HS_PK" --bootstrap "$C_NODE" >"$WORK/c-peer-2.log" 2>"$WORK/c-peer-2-stderr.log" &
    C2_PID=$!
    wait_for "$WORK/c-peer-2.log" '^TOX_ID ' 10 || fail "second C peer did not start"
    C2_PK=$(sed -n 's/^PUBLIC_KEY //p' "$WORK/c-peer-2.log")
    HS_FRIEND=(--friend "$C_PK" --friend "$C2_PK" --bootstrap "$C_NODE" --group "hello group" --group-new "Haskell group")
    ;;
  conference3)
    "$C_PEER" --friend "$HS_PK" --bootstrap "$C_NODE" >"$WORK/c-peer-2.log" 2>"$WORK/c-peer-2-stderr.log" &
    C2_PID=$!
    wait_for "$WORK/c-peer-2.log" '^TOX_ID ' 10 || fail "second C peer did not start"
    C2_PK=$(sed -n 's/^PUBLIC_KEY //p' "$WORK/c-peer-2.log")
    HS_FRIEND=(--friend "$C_PK" --friend "$C2_PK" --bootstrap "$C_NODE" --conference "hello conference")
    ;;
  file)
    HS_FRIEND=(--friend "$C_PK" --bootstrap "$C_NODE" --send-file "$WORK/data.bin" --receive-dir "$WORK/hs-files")
    ;;
  hs-request) HS_FRIEND=(--friend "$C_ID" --bootstrap "$C_NODE") ;;
  c-request) HS_FRIEND=(--accept-requests --bootstrap "$C_NODE") ;;
  tcp | file-tcp | group-tcp | group-leave-tcp | hs-group-tcp | public-group-tcp | hs-public-group-tcp)
    HS_FRIEND=(--no-udp --tcp-relay "$(sed -n 's/^TCP_RELAY //p' "$WORK/c-peer.log")")
    case "$MODE" in
      group-tcp) HS_FRIEND+=(--friend "$C_PK" --group "hello group") ;;
      group-leave-tcp) HS_FRIEND+=(--friend "$C_PK" --group "hello group" --group-leave) ;;
      hs-group-tcp) HS_FRIEND+=(--friend "$C_PK" --group "hello group" --group-new "Haskell group") ;;
      public-group-tcp)
        wait_for "$WORK/c-peer.log" '^GROUP_CHAT_ID ' 10 || fail "C peer did not found the group"
        HS_FRIEND+=(--group "hello group" --group-join "$(sed -n 's/^GROUP_CHAT_ID //p' "$WORK/c-peer.log")")
        ;;
      hs-public-group-tcp) HS_FRIEND+=(--group "hello group" --group-public "Haskell public group") ;;
      file-tcp) HS_FRIEND+=(--friend "$C_PK" --send-file "$WORK/data.bin" --receive-dir "$WORK/hs-files") ;;
      *) HS_FRIEND+=(--friend "$C_PK") ;;
    esac
    while read -r node; do
      HS_FRIEND+=(--bootstrap "$node")
    done < <(sed -n 's/^DHT_NODE //p' "$WORK/c-peer.log")
    ;;
  ipv6)
    HS_FRIEND=(--friend "$C_PK" --tcp-port "$HS_RELAY_PORT")
    while read -r node; do
      HS_FRIEND+=(--bootstrap "$node")
    done < <(sed -n 's/^DHT_NODE //p' "$WORK/c-peer.log")
    ;;
  hs-relay)
    HS_FRIEND=(--friend "$C_PK" --tcp-port "$HS_RELAY_PORT")
    while read -r node; do
      HS_FRIEND+=(--bootstrap "$node")
    done < <(sed -n 's/^DHT_NODE //p' "$WORK/c-peer.log")
    ;;
esac

"$HS_NODE" --no-local-discovery --seed "$SEED" "${HS_FRIEND[@]}" --message "$MESSAGE" >"$WORK/hs-node.log" 2>&1 &
HS_PID=$!

case "$MODE" in
  hs-request)
    wait_for "$WORK/c-peer.log" "^FRIEND_REQUEST $HS_PK\$" 20 || fail "C peer did not receive the friend request"
    grep -q '^FRIEND_REQUEST_MESSAGE Hello from Haskell$' "$WORK/c-peer.log" || fail "wrong friend request message"
    ;;
  c-request)
    wait_for "$WORK/hs-node.log" "^FRIEND_REQUEST $C_PK Hello from C\$" 20 || fail "Haskell node did not receive the friend request"
    ;;
esac

if [ "$MODE" != public-group ] && [ "$MODE" != hs-public-group ] && [ "$MODE" != public-group-tcp ] && [ "$MODE" != hs-public-group-tcp ]; then
  wait_for "$WORK/hs-node.log" "^FRIEND_ONLINE $C_PK\$" 25 || fail "Haskell node did not see the C peer online"
  if [ "$MODE" = tcp ] || [ "$MODE" = file-tcp ] || [ "$MODE" = hs-relay ] || [ "$MODE" = group-tcp ] || [ "$MODE" = hs-group-tcp ]; then
    wait_for "$WORK/c-peer.log" "^FRIEND_CONNECTION 0 TOX_CONNECTION_TCP\$" 5 || fail "C peer is not connected over TCP"
  fi
  wait_for "$WORK/c-peer.log" "^MESSAGE 0 $MESSAGE\$" 10 || fail "C peer did not receive the message"
  wait_for "$WORK/hs-node.log" "^MESSAGE $C_PK $MESSAGE\$" 10 || fail "Haskell node did not receive the echo"
  wait_for "$WORK/hs-node.log" "^RECEIPT $C_PK 1\$" 10 || fail "Haskell node got no read receipt"
  wait_for "$WORK/hs-node.log" "^FRIEND_NAME $C_PK C peer\$" 10 || fail "Haskell node did not receive the C peer's name"
  wait_for "$WORK/c-peer.log" "^FRIEND_NAME 0 Haskell peer\$" 10 || fail "C peer did not receive the Haskell node's name"
fi

if [ "$MODE" = custom ]; then
  wait_for "$WORK/c-peer.log" "^LOSSLESS_PACKET 0 hello custom\$" 10 || fail "C peer did not get the lossless packet"
  wait_for "$WORK/hs-node.log" "^LOSSLESS_PACKET $C_PK echo: hello custom\$" 10 || fail "Haskell node did not get the lossless echo"
  wait_for "$WORK/c-peer.log" "^LOSSY_PACKET 0 hello custom\$" 10 || fail "C peer did not get a lossy packet"
  wait_for "$WORK/hs-node.log" "^LOSSY_PACKET $C_PK echo: hello custom\$" 10 || fail "Haskell node did not get a lossy echo"
fi

if [ "$MODE" = c-conference ] || [ "$MODE" = c-av-conference ]; then
  wait_for "$WORK/hs-node.log" "^CONFERENCE_JOIN Right 0\$" 20 || fail "Haskell node did not join the conference"
  wait_for "$WORK/c-peer.log" "^CONFERENCE_MESSAGE 0 hello conference\$" 20 || fail "C peer did not get the conference message"
  wait_for "$WORK/hs-node.log" "^CONFERENCE_MESSAGE 0 echo: hello conference\$" 10 || fail "Haskell node did not get the echo"
  ! grep -q "^CONFERENCE_MESSAGE 0 hello conference\$" "$WORK/hs-node.log" || fail "Haskell node heard its own message"
fi

if [ "$MODE" = conference ] || [ "$MODE" = conference-reload ]; then
  wait_for "$WORK/c-peer.log" "^CONFERENCE_JOINED " 10 || fail "C peer did not join the conference"
  wait_for "$WORK/c-peer.log" "^CONFERENCE_TITLE 0 Haskell conference\$" 10 || fail "C peer did not get the title"
  wait_for "$WORK/c-peer.log" "^CONFERENCE_MESSAGE 0 hello conference\$" 10 || fail "C peer did not get the conference message"
  wait_for "$WORK/hs-node.log" "^CONFERENCE_MESSAGE 0 echo: hello conference\$" 10 || fail "Haskell node did not get the echo"
fi

if [ "$MODE" = conference3 ]; then
  wait_for "$WORK/c-peer-2.log" "^CONFERENCE_JOINED " 20 || fail "second C peer did not join the conference"
  wait_for "$WORK/c-peer.log" "^CONFERENCE_MESSAGE 0 hello conference\$" 20 || fail "C peer did not get the conference message"
  wait_for "$WORK/c-peer-2.log" "^CONFERENCE_MESSAGE 0 hello conference\$" 10 || fail "second C peer did not get the conference message"
  wait_for "$WORK/c-peer.log" "^CONFERENCE_MESSAGE 0 echo: hello conference\$" 10 || fail "C peer did not get the other C peer's echo"
  wait_for "$WORK/c-peer-2.log" "^CONFERENCE_MESSAGE 0 echo: hello conference\$" 10 || fail "second C peer did not get the other C peer's echo"
  wait_for "$WORK/hs-node.log" "^CONFERENCE_MESSAGE 0 echo: hello conference\$" 10 || fail "Haskell node did not get an echo"
  wait_count "$WORK/hs-node.log" '^CONFERENCE_MESSAGE 0 echo: hello conference$' 2 5 || fail "Haskell node did not get both echoes"
fi

if [ "$MODE" = group ] || [ "$MODE" = hs-group ] || [ "$MODE" = group-restart ] || [ "$MODE" = group-reload ] || [ "$MODE" = group-leave ] || [ "$MODE" = group-leave-tcp ] || [ "$MODE" = group-tcp ] || [ "$MODE" = hs-group-tcp ] ||
   [ "$MODE" = group-topic ] || [ "$MODE" = hs-group-topic ] || [ "$MODE" = group-private ]; then
  if [ "$MODE" != hs-group ] && [ "$MODE" != hs-group-tcp ] && [ "$MODE" != hs-group-topic ]; then
    wait_for "$WORK/hs-node.log" "^GROUP_SELF_JOIN " 20 || fail "Haskell node did not join the group"
    wait_for "$WORK/hs-node.log" "^GROUP_PEER_JOIN 0 C peer\$" 10 || fail "Haskell node does not see the C peer in the group"
  else
    wait_for "$WORK/c-peer.log" "^GROUP_SELF_JOIN " 20 || fail "C peer did not join the group"
  fi
  wait_for "$WORK/c-peer.log" "^GROUP_PEER_JOIN 0 Haskell peer\$" 10 || fail "C peer does not see the Haskell node in the group"
  wait_for "$WORK/c-peer.log" "^GROUP_MESSAGE 0 hello group\$" 10 || fail "C peer did not get the group message"
  wait_for "$WORK/hs-node.log" "^GROUP_MESSAGE 0 echo: hello group\$" 10 || fail "Haskell node did not get the group echo"
  wait_for "$WORK/c-peer.log" "^GROUP_CUSTOM 0 hello group\$" 10 || fail "C peer did not get the custom packet"
  wait_for "$WORK/hs-node.log" "^GROUP_CUSTOM 0 echo: hello group\$" 10 || fail "Haskell node did not get the custom packet echo"
fi

if [ "$MODE" = public-group ] || [ "$MODE" = public-group-tcp ]; then
  wait_for "$WORK/hs-node.log" "^GROUP_SELF_JOIN " 60 || fail "Haskell node did not join the group"
  wait_for "$WORK/c-peer.log" "^GROUP_PEER_JOIN 0 Haskell peer\$" 10 || fail "C peer does not see the Haskell node in the group"
  wait_for "$WORK/c-peer.log" "^GROUP_MESSAGE 0 hello group\$" 10 || fail "C peer did not get the group message"
  wait_for "$WORK/hs-node.log" "^GROUP_MESSAGE 0 echo: hello group\$" 10 || fail "Haskell node did not get the group echo"
fi

if [ "$MODE" = hs-public-group ] || [ "$MODE" = hs-public-group-tcp ]; then
  wait_for "$WORK/hs-node.log" '^GROUP_CHAT_ID ' 10 || fail "Haskell node did not found the group"
  "$C_PEER" --bootstrap "$C_NODE" --group-join "$(sed -n 's/^GROUP_CHAT_ID //p' "$WORK/hs-node.log")" \
    >"$WORK/c-peer-2.log" 2>"$WORK/c-peer-2-stderr.log" &
  C2_PID=$!
  wait_for "$WORK/c-peer-2.log" "^GROUP_SELF_JOIN " 60 || fail "second C peer did not join the group"
  wait_for "$WORK/hs-node.log" "^GROUP_PEER_JOIN 0 C peer\$" 10 || fail "Haskell node does not see the C peer in the group"
  wait_for "$WORK/c-peer-2.log" "^GROUP_MESSAGE 0 hello group\$" 10 || fail "second C peer did not get the group message"
  wait_for "$WORK/hs-node.log" "^GROUP_MESSAGE 0 echo: hello group\$" 10 || fail "Haskell node did not get the group echo"
fi

if [ "$MODE" = group-mod ] || [ "$MODE" = hs-group-mod ]; then
  if [ "$MODE" = group-mod ]; then TARGET="$WORK/hs-node.log"; else TARGET="$WORK/c-peer.log"; fi
  wait_for "$TARGET" "^GROUP_MODERATION 0 KICK\$" 30 || fail "the peer was not kicked"
  for line in "GROUP_PEER_LIMIT 0 50" "GROUP_VOICE 0 MODERATOR" "GROUP_TOPIC_LOCK 0 DISABLED" "GROUP_PASSWORD 0 secret"; do
    grep -q "^$line\$" "$TARGET" || fail "the peer did not see $line"
  done
  [ "$(sed -n 's/^GROUP_MODERATION 0 //p' "$TARGET" | tr '\n' ' ')" = "MODERATOR USER OBSERVER USER KICK " ] ||
    fail "the peer saw the wrong role changes"
fi

if [ "$MODE" = group3 ]; then
  for log in c-peer.log c-peer-2.log; do
    wait_for "$WORK/$log" "^GROUP_SELF_JOIN " 20 || fail "a C peer did not join the group"
  done
  for log in c-peer.log c-peer-2.log; do
    wait_for "$WORK/$log" "^GROUP_PEER_JOIN 0 Haskell peer\$" 10 || fail "a C peer does not see the Haskell node"
    wait_for "$WORK/$log" "^GROUP_PEER_JOIN 0 C peer\$" 30 || fail "the C peers did not meet"
    wait_for "$WORK/$log" "^GROUP_MESSAGE 0 hello group\$" 10 || fail "a C peer did not get the group message"
  done
  wait_for "$WORK/hs-node.log" "^GROUP_MESSAGE 0 echo: hello group\$" 10 || fail "Haskell node did not get an echo"
fi

if [ "$MODE" = group3-c ]; then
  wait_for "$WORK/hs-node.log" "^GROUP_SELF_JOIN " 20 || fail "Haskell node did not join the group"
  wait_for "$WORK/c-peer-2.log" "^GROUP_PEER_JOIN 0 Haskell peer\$" 30 || fail "the second C peer did not meet the Haskell node"
  wait_count "$WORK/hs-node.log" '^GROUP_PEER_JOIN 0 C peer$' 2 10 || fail "Haskell node does not see both C peers"
  wait_for "$WORK/c-peer-2.log" "^GROUP_MESSAGE 0 hello group\$" 10 || fail "the second C peer did not get the group message"
fi

if [ "$MODE" = ipv6 ]; then
  # Either side may start the session.
  grep -Eq '^\[Info\] (Connecting to|Accepting session from) .* address = SocketAddress "::1"' "$WORK/hs-node.log" ||
    fail "Haskell node did not connect over IPv6"
  grep -q "^FRIEND_CONNECTION 0 TOX_CONNECTION_UDP\$" "$WORK/c-peer.log" || fail "C peer is not connected over UDP"
  wait_for "$WORK/c-peer.log" "^TCP_RELAY_ADDED TOX_ERR_BOOTSTRAP_OK\$" 10 || fail "C peer did not add the Haskell relay over IPv6"
fi

if [ "$MODE" = file ] || [ "$MODE" = file-tcp ]; then
  wait_for "$WORK/c-peer.log" "^FILE_RECEIVED data.bin 300000\$" 30 || fail "C peer did not receive the file"
  cmp "$WORK/data.bin" "$WORK/c-files/data.bin" || fail "C peer received a different file"
  wait_for "$WORK/hs-node.log" "^FILE_SENT data.bin\$" 10 || fail "Haskell node did not see the file arrive"
  wait_for "$WORK/hs-node.log" "^FILE_RECEIVED echo-data.bin 300000\$" 30 || fail "Haskell node did not receive the file back"
  cmp "$WORK/data.bin" "$WORK/hs-files/echo-data.bin" || fail "Haskell node received a different file"
fi

if [ "$MODE" = restart ]; then
  wait_for "$WORK/c-peer.log" "^CONFERENCE_MESSAGE 0 hello conference\$" 20 || fail "C peer did not get the conference message"
  kill "$HS_PID"
  wait "$HS_PID" || true
  wait_for "$WORK/c-peer.log" "^FRIEND_CONNECTION 0 TOX_CONNECTION_NONE\$" 10 || fail "C peer did not see the Haskell node leave"
  "$C_PEER" --load "$WORK/hs.tox" --print-id >"$WORK/c-load.log" 2>&1 || fail "C cannot load the Haskell save file"
  grep -q "^PUBLIC_KEY $HS_PK\$" "$WORK/c-load.log" || fail "C loads a different key from the save file"
  grep -q "^NAME Haskell peer\$" "$WORK/c-load.log" || fail "C loads a different name from the save file"
  grep -q "^FRIEND $C_PK\$" "$WORK/c-load.log" || fail "C does not load the friend from the save file"
  "$HS_NODE" --no-local-discovery --save "$WORK/hs.tox" --bootstrap "$C_NODE" --message "$MESSAGE again" --conference "hello again" >"$WORK/hs-node-2.log" 2>&1 &
  HS_PID=$!
  wait_for "$WORK/hs-node-2.log" "^PUBLIC_KEY $HS_PK\$" 10 || fail "Haskell node has a new key after restarting"
  wait_for "$WORK/hs-node-2.log" "^FRIEND_ONLINE $C_PK\$" 25 || fail "Haskell node did not see the C peer online after restarting"
  wait_for "$WORK/c-peer.log" "^MESSAGE 0 $MESSAGE again\$" 10 || fail "C peer did not receive the message after the restart"
  wait_for "$WORK/c-peer.log" "^CONFERENCE_MESSAGE 0 hello again\$" 20 || fail "Haskell node did not get back into the conference"
  ! grep -q "^CONFERENCE_JOINED 1" "$WORK/c-peer.log" || fail "Haskell node made a new conference after restarting"
fi

if [ "$MODE" = group-restart ]; then
  CHAT_ID=$(sed -n 's/^GROUP_CHAT_ID //p' "$WORK/c-peer.log")
  kill "$HS_PID"
  wait "$HS_PID" || true
  "$C_PEER" --load "$WORK/hs.tox" --print-id >"$WORK/c-load.log" 2>&1 || fail "C cannot load the Haskell save file"
  grep -q "^GROUP $CHAT_ID\$" "$WORK/c-load.log" || fail "C does not load the group from the save file"
  "$HS_NODE" --no-local-discovery --save "$WORK/hs.tox" --bootstrap "$C_NODE" --group "hello again" >"$WORK/hs-node-2.log" 2>&1 &
  HS_PID=$!
  wait_for "$WORK/hs-node-2.log" "^GROUP $CHAT_ID\$" 10 || fail "Haskell node did not load the group"
  wait_for "$WORK/hs-node-2.log" "^GROUP_PEER_JOIN 0 C peer\$" 40 || fail "Haskell node did not get back into the group"
  wait_for "$WORK/hs-node-2.log" "^GROUP_SELF_JOIN 0\$" 30 || fail "Haskell node did not sync the group after restarting"
  wait_for "$WORK/c-peer.log" "^GROUP_MESSAGE 0 hello again\$" 10 || fail "C peer did not get the message after the restart"
  wait_for "$WORK/hs-node-2.log" "^GROUP_MESSAGE 0 echo: hello again\$" 10 || fail "Haskell node did not get the echo after the restart"
  ! grep -q "^GROUP_SELF_JOIN 1" "$WORK/hs-node-2.log" || fail "Haskell node joined a second group after restarting"
fi

if [ "$MODE" = group-private ]; then
  wait_for "$WORK/c-peer.log" "^GROUP_PRIVATE_MESSAGE 0 psst\$" 10 || fail "C peer did not get the private message"
  wait_for "$WORK/hs-node.log" "^GROUP_PRIVATE_MESSAGE 0 echo: psst\$" 10 || fail "Haskell node did not get the private echo"
  ! grep -q "^GROUP_MESSAGE 0 psst\$" "$WORK/c-peer.log" || fail "the private message went to the group"
fi

if [ "$MODE" = group-topic ]; then
  wait_for "$WORK/hs-node.log" "^GROUP_TOPIC_SET False False\$" 10 || fail "Haskell node did not try the topic"
  ! grep -q "^GROUP_TOPIC_SET True" "$WORK/hs-node.log" || fail "Haskell node may set a locked topic"
  ! grep -q "^GROUP_TOPIC 0 Haskell topic\$" "$WORK/c-peer.log" || fail "C peer took a topic the lock refuses"
fi

if [ "$MODE" = hs-group-topic ]; then
  grep -q "^GROUP_TOPIC_SET True True\$" "$WORK/hs-node.log" || fail "Haskell founder could not set the topic"
  wait_for "$WORK/c-peer.log" "^GROUP_TOPIC 0 Haskell topic\$" 10 || fail "C peer did not get the topic"
fi

if [ "$MODE" = group-leave ] || [ "$MODE" = group-leave-tcp ]; then
  wait_for "$WORK/hs-node.log" "^GROUP_LEFT 0\$" 10 || fail "Haskell node did not leave the group"
  wait_for "$WORK/c-peer.log" "^GROUP_PEER_EXIT 0 Haskell peer TOX_GROUP_EXIT_TYPE_QUIT\$" 10 || fail "C peer did not see the Haskell node leave"
fi

if [ "$MODE" = conference-reload ]; then
  CONFERENCES=("$WORK"/chats/*.conference)
  [ -f "${CONFERENCES[0]}" ] || fail "Haskell node did not keep the conference in its own file"
  kill "$HS_PID"
  wait "$HS_PID" || true
  "$HS_NODE" --no-local-discovery --friend "$C_PK" --save "$WORK/hs.tox" --chats "$WORK/chats" --bootstrap "$C_NODE" --conference "hello again" >"$WORK/hs-node-2.log" 2>&1 &
  HS_PID=$!
  wait_for "$WORK/hs-node-2.log" "^CONFERENCE_RESTORED 0\$" 10 || fail "Haskell node did not restore the conference"
  wait_for "$WORK/c-peer.log" "^CONFERENCE_MESSAGE 0 hello again\$" 30 || fail "Haskell node did not get back into the conference"
  ! grep -q "^CONFERENCE_JOINED 1" "$WORK/c-peer.log" || fail "Haskell node made a new conference after restarting"
fi

if [ "$MODE" = group-reload ]; then
  CHAT_ID=$(sed -n 's/^GROUP_CHAT_ID //p' "$WORK/c-peer.log")
  [ -f "$WORK/chats/$CHAT_ID.group" ] || fail "Haskell node did not keep the group in its own file"
  kill "$HS_PID"
  wait "$HS_PID" || true
  "$HS_NODE" --no-local-discovery --friend "$C_PK" --save "$WORK/hs.tox" --chats "$WORK/chats" --bootstrap "$C_NODE" --group "hello again" >"$WORK/hs-node-2.log" 2>&1 &
  HS_PID=$!
  wait_for "$WORK/hs-node-2.log" "^PUBLIC_KEY $HS_PK\$" 10 || fail "Haskell node has a new key after restarting"
  wait_for "$WORK/hs-node-2.log" "^GROUP $CHAT_ID\$" 10 || fail "Haskell node did not restore the group"
  wait_for "$WORK/hs-node-2.log" "^GROUP_PEER_JOIN 0 C peer\$" 40 || fail "Haskell node did not get back into the group"
  wait_for "$WORK/hs-node-2.log" "^GROUP_SELF_JOIN 0\$" 30 || fail "Haskell node did not sync the group after restarting"
  wait_for "$WORK/c-peer.log" "^GROUP_MESSAGE 0 hello again\$" 10 || fail "C peer did not get the message after the restart"
  wait_for "$WORK/hs-node-2.log" "^GROUP_MESSAGE 0 echo: hello again\$" 10 || fail "Haskell node did not get the echo after the restart"
  ! grep -q "^GROUP_SELF_JOIN 1" "$WORK/hs-node-2.log" || fail "Haskell node joined a second group after restarting"
fi

echo "PASS"

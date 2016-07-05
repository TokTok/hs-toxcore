#include <stdarg.h>
#include <errno.h>
#include <fcntl.h>
#include <stdio.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netdb.h>
#include <string.h>
#include <unistd.h>

#include "errors.h"
#include "methods.h"
#include "util.h"


static int
protocol_error (msgpack_packer *pk, char const *fmt, ...)
{
  msgpack_pack_array (pk, 4); // 4 elements in the array
  msgpack_pack_uint8 (pk, 1); // 1. type = response
  // 2. We don't know the msgid, because the packet we received is not a valid
  // msgpack-rpc packet.
  msgpack_pack_uint64 (pk, 0);

  // 3. Error message.
  va_list ap;
  va_start (ap, fmt);
  int res = msgpack_pack_vstringf (pk, fmt, ap);
  va_end (ap);

  // 4. No success result.
  msgpack_pack_array (pk, 0);

  return res;
}


static bool
type_check (msgpack_packer *pk, msgpack_object req, int index, msgpack_object_type type)
{
  if (req.via.array.ptr[index].type != type)
    {
      protocol_error (pk, "element %d should be %s, but is %s",
          index,
          type_name (type),
          type_name (req.via.array.ptr[index].type));
      return false;
    }
  return true;
}


static int
write_sample_input (msgpack_object req)
{
  msgpack_object_str name = req.via.array.ptr[2].via.str;
  char filename[128] = "test-toxcore/test-inputs/";
  memcpy (filename + strlen (filename), name.ptr, name.size);
  memcpy (filename + strlen (filename) + name.size, ".mp", 4);

  int fd = open (filename, O_WRONLY | O_CREAT, S_IRUSR | S_IWUSR);
  if (fd < 0)
    // If we can't open the sample file, we just don't write it.
    return E_OK;

  check_return (E_WRITE, ftruncate (fd, 0));

  msgpack_sbuffer sbuf __attribute__ ((__cleanup__ (msgpack_sbuffer_destroy)));
  msgpack_sbuffer_init (&sbuf);

  msgpack_packer pk;
  msgpack_packer_init (&pk, &sbuf, msgpack_sbuffer_write);

  msgpack_pack_object (&pk, req);

  check_return (E_WRITE, write (fd, sbuf.data, sbuf.size));

  return E_OK;
}


static int
handle_request (bool collect_samples, int write_fd, msgpack_object req)
{
  msgpack_sbuffer sbuf __attribute__ ((__cleanup__ (msgpack_sbuffer_destroy))); /* buffer */
  msgpack_sbuffer_init (&sbuf); /* initialize buffer */

  msgpack_packer pk;    /* packer */
  msgpack_packer_init (&pk, &sbuf, msgpack_sbuffer_write); /* initialize packer */

  if (req.type != MSGPACK_OBJECT_ARRAY)
    protocol_error (&pk, "expected array, but got %s", type_name (req.type));
  else if (req.via.array.size != 4)
    protocol_error (&pk, "array length should be 4, but is %d", req.via.array.size);
  else if (
      type_check (&pk, req, 0, MSGPACK_OBJECT_POSITIVE_INTEGER) &&
      type_check (&pk, req, 1, MSGPACK_OBJECT_POSITIVE_INTEGER) &&
      type_check (&pk, req, 2, MSGPACK_OBJECT_STR) &&
      type_check (&pk, req, 3, MSGPACK_OBJECT_ARRAY))
    {
      printf ("Request: ");
      msgpack_object_print (stdout, req);
      printf ("\n");

      if (collect_samples)
        propagate (write_sample_input (req));

      msgpack_pack_array (&pk, 4); // 4 elements in the array
      msgpack_pack_uint8 (&pk, 1); // 1. type = response
      msgpack_pack_uint64 (&pk, req.via.array.ptr[1].via.u64); // 2. msgid

      // if error is null, this writes 3. no error, and 4. result
      char const *error = call_method (req.via.array.ptr[2].via.str, req.via.array.ptr[3].via.array, &pk);
      if (error)
        {
          msgpack_pack_string (&pk, error);
          msgpack_pack_array (&pk, 0);
        }
    }
  check_return (E_WRITE, write (write_fd, sbuf.data, sbuf.size));

  return E_OK;
}


static int
communicate (bool collect_samples, int read_fd, int write_fd)
{
  msgpack_unpacker unp __attribute__ ((__cleanup__ (msgpack_unpacker_destroy)));
  msgpack_unpacker_init (&unp, 128);

  while (1)
    {
      char buf[64];
      int size = check_return (E_READ, read (read_fd, buf, sizeof buf));
      if (size == 0)
        break;

      if (msgpack_unpacker_buffer_capacity (&unp) < size)
        if (!msgpack_unpacker_reserve_buffer (&unp, size))
          return E_NOMEM;

      memcpy (msgpack_unpacker_buffer (&unp), buf, size);
      msgpack_unpacker_buffer_consumed (&unp, size);

      msgpack_unpacked req __attribute__ ((__cleanup__ (msgpack_unpacked_destroy)));
      msgpack_unpacked_init (&req);
      switch (msgpack_unpacker_next (&unp, &req))
        {
          case MSGPACK_UNPACK_SUCCESS:
            propagate (handle_request (collect_samples, write_fd, req.data));
            break;
          case MSGPACK_UNPACK_EXTRA_BYTES:
            printf ("EXTRA_BYTES\n");
            break;
          case MSGPACK_UNPACK_CONTINUE: break;
          case MSGPACK_UNPACK_PARSE_ERROR: return E_PARSE;
          case MSGPACK_UNPACK_NOMEM_ERROR: return E_NOMEM;
        }
    }

  return E_OK;
}


static int
closep (int *fd)
{
  return close (*fd);
}


static int
run_tests (bool collect_samples, int port)
{
  int listen_fd __attribute__ ((__cleanup__ (closep)));
  listen_fd = check_return (E_SOCKET, socket (AF_INET, SOCK_STREAM, 0));

  struct sockaddr_in servaddr;
  servaddr.sin_family = AF_INET;
  servaddr.sin_addr.s_addr = htons (INADDR_ANY);
  servaddr.sin_port = htons (port);

  check_return (E_BIND, bind (listen_fd, (struct sockaddr *) &servaddr, sizeof servaddr));
  check_return (E_LISTEN, listen (listen_fd, 10));

  while (1)
    {
      int comm_fd __attribute__ ((__cleanup__ (closep)));
      comm_fd = check_return (E_ACCEPT, accept (listen_fd, NULL, NULL));
      propagate (communicate (collect_samples, comm_fd, comm_fd));
    }

  return E_OK;
}


uint32_t
test_main (bool collect_samples, uint16_t port)
{
  int result = run_tests (collect_samples, port);
  if (result == E_OK)
    return E_OK;
  return result | (errno << 8);
}


__attribute__ ((__weak__)) int main (int argc, char **argv);

int
main (int argc, char **argv)
{
  return communicate (false, STDIN_FILENO, STDOUT_FILENO);
}

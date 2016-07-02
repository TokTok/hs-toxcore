#include <errno.h>
#include <stdio.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netdb.h>
#include <string.h>
#include <unistd.h>

#include "errors.h"
#include "methods.h"

#define check_return(err, expr) __extension__ ({ \
    __typeof__ (expr) _r = (expr); \
    if (_r < 0) \
      return err; \
    _r; \
  })

#define propagate(expr) __extension__ ({ \
    __typeof__ (expr) _r = (expr); \
    if (_r != E_OK) \
      return _r; \
  })



static int
send_response (int comm_fd, msgpack_sbuffer const *res)
{
  check_return (E_WRITE, write (comm_fd, res->data, res->size));
  return E_OK;
}


static int
handle_request (int comm_fd, msgpack_object req)
{
  msgpack_sbuffer sbuf; /* buffer */
  msgpack_sbuffer_init (&sbuf); /* initialize buffer */

  msgpack_packer res;    /* packer */
  msgpack_packer_init (&res, &sbuf, msgpack_sbuffer_write); /* initialize packer */

  call_method (&req, &res);
  int r = send_response (comm_fd, &sbuf);

  msgpack_sbuffer_destroy (&sbuf);

  return r;
}


static int
run_tests (int port)
{
  int listen_fd = socket (AF_INET, SOCK_STREAM, 0);

  struct sockaddr_in servaddr;
  servaddr.sin_family = AF_INET;
  servaddr.sin_addr.s_addr = htons (INADDR_ANY);
  servaddr.sin_port = htons (port);

  check_return (E_BIND, bind (listen_fd, (struct sockaddr *) &servaddr, sizeof servaddr));

  check_return (E_LISTEN, listen (listen_fd, 10));

  while (1)
    {
      int comm_fd = check_return (E_ACCEPT, accept (listen_fd, NULL, NULL));

      msgpack_unpacker unp;
      msgpack_unpacker_init (&unp, 128);

      while (1)
        {
          char buf[64];
          int size = check_return (E_READ, read (comm_fd, buf, sizeof buf));
          if (size == 0)
            break;

          if (msgpack_unpacker_buffer_capacity (&unp) < size)
            {
              if (!msgpack_unpacker_reserve_buffer (&unp, size))
                return E_NOMEM;
            }

          memcpy (msgpack_unpacker_buffer (&unp), buf, size);
          msgpack_unpacker_buffer_consumed (&unp, size);

          msgpack_unpacked req;
          msgpack_unpacked_init (&req);
          switch (msgpack_unpacker_next (&unp, &req))
            {
              case MSGPACK_UNPACK_SUCCESS:
                propagate (handle_request (comm_fd, req.data));
                break;
              case MSGPACK_UNPACK_EXTRA_BYTES:
                printf ("EXTRA_BYTES\n");
                break;
              case MSGPACK_UNPACK_CONTINUE: break;
              case MSGPACK_UNPACK_PARSE_ERROR: return E_PARSE;
              case MSGPACK_UNPACK_NOMEM_ERROR: return E_NOMEM;
            }
          msgpack_unpacked_destroy (&req);
        }

      msgpack_unpacker_destroy (&unp);
    }

  return E_OK;
}


int
test_main (int port)
{
  int result = run_tests (port);
  if (result == E_OK)
    return E_OK;
  return result | (errno << 8);
}

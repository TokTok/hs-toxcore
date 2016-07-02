#include "methods.h"

#define MIN(a,b) ((a) < (b) ? a : b)


void
call_method (msgpack_zone *zone, msgpack_object const *req, msgpack_object *res)
{
  printf ("Request: ");
  msgpack_object_print (stdout, *req);
  printf ("\n");

  *res = *req;
  res->via.array.ptr[0].via.u64 = 1; // response

  msgpack_object_str name = req->via.array.ptr[2].via.str;
#define NAME_IS(NAME) (memcmp (name.ptr, NAME, MIN (name.size, sizeof NAME)) == 0)
  if (NAME_IS ("KeyPair.newNonce"))
    {
      printf ("Creating new nonce\n");
      // TODO: create nonce and put it into *res.
    }
}

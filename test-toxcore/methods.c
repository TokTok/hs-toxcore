#include "methods.h"

#define MIN(a,b) ((a) < (b) ? a : b)


void
call_method (msgpack_object const *req, msgpack_packer *res)
{
  printf ("Request: ");
  msgpack_object_print (stdout, *req);
  printf ("\n");

  uint64_t msgid = req->via.array.ptr[1].via.u64;

  msgpack_pack_array (res, 4);
  msgpack_pack_uint8 (res, 1); // type = response
  msgpack_pack_uint64 (res, msgid); // msgid
  msgpack_pack_array (res, 0); // no error

  msgpack_object_str name = req->via.array.ptr[2].via.str;
#define NAME_IS(NAME) (memcmp (name.ptr, NAME, MIN (name.size, sizeof NAME)) == 0)
  if (NAME_IS ("KeyPair.newNonce"))
    {
      printf ("Creating new nonce\n");
      uint8_t nonce[24] = { 0 };
      nonce[0] = msgid;
      msgpack_pack_bin (res, sizeof nonce);
      msgpack_pack_bin_body (res, nonce, sizeof nonce);
    }
  else
    {
      msgpack_pack_object (res, *req);
    }
}

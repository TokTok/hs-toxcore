#include "methods.h"

#include "crypto_core.h"


bool
call_method (msgpack_object_str name, msgpack_object_array args, msgpack_packer *res)
{
#define NAME_IS(NAME) name.size == sizeof NAME - 1 && memcmp (name.ptr, NAME, name.size) == 0
  if (NAME_IS ("Nonce.newNonce"))
    {
      uint8_t nonce[24] = { 0 };
      new_nonce (nonce);
      msgpack_pack_bin (res, sizeof nonce);
      msgpack_pack_bin_body (res, nonce, sizeof nonce);
    }
  else if (NAME_IS ("Nonce.increment"))
    {
      if (args.ptr[0].type != MSGPACK_OBJECT_BIN) return false;
      if (args.ptr[0].via.bin.size != 24        ) return false;

      uint8_t nonce[24];
      memcpy (nonce, args.ptr[0].via.bin.ptr, 24);
      increment_nonce (nonce);
      msgpack_pack_bin (res, sizeof nonce);
      msgpack_pack_bin_body (res, nonce, sizeof nonce);
    }
  else
    {
      // Default action: echo.
      msgpack_pack_array (res, args.size);
      for (size_t i = 0; i < args.size; i++)
        msgpack_pack_object (res, args.ptr[i]);
    }

  return true;
}

#include "methods.h"

#include "util.h"

#include <crypto_core.h>


#define success msgpack_pack_array (res, 0); if (true)


static void
pending (msgpack_packer *res)
{
  msgpack_pack_string (res, "Pending");
  msgpack_pack_array (res, 0);
}


bool
call_method (msgpack_object_str name, msgpack_object_array args, msgpack_packer *res)
{
#define NAME_IS(NAME) name.size == sizeof NAME - 1 && memcmp (name.ptr, NAME, name.size) == 0
  if (NAME_IS ("Nonce.newNonce"))
    {
      uint8_t nonce[24] = { 0 };
      new_nonce (nonce);

      success {
        msgpack_pack_bin (res, sizeof nonce);
        msgpack_pack_bin_body (res, nonce, sizeof nonce);
      }
    }
  else if (NAME_IS ("Nonce.increment"))
    {
      if (args.ptr[0].type != MSGPACK_OBJECT_BIN) return false;
      if (args.ptr[0].via.bin.size != 24        ) return false;

      uint8_t nonce[24];
      memcpy (nonce, args.ptr[0].via.bin.ptr, 24);
      increment_nonce (nonce);

      success {
        msgpack_pack_bin (res, sizeof nonce);
        msgpack_pack_bin_body (res, nonce, sizeof nonce);
      }
    }
  else
    // Default action: "Pending" exception.
    pending (res);

  return true;
}

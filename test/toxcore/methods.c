#include "methods.h"

#include "util.h"

#include <crypto_core.h>


char const *const pending = "Pending";
char const *const unimplemented = "Unimplemented";


METHOD (Box, encrypt)
{
  return pending;
}


METHOD (Box, decrypt)
{
  return pending;
}


METHOD (CombinedKey, precompute)
{
  return pending;
}


METHOD (KeyPair, newKeyPair)
{
  return pending;
}


METHOD (KeyPair, fromSecretKey)
{
  return pending;
}


METHOD (Nonce, newNonce)
{
  uint8_t nonce[24] = { 0 };
  new_nonce (nonce);

  SUCCESS {
    msgpack_pack_bin (res, sizeof nonce);
    msgpack_pack_bin_body (res, nonce, sizeof nonce);
  }

  return 0;
}


METHOD (Nonce, increment)
{
  CHECK (args.size == 1);
  CHECK (args.ptr[0].type == MSGPACK_OBJECT_BIN);
  CHECK (args.ptr[0].via.bin.size == 24);

  uint8_t nonce[24];
  memcpy (nonce, args.ptr[0].via.bin.ptr, 24);
  increment_nonce (nonce);

  SUCCESS {
    msgpack_pack_bin (res, sizeof nonce);
    msgpack_pack_bin_body (res, nonce, sizeof nonce);
  }

  return 0;
}


char const *
call_method (msgpack_object_str name, msgpack_object_array args, msgpack_packer *res)
{
#define DISPATCH(SERVICE, NAME) \
  if (name.size == sizeof #SERVICE"."#NAME - 1 && \
      memcmp (name.ptr, #SERVICE"."#NAME, name.size) == 0) \
    return SERVICE##_##NAME (args, res)
  DISPATCH (Binary, decode);
  DISPATCH (Binary, encode);
  DISPATCH (Box, decrypt);
  DISPATCH (Box, encrypt);
  DISPATCH (CombinedKey, precompute);
  DISPATCH (KeyPair, fromSecretKey);
  DISPATCH (KeyPair, newKeyPair);
  DISPATCH (Nonce, increment);
  DISPATCH (Nonce, newNonce);
#undef DISPATCH

  // Default action: "Unimplemented" exception. New tests should be added here
  // returning "Pending" until they are properly implemented.
  return unimplemented;
}

#include "methods.h"

#include "util.h"

#include <crypto_core.h>


char const *const pending = "Pending";
char const *const unimplemented = "Unimplemented";


METHOD (array, Box, encrypt)
{
  return pending;
}


METHOD (array, Box, decrypt)
{
  return pending;
}


METHOD (array, CombinedKey, precompute)
{
  return pending;
}


METHOD (array, KeyPair, newKeyPair)
{
  uint8_t key1[crypto_box_PUBLICKEYBYTES];
  uint8_t key2[crypto_box_SECRETKEYBYTES];
  crypto_box_keypair(key1, key2);

  SUCCESS{
    //init array
    msgpack_pack_array(res, 2);
    msgpack_pack_bin(res, crypto_box_PUBLICKEYBYTES);
    msgpack_pack_bin_body(res, key1, crypto_box_PUBLICKEYBYTES);

    msgpack_pack_bin(res, crypto_box_SECRETKEYBYTES);
    msgpack_pack_bin_body(res, key2, crypto_box_SECRETKEYBYTES);
  }
  return 0;
}


METHOD (array, KeyPair, fromSecretKey)
{
  uint8_t secret_key[crypto_scalarmult_SCALARBYTES];
  memcpy(secret_key, args.ptr[0].via.bin.ptr, crypto_scalarmult_SCALARBYTES);

  uint8_t public_key[crypto_scalarmult_BYTES];
  crypto_scalarmult_base(public_key, secret_key);

  SUCCESS{
    msgpack_pack_array(res, 2);

    msgpack_pack_bin(res, crypto_scalarmult_SCALARBYTES);
    msgpack_pack_bin_body(res, secret_key, crypto_scalarmult_SCALARBYTES);
    msgpack_pack_bin(res, crypto_scalarmult_SCALARBYTES);
    msgpack_pack_bin_body(res, public_key, crypto_scalarmult_SCALARBYTES);
  }
  return 0;
}


METHOD (array, Nonce, newNonce)
{
  uint8_t nonce[24] = { 0 };
  new_nonce (nonce);

  SUCCESS {
    msgpack_pack_bin (res, sizeof nonce);
    msgpack_pack_bin_body (res, nonce, sizeof nonce);
  }

  return 0;
}


METHOD (array, Nonce, increment)
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

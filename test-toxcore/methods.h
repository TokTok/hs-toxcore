#pragma once

#include <msgpack.h>

#define CHECK(cond) if (!(cond)) return #cond
#define SUCCESS msgpack_pack_array (res, 0); if (true)

#define METHOD(SERVICE, NAME) \
char const * \
SERVICE##_##NAME (msgpack_object_array args, msgpack_packer *res)

METHOD (Binary, decode);
METHOD (Binary, encode);

char const *call_method (msgpack_object_str name, msgpack_object_array args, msgpack_packer *res);

extern char const *const pending;
extern char const *const unimplemented;

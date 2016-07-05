#pragma once

#include <msgpack.h>

#define CHECK(cond) if (!(cond)) return #cond
#define SUCCESS msgpack_pack_array (res, 0)

#define METHOD(TYPE, SERVICE, NAME) \
char const * \
SERVICE##_##NAME (msgpack_object_##TYPE args, msgpack_packer *res)

METHOD (array, Binary, decode);
METHOD (array, Binary, encode);

char const *call_method (msgpack_object_str name, msgpack_object_array args, msgpack_packer *res);

extern char const *const pending;
extern char const *const unimplemented;

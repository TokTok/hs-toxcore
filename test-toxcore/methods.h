#pragma once

#include <msgpack.h>

char const *call_method (msgpack_object_str name, msgpack_object_array args, msgpack_packer *res);

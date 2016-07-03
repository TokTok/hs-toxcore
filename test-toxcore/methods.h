#pragma once

#include <msgpack.h>

bool call_method (msgpack_object_str name, msgpack_object_array args, msgpack_packer *res);

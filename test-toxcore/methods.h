#pragma once

#include <msgpack.h>

void call_method (msgpack_object const *req, msgpack_packer *res);

#pragma once

#include <msgpack.h>

void call_method (msgpack_zone *zone, msgpack_object const *req, msgpack_object *res);

#pragma once

#include "util.h"

#include <msgpack.h>


#define CHECK(COND) do { if (!(COND)) return #COND; } while (0)

#define CHECK_SIZE(ARG, SIZE) do { \
  if ((ARG).size != (SIZE)) \
    return ssprintf ( \
        "%s:%d: Size of `" #ARG "' expected to be %zu, but was %zu", \
        __FILE__, __LINE__, SIZE, (ARG).size); \
} while (0)

#define CHECK_TYPE(ARG, ...) do { \
  msgpack_object_type valid[] = { __VA_ARGS__ }; \
  char const *type_check_result = check_type ( \
      __FILE__, __LINE__, \
      #ARG, (ARG).type, sizeof valid / sizeof *valid, valid); \
  if (type_check_result) \
    return type_check_result; \
} while (0)


static inline char const *
check_type (
    char const *file, int line,
    char const *arg, msgpack_object_type type,
    size_t count, msgpack_object_type *valid)
{
  for (size_t i = 0; i < count; i++)
    if (type == valid[i])
      return NULL;
  if (count == 1)
    return ssprintf (
        "%s:%d: Type of `%s' expected to be %s, but was %s",
        file, line, arg,
        type_name (valid[0]),
        type_name (type));
  else if (count == 2)
    return ssprintf (
        "%s:%d: Type of `%s' expected to be %s or %s, but was %s",
        file, line, arg,
        type_name (valid[0]),
        type_name (valid[1]),
        type_name (type));
  return "noooo!";
}


#define SUCCESS msgpack_pack_array (res, 0); if (true)


#define METHOD(TYPE, SERVICE, NAME) \
char const * \
SERVICE##_##NAME (msgpack_object_##TYPE args, msgpack_packer *res)


// These are not defined by msgpack.h, but we need them for uniformity in the
// METHOD macro.
typedef int64_t msgpack_object_i64;
typedef uint64_t msgpack_object_u64;


METHOD (array, Binary, decode);
METHOD (array, Binary, encode);


char const *call_method (msgpack_object_str name, msgpack_object_array args, msgpack_packer *res);

extern char const *const pending;
extern char const *const unimplemented;

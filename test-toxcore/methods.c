#include "methods.h"

void
call_method (msgpack_zone *zone, msgpack_object const *req, msgpack_object *res)
{
  printf ("Request: ");
  msgpack_object_print (stdout, *req);
  printf ("\n");

  *res = *req;
}

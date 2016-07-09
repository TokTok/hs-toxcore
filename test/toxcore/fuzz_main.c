#include "driver.h"

#include <unistd.h>


int
main (int argc, char **argv)
{
  struct settings cfg = { false, false };
  return communicate (cfg, STDIN_FILENO, STDOUT_FILENO);
}

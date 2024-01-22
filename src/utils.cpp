#include <cstdint>
#include "utils.hpp"

bool isBigEndian() {
  union {
    uint32_t i;
    char c[4];
  } bInt = {0x01020304};

  return bInt.c[0] == 1;
}

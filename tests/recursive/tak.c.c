#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define INT long

INT tak (INT x, INT y, INT z) {
  if (y >= x)
    return z;
  else
    return tak(tak(x-1, y, z),
               tak(y-1, z, x),
               tak(z-1, x, y));
}

INT run(INT x, INT y, INT z) {
  if (x < 0) x = 18;
  if (y < 0) y = 12;
  if (z < 0) z = 6;
  return tak(x, y, z);
}

int main(int argc, const char *argv[]) {

  INT x = -1;
  INT y = -1;
  INT z = -1;
  int repeat = 1;
  int i = 1;

  while (argv[i] && argv[i+1]) {
    if (!strcmp(argv[i], "repeat:")) {
      repeat = atoi(argv[i+1]);
      i += 2;
    } else if (!strcmp(argv[i], "x:")) {
      x = atoi(argv[i+1]);
      i += 2;
    } else if (!strcmp(argv[i], "y:")) {
      y = atoi(argv[i+1]);
      i += 2;
    } else if (!strcmp(argv[i], "z:")) {
      z = atoi(argv[i+1]);
      i += 2;
    } else {
      break;
    }
  }

  if (argv[i]) {
    printf("usage: %s [repeat: N] [x: N] [y: N] [z: N]\n", argv[0]);
    exit(1);
  } else {
    INT accum = 0;
    while (repeat-- > 0) {
      accum = (accum+1) | run(x | (accum == 999999999), y | (accum == 999999999), z | (accum == 999999999));
    }
    exit(accum == 999999999); /* trick the compiler... should always return 0 */
  }
}

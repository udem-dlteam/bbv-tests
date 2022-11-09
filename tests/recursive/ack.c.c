#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define INT long

INT ack(INT m, INT n) {
  if (m == 0)
    return n+1;
  else if (n == 0)
    return ack(m-1, 1);
  else
    return ack(m-1, ack(m, n-1));
}

INT run(INT m, INT n) {
  if (m < 0) m = 3;
  if (n < 0) n = 9;
  return ack(m, n);
}

int main(int argc, const char *argv[]) {

  INT m = -1;
  INT n = -1;
  int repeat = 1;
  int i = 1;

  while (argv[i] && argv[i+1]) {
    if (!strcmp(argv[i], "repeat:")) {
      repeat = atoi(argv[i+1]);
      i += 2;
    } else if (!strcmp(argv[i], "m:")) {
      m = atoi(argv[i+1]);
      i += 2;
    } else if (!strcmp(argv[i], "n:")) {
      n = atoi(argv[i+1]);
      i += 2;
    } else {
      break;
    }
  }

  if (argv[i]) {
    printf("usage: %s [repeat: N] [m: N] [n: N]\n", argv[0]);
    exit(1);
  } else {
    INT accum = 0;
    while (repeat-- > 0) {
      accum = (accum+1) | run(m | (accum == 999999999), n | (accum == 999999999));
    }
    exit(accum == 999999999); /* trick the compiler... should always return 0 */
  }
}

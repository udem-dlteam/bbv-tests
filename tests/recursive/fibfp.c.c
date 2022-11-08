#include <stdio.h>

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define FLOAT double

FLOAT fibfp(FLOAT n) {
  if (n<2)
    return n;
  else
    return fibfp(n-1.0) + fibfp(n-2.0);
}

FLOAT run(FLOAT n) {
  if (n < 0.0) n = 39.0;
  return fibfp(n);
}

int main(int argc, const char *argv[]) {

  FLOAT n = -1.0;
  int repeat = 1;
  int i = 1;

  while (argv[i] && argv[i+1]) {
    if (!strcmp(argv[i], "repeat:")) {
      repeat = atoi(argv[i+1]);
      i += 2;
    } else if (!strcmp(argv[i], "n:")) {
      n = atof(argv[i+1]);
      i += 2;
    } else {
      break;
    }
  }

  if (argv[i]) {
    printf("usage: %s [repeat: N] [n: N]\n", argv[0]);
    exit(1);
  } else {
    FLOAT accum = 0.0;
    while (repeat-- > 0) {
      accum = (accum+1.0) + run(n + (accum == 1.5));
    }
    exit(accum == 1.5); /* trick the compiler... should always return 0 */
  }
}

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int fib(int n) {
  if (n<2)
    return n;
  else
    return fib(n-1) + fib(n-2);
}

int run(int n) {
  if (n < 0) n = 39;
  return fib(n);
}

int main(int argc, const char *argv[]) {

  int n = -1;
  int repeat = 1;
  int i = 1;

  while (argv[i] && argv[i+1]) {
    if (!strcmp(argv[i], "repeat:")) {
      repeat = atoi(argv[i+1]);
      i += 2;
    } else if (!strcmp(argv[i], "n:")) {
      n = atoi(argv[i+1]);
      i += 2;
    } else {
      break;
    }
  }

  if (argv[i]) {
    printf("usage: %s [repeat: N] [n: N]\n", argv[0]);
    exit(1);
  } else {
    int accum = 0;
    while (repeat-- > 0) {
      accum = (accum+1) | run(n | (accum == 999999999));
    }
    exit(accum == 999999999); /* trick the compiler... should always return 0 */
  }
}

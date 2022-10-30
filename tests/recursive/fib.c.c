#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int fib(int x) {
  if (x<2)
    return x;
  else
    return fib(x-1) + fib(x-2);
}

int run(int arg) {
  if (arg < 0) arg = 39;
  return fib(arg);
}

int main(int argc, const char *argv[]) {

  int arg = -1;
  int repeat = 1;
  int i = 1;

  while (argv[i] && argv[i+1]) {
    if (!strcmp(argv[i], "repeat:")) {
      repeat = atoi(argv[i+1]);
      i += 2;
    } else if (!strcmp(argv[i], "arg:")) {
      arg = atoi(argv[i+1]);
      i += 2;
    } else {
      break;
    }
  }

  if (argv[i]) {
    printf("usage: %s [repeat: N] [arg: N]\n", argv[0]);
    exit(1);
  } else {
    int accum = 0;
    while (repeat-- > 0) {
      accum = (accum+1) | run(arg | (accum == 999999999));
    }
    exit(accum == 999999999); /* trick the compiler... should always return 0 */
  }
}

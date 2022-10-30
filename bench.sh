#!/bin/sh

bench=tests/recursive/fib
GCC=gcc
arg=39
repeat=10

if true ; then

  for SG in S G ; do
    for V in 0 1 2 3 4 5 ; do
      for I in 0 350 ; do
        ./run --gambit -I $I -$SG -V $V $bench.scm
      done
    done
  done

  for O in 0 1 2 3 ; do
    $GCC -fomit-frame-pointer -O$O $bench.c.c -o $bench.c.O$O.exe
  done

fi

for SG in S G ; do
  for V in 0 1 2 3 4 5 ; do
    for I in 0 350 ; do
      printf "(%s gambit %s V=%s I=%s)\n" `(time $bench.gambit.$I$SG$V.exe repeat: $repeat arg: $arg) 2>&1 | fgrep real | sed -e 's/.*0m//' -e 's/s$//'` $SG $V $I
    done
  done
done

for O in 0 1 2 3 ; do
  printf "(%s $GCC -O%s)\n" `(time $bench.c.O$O.exe repeat: $repeat arg: $arg) 2>&1 | fgrep real | sed -e 's/.*0m//' -e 's/s$//'` $O
done

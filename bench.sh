#!/bin/sh

GCC=gcc

bench=tests/recursive/fib ; args="n: 39" ; repeat=10
#bench=tests/recursive/ack ; args="m: 3 n: 9" ; repeat=50
#bench=tests/recursive/tak ; args="x: 18 y: 12 z: 6" ; repeat=10000

SC_range="gambit"
SG_range="S G"
V_range="0 1 2 3 4 5"
I_range="0 350"
CC_range="$GCC clang"
O_range="0 1 2 3"

if false ; then
  # for quick testing
  SC_range="gambit"
  SG_range="G"
  V_range="0 4"
  I_range="350"
  CC_range="$GCC"
  O_range="1"
fi

if true ; then

  for SC in $SC_range ; do
    for SG in $SG_range ; do
      for V in $V_range ; do
        for I in $I_range ; do
          ./run --$SC -I $I -$SG -V $V $bench.scm
        done
      done
    done
  done

  for CC in $CC_range ; do
    for O in $O_range ; do
      $CC -fomit-frame-pointer -O$O $bench.c.c -o $bench.$CC.O$O.exe
    done
  done

fi

for SC in $SC_range ; do
  for SG in $SG_range ; do
    for V in $V_range ; do
      for I in $I_range ; do
        printf "(%s $SC %s V=%s I=%s)\n" `(time $bench.$SC.$I$SG$V.exe repeat: $repeat $args) 2>&1 | fgrep real | sed -e 's/.*0m//' -e 's/s$//'` $SG $V $I
      done
    done
  done
done

for CC in $CC_range ; do
  for O in $O_range ; do
    printf "(%s $CC -O%s)\n" `(time $bench.$CC.O$O.exe repeat: $repeat $args) 2>&1 | fgrep real | sed -e 's/.*0m//' -e 's/s$//'` $O
  done
done

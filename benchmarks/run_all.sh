#!/bin/bash
#set -euxo pipefail

files=("../tests/paper/micro/rev.scm"
       "../tests/paper/micro/fibfp.scm"
       "../tests/paper/micro/tak.scm"
       "../tests/paper/micro/ack.scm"
       "../tests/paper/micro/vlen.scm"
       "../tests/paper/micro/vec.scm"
       "../tests/paper/micro/primes.scm"
       "../tests/paper/micro/nqueens.scm"
       "../tests/paper/macro/conform.scm"
       "../tests/paper/macro/slatex.scm"
       "../tests/paper/macro/almabench.scm")


#$(find ../tests/paper -type f -name "*.scm")

gambit_strategies=("linear")

bigloo_strategies=("size")

. ./venv/bin/activate

gambit=../../gambit

for file in ${files[@]}; do
  # Gambit
  python ./benchmark.py -v benchmark ${file} -g ${gambit} -l 0 1 5 -r 1 -m linear -f -t 1800
  # python ./benchmark.py -v benchmark ${file} -O -g ${gambit} -l 0 1 2 3 4 5 -r 10 -m linear -f -t 1800
  # Bigloo
  # python ./benchmark.py -v benchmark ${file} -b -l 0 1 2 3 4 5 -r 10 -m size -f -t 1800
  # python ./benchmark.py -v benchmark ${file} -O -b -l 0 1 2 3 4 5 -r 10 -m size -f -t 1800
done

deactivate

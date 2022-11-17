#!/bin/bash
#set -euxo pipefail

files=("bglstone/almabench.scm"
       "bglstone/bague.scm"
       "bglstone/beval.scm"
       "bglstone/maze.scm"
       "bglstone/mbrot.scm"
       "bglstone/peval.scm"
       "recursive/ack.scm"
       "recursive/fib.scm"
       "recursive/fibfp.scm"
       "recursive/tak.scm"
       "recursive/takl.scm"
       "recursive/diviter.scm"
       "recursive/divrec.scm"
       "recursive/array1.scm"
       "recursive/primes.scm"
       "recursive/newton.scm"
       "gabriel/browse.scm"
       "gabriel/mazefun.scm"
       "gabriel/nqueens.scm"
       "gabriel/puzzle.scm"
       "gabriel/quicksort.scm"
       "gabriel/sum.scm"
       "gabriel/sumfp.scm"
       "gabriel/triangl.scm")

for file in ${files[@]}; do
  timeout 600 ./benchmark.py ../tests/${file} -g ../../gambit -v -l 0 1 2 3 4 5 6 7 8 -csv results/${file}.csv
done

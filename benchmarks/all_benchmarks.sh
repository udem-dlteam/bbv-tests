#!/bin/bash
#set -euxo pipefail

files=("bglstone/almabench"
       "bglstone/bague"
       "bglstone/beval"
       "bglstone/maze"
       "bglstone/mbrot"
       "bglstone/peval"
       "recursive/ack"
       "recursive/fib"
       "recursive/fibfp"
       "recursive/tak"
       "recursive/takl"
       "recursive/diviter"
       "recursive/divrec"
       "recursive/array1"
       "recursive/primes"
       "recursive/newton"
       "gabriel/browse"
       "gabriel/mazefun"
       "gabriel/nqueens"
       "gabriel/puzzle"
       "gabriel/quicksort"
       "gabriel/sum"
       "gabriel/sumfp"
       "gabriel/triangl")

for file in ${files[@]}; do
  timeout 600 ./benchmark.py ../tests/${file}.scm -g ../../gambit -v -l 0 1 2 3 4 5 6 7 8 -t 60 -csv results/${file}.csv
done

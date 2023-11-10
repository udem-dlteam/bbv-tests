#!/bin/bash
#set -euxo pipefail

files=("recursive/ack"
       "recursive/fib"
       "recursive/fibfp"
       "recursive/tak"
       "recursive/takl"
       "recursive/diviter"
       "recursive/divrec"
       "recursive/array1"
       "recursive/primes"
       "recursive/newton"
       "bglstone/almabench"
       "bglstone/bague"
       "bglstone/beval"
       "bglstone/maze"
       "bglstone/mbrot"
       "bglstone/peval"
       "gabriel/browse"
       "gabriel/mazefun"
       "gabriel/nqueens"
       "gabriel/puzzle"
       "gabriel/quicksort"
       "gabriel/sum"
       "gabriel/sumfp"
       "gabriel/triangl")

strategies=("linear"
            "sametypes"
            "entropy")

. ./venv/bin/activate

gambit=../../gambit

for file in ${files[@]}; do
  for strat in ${strategies[@]}; do
    python ./benchmark.py -v benchmark ../tests/${file}.scm -g ${gambit} -r 5 -m ${strat} -f
  done
done

deactivate

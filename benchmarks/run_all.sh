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
       "gabriel/browse"
       "gabriel/mazefun"
       "gabriel/nqueens"
       "gabriel/puzzle"
       "gabriel/quicksort"
       "gabriel/sum"
       "gabriel/sumfp"
       "gabriel/triangl"
       "bglstone/bague"
       "bglstone/almabench"
       # Uses default repeat: 20 in benchmarks.py
       # So run them last in case one of them takes forever
       "recursive/primes"
       "recursive/newton"
       "bglstone/beval"
       "bglstone/maze"
       "bglstone/mbrot"
       "bglstone/peval")

gambit_strategies=("linear"
                   "sametypes"
                   "entropy")

bigloo_strategies=("size")

. ./venv/bin/activate

gambit=../../gambit

for file in ${files[@]}; do
  # Gambit
  python ./benchmark.py -v benchmark ../tests/${file}.scm -g ${gambit} -l 0 1 2 3 4 5 -r 50 -m linear -f
  python ./benchmark.py -v benchmark ../tests/${file}.scm -O -g ${gambit} -l 0 1 2 3 4 5 -r 50 -m linear -f
  # Bigloo
  python ./benchmark.py -v benchmark ../tests/${file}.scm -b -l 0 1 2 3 4 5 -r 50 -m size -f
  python ./benchmark.py -v benchmark ../tests/${file}.scm -O -b -l 0 1 2 3 4 5 -r 50 -m size -f
done

deactivate

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

. ./venv/bin/activate

gambit=../../gambit

for file in ${files[@]}; do
  python ./benchmark.py ../tests/${file}.scm -g ${gambit} -n 5 -v -l 0 1 2 3 4 5 -m linear entropy sametypes -chart results/${file}-time.png -chart-params time
  python ./benchmark.py ../tests/${file}.scm -g ${gambit} -n 5 -v -l 0 1 2 3 4 5 -m linear entropy sametypes -chart results/${file}-instructions.png -chart-params machine_instructions
  python ./benchmark.py ../tests/${file}.scm -g ${gambit} -n 5 -v -l 0 1 2 3 4 5 -m linear entropy sametypes -chart results/${file}-typechecks.png -chart-params typechecks
  python ./benchmark.py ../tests/${file}.scm -g ${gambit} -n 5 -v -l 0 1 2 3 4 5 -chart results/${file}-primitives.png -chart-params primitives 8
done

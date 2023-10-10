#!/bin/bash
#set -euxo pipefail

files=("ack"
       "fib"
       "fibfp"
       "tak"
       "takl"
       "diviter"
       "divrec"
       "array1"
       "primes"
       "newton"
       "almabench"
       "bague"
       "beval"
       "maze"
       "mbrot"
       "peval"
       "browse"
       "mazefun"
       "nqueens"
       "puzzle"
       "quicksort"
       "sum"
       "sumfp"
       "triangl")

strategies=("linear"
            "sametypes"
            "entropy")

. ./venv/bin/activate

system=arctic

for strat in ${strategies[@]}; do
  python ./benchmark.py -d analysis -s ${system} -m ${strat} -o results/overall
  for file in ${files[@]}; do
    python ./benchmark.py -d plot -b ${file} -s ${system} -p 8 -e task-clock -o results/individual
    python ./benchmark.py -d plot -b ${file} -s ${system} -e task-clock instructions -o results/individual
  done
done

deactivate

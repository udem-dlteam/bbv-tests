#!/bin/bash
#set -euxo pipefail

files=("recursive/ack"
       "recursive/fib"
       "recursive/fibfp"
       "recursive/tak")

strategies=("linear"
            "sametypes"
            "entropy")

. ./venv/bin/activate

gambit=../../gambit

for file in ${files[@]}; do
  for strat in ${strategies[@]}; do
    python ./benchmark.py -v benchmark ../tests/${file}.scm -g ${gambit} -m ${strat} -f
  done
done

deactivate

#!/bin/bash
#set -euxo pipefail

files=$(find ../tests/paper -type f -name "*.scm")

gambit_strategies=("linear")

bigloo_strategies=("size")

. ./venv/bin/activate

gambit=../../gambit

for file in ${files[@]}; do
  # Gambit
  python ./benchmark.py -v benchmark ${file} -g ${gambit} -l 0 1 2 3 4 5 -r 10 -m linear -f -t 1800
  python ./benchmark.py -v benchmark ${file} -O -g ${gambit} -l 0 1 2 3 4 5 -r 10 -m linear -f -t 1800
  # Bigloo
  python ./benchmark.py -v benchmark ${file} -b -l 0 1 2 3 4 5 -r 10 -m size -f -t 1800
  python ./benchmark.py -v benchmark ${file} -O -b -l 0 1 2 3 4 5 -r 10 -m size -f -t 1800
done

deactivate

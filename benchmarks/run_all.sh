#!/bin/bash
#set -euxo pipefail

files=$(find ../tests/paper -type f -name "*.scm")

. ./venv/bin/activate

gambit=../../gambit

for file in ${files[@]}; do
  # Gambit
  python ./benchmark.py -v benchmark ${file} -g ${gambit} -l 0 1 2 3 4 8 16 -r 5 -m linear -f -t 1800
  python ./benchmark.py -v benchmark ${file} -O -g ${gambit} -l 0 1 2 3 4 8 16 -r 5 -m linear -f -t 1800
  # Bigloo
  python ./benchmark.py -v benchmark ${file} -b ${gambit} -l 0 1 2 3 4 8 16 -r 5 -m size -f -t 1800
  python ./benchmark.py -v benchmark ${file} -O -b ${gambit} -l 0 1 2 3 4 8 16 -r 5 -m size -f -t 1800
done

deactivate

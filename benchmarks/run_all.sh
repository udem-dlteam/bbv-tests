#!/bin/bash

files=$(find ../tests/paper -type f -name "*.scm")

. ./venv/bin/activate

gambit=../../gambit

for limit in 0 4 1 2 3 8 16; do
  for file in ${files[@]}; do
    # Gambit
    python ./benchmark.py -v benchmark ${file} -g ${gambit} -l ${limit} -r 5 -f -t 3600
    python ./benchmark.py -v benchmark ${file} -O -g ${gambit} -l ${limit} -r 5 -f -t 3600
    python ./benchmark.py -v benchmark ${file} -u -g ${gambit} -l ${limit} -r 5 -f -t 3600
    python ./benchmark.py -v benchmark ${file} -u -O -g ${gambit} -l ${limit} -r 5 -f -t 3600
    # Bigloo
    python ./benchmark.py -v benchmark ${file} --bigloo -l ${limit} -r 5 -f -t 3600
    python ./benchmark.py -v benchmark ${file} -O --bigloo -l ${limit} -r 5 -f -t 3600
    python ./benchmark.py -v benchmark ${file} -u --bigloo -l ${limit} -r 5 -f -t 3600
    python ./benchmark.py -v benchmark ${file} -u -O --bigloo -l ${limit} -r 5 -f -t 3600
  done
done

deactivate

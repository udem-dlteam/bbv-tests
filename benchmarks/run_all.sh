#!/bin/bash
#set -euxo pipefail

files=../tests/paper/macro/earley.scm #$(find ../tests/paper -type f -name "*.scm")

. ./venv/bin/activate

gambit=../../gambit

for file in ${files[@]}; do
  # Gambit
  python ./benchmark.py -v benchmark ${file} -g ${gambit} -l 0 1 2 3 4 8 16 -r 5 -f -t 3600
  python ./benchmark.py -v benchmark ${file} -O -g ${gambit} -l 0 1 2 3 4 8 16 -r 5 -f -t 3600
  python ./benchmark.py -v benchmark ${file} -u -g ${gambit} -l 0 1 2 3 4 8 16 -r 5 -f -t 3600
  python ./benchmark.py -v benchmark ${file} -u -O -g ${gambit} -l 0 1 2 3 4 8 16 -r 5 -f -t 3600
  # Bigloo
  python ./benchmark.py -v benchmark ${file} --bigloo -l 0 1 2 3 4 8 16 -r 5 -f -t 3600
  python ./benchmark.py -v benchmark ${file} -O --bigloo -l 0 1 2 3 4 8 16 -r 5 -f -t 3600
  python ./benchmark.py -v benchmark ${file} -u --bigloo -l 0 1 2 3 4 8 16 -r 5 -f -t 3600
  python ./benchmark.py -v benchmark ${file} -u -O --bigloo -l 0 1 2 3 4 8 16 -r 5 -f -t 3600
done

deactivate

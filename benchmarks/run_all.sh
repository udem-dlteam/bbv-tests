#!/bin/bash

files=$(find ../tests/paper -type f -name "*.scm")

. ./venv/bin/activate

gambit=../../bbv-gambit

reps=50
timeout=3600

# BBV runs
for limit in 0 1 2 3 4 5 6 7 8 9 10 20; do
  for file in ${files[@]}; do
    # Gambit
    python ./benchmark.py -v benchmark ${file} -O -g ${gambit} -l ${limit} -r ${reps} -t ${timeout}
    
    # Bigloo
    python ./benchmark.py -v benchmark ${file} -O --bigloo -l ${limit} -r ${reps} -t ${timeout}
  done
done

for file in ${files[@]}; do
  # Unsafe runs
  python ./benchmark.py -v benchmark ${file} -O -u -g ${gambit} -l 0 -r ${reps} -t ${timeout}
  python ./benchmark.py -v benchmark ${file} -O -u --bigloo -l 0 -r ${reps} -t ${timeout}
  
  # Gambit with no optimizations at all
  python ./benchmark.py -v benchmark ${file} -g ${gambit} -l 0 -r 1 -t ${timeout}
done

deactivate

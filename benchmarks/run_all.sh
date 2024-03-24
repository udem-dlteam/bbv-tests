#!/bin/bash

scm_files=$(find ../tests/paper -type f -name "*.scm")
js_files=$(find ../tests/paper -type f -name "*.js")

. ./venv/bin/activate

gambit=../../bbv-gambit
chez=../../ChezScheme

reps=50
timeout=3600

# Do NodeJS and Chez runs first they should be quick, having no version limits
for file in ${js_files[@]}; do
  python ./benchmark.py -v benchmark ${file} -O --node -l 0 -r ${reps} -t ${timeout}
done

for file in ${scm_files[@]}; do
  python ./benchmark.py -v benchmark ${file} -O --chez ${chez} -l 0 -r ${reps} -t ${timeout}
done

# BBV runs
for limit in 0 1 2 3 4 5 6 7 8 9 10 15 20; do
  for file in ${scm_files[@]}; do
    # Gambit
    python ./benchmark.py -v benchmark ${file} -O --gambit ${gambit} -l ${limit} -r ${reps} -t ${timeout}
    
    # Bigloo
    python ./benchmark.py benchmark ${file} -O --bigloo -l ${limit} -r ${reps} -t ${timeout}
  done
done

for file in ${scm_files[@]}; do
  # Unsafe runs
  python ./benchmark.py -v benchmark ${file} -O -u --gambit ${gambit} -l 0 -r ${reps} -t ${timeout}
  python ./benchmark.py -v benchmark ${file} -O -u --bigloo -l 0 -r ${reps} -t ${timeout}
  
  # Gambit with no optimizations at all for primitive count
  python ./benchmark.py -v benchmark ${file} --gambit ${gambit} -l 0 -r 1 -t ${timeout}
done

deactivate

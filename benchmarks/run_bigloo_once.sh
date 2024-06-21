#!/bin/bash

. ./venv/bin/activate

scm_files=$(find ../tests/paper -type f -regex '^.*/[^.]*\.scm$')

reps=1
timeout=3600

verbosity="-v"

limit=$1

# BBV runs
for file in ${scm_files[@]}; do
  # Bigloo
  python ./benchmark.py $verbosity benchmark ${file} -O --bigloo -l ${limit} -r ${reps} -t ${timeout}
done

for file in ${scm_files[@]}; do
  # Unsafe runs
  python ./benchmark.py $verbosity benchmark ${file} -O -u --bigloo -l 0 -r ${reps} -t ${timeout}
done

deactivate

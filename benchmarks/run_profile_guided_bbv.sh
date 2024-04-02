#!/bin/bash
sleep 5

scm_files=$(find ../tests/paper -type f -regex '^.*macro/[^.]*\.scm$')

. ./venv/bin/activate

gambit=../../bbv-gambit

reps=10
timeout=3600

verbosity="-v"

for limit in 0 5; do
  for file in ${scm_files[@]}; do
    # Gambit
    python ./benchmark.py $verbosity benchmark ${file} -O --gambit ${gambit} -l 0 -r ${reps} -t ${timeout}
  done
done

for file in ${scm_files[@]}; do
  python ./benchmark.py $verbosity profile_guided ${file} --gambit ${gambit} -r ${reps} -t ${timeout}
done

deactivate

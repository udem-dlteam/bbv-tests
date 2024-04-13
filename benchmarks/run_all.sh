#!/bin/bash
sleep 5

scm_files=$(find ../tests/paper -type f -regex '^.*/[^.]*\.scm$')
js_files=$(find ../tests/paper -type f -regex '^.*/[^.]*\.js$')
rkt_files=$(find ../tests/paper -type f -regex '^.*/[^.]*\.rkt$')

. ./venv/bin/activate

gambit=../../bbv-gambit
racket=../../racket

reps=10
timeout=3600

verbosity="-v"

# BBV runs
for limit in 0 1 2 3 4 5 6 7 8 9 10 20; do
  for file in ${scm_files[@]}; do
    # Gambit
    python ./benchmark.py $verbosity benchmark ${file} -O --gambit ${gambit} -l ${limit} -r ${reps} -t ${timeout}
    
    # Bigloo
    python ./benchmark.py $verbosity benchmark ${file} -O --bigloo -l ${limit} -r ${reps} -t ${timeout}
  done
done

for file in ${scm_files[@]}; do
  # Unsafe runs
  python ./benchmark.py $verbosity benchmark ${file} -O -u --gambit ${gambit} -l 0 -r ${reps} -t ${timeout}
  python ./benchmark.py $verbosity benchmark ${file} -O -u --bigloo -l 0 -r ${reps} -t ${timeout}
  
  # Gambit with no optimizations at all for primitive count
  python ./benchmark.py $verbosity benchmark ${file} --gambit ${gambit} -l 0 -r 1 -t ${timeout}
done

# NodeJS, Chez and Racket
for file in ${rkt_files[@]}; do
  python ./benchmark.py $verbosity benchmark ${file} -O --racket ${racket} -l 0 -r ${reps} -t ${timeout}
done

for file in ${js_files[@]}; do
  python ./benchmark.py $verbosity benchmark ${file} -O --node -l 0 -r ${reps} -t ${timeout}
done

for file in ${scm_files[@]}; do
  python ./benchmark.py $verbosity benchmark ${file} -O --chez -l 0 -r ${reps} -t ${timeout}
done

deactivate

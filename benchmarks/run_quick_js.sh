#!/bin/bash
sleep 5

js_files=$(find ../tests/paper -type f -regex '^.*/[^.]*\.js$')
rkt_files=$(find ../tests/paper -type f -regex '^.*/[^.]*\.rkt$')

. ./venv/bin/activate

reps=5
timeout=3600

verbosity="-v"

for file in ${rkt_files[@]}; do
  python ./benchmark.py $verbosity benchmark ${file} -O --racket ${racket} -l 0 -r ${reps} -t ${timeout}
done

for file in ${js_files[@]}; do
  python ./benchmark.py $verbosity benchmark ${file} -O --node -l 0 -r ${reps} -t ${timeout}
done

deactivate

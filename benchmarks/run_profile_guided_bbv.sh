#!/bin/bash
sleep 5

. ./venv/bin/activate

gambit=../../bbv-gambit

reps=10
timeout=1800

verbosity="-v"

file=../tests/paper/macro/almabench.scm
python ./benchmark.py $verbosity profile_guided ${file} --gambit ${gambit} -r ${reps} -t ${timeout}

deactivate

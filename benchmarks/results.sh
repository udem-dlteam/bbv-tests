#!/bin/bash

files=$(find ../tests/paper -type f -name "*.scm")

benchmarks=() # Initialize an empty array

for file in ${files[@]}; do
    base=$(basename "$file" .scm)  # Extracts the base name without the extension
    benchmarks+=("$base")       # Append the base name to the array
done

benchmarks=( "${benchmarks[@]/rev}" )
benchmarks=( "${benchmarks[@]/vlen}" )
#benchmarks=( "${benchmarks[@]/vec}" )

version_limits="0 1 2 3 4 5 6 7 8 9 10 20"
system="arctic"

. ./venv/bin/activate

python benchmark.py csv -s $system -c bbv-gambit -b ${benchmarks[@]} -l $version_limits
python benchmark.py csv -s $system -c bigloo -b ${benchmarks[@]} -l $version_limits

python benchmark.py heatmap -c bbv-gambit -s $system -l $version_limits -b ${benchmarks[@]}
python benchmark.py heatmap -c bigloo -s $system -l $version_limits -b ${benchmarks[@]}

deactivate

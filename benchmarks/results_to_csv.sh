#!/bin/bash

files=$(find ../tests/paper -type f -name "*.scm")

benchmarks=() # Initialize an empty array

for file in ${files[@]}; do
    base=$(basename "$file" .scm)  # Extracts the base name without the extension
    benchmarks+=("$base")       # Append the base name to the array
done

. ./venv/bin/activate

python benchmark.py csv -s arctic -c gambit -b ${benchmarks[@]} -l 0 1 2 3 4 8 16
python benchmark.py csv -s arctic -c bigloo -b ${benchmarks[@]} -l 0 1 2 3 4 8 16

deactivate

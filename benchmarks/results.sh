#!/bin/bash

files=$(find ../tests/paper -type f -name "*.scm")
js_files=$(find ../tests/paper -type f -name "*.js")

benchmarks=()

for file in ${files[@]}; do
    base=$(basename "$file" .scm)  # Extracts the base name without the extension
    benchmarks+=("$base")          # Append the base name to the array
done

js_benchmarks=()

for file in ${js_files[@]}; do
    base=$(basename "$file" .js)
    js_benchmarks+=("$base")
done

version_limits="0 5"
system="arctic"

. ./venv/bin/activate

python benchmark.py csv -s $system -l $version_limits

python benchmark.py heatmap -c gambit -s $system -l $version_limits -b ${benchmarks[@]}
python benchmark.py heatmap -c bigloo -s $system -l $version_limits -b ${benchmarks[@]}

deactivate

scm_files=$(find ../tests/paper/micro -type f -regex '^.*/[^.]*\.scm$')

gambit=../../bbv-gambit

geomean=1.0
order=0

# BBV runs
for file in ${scm_files[@]}; do
    order=$((order + 1))

    # Gambit
    out_V0=env BBV_PARAMETERS=$@ ../compile -S gambit -D ${gambit} -V 0 -O3 -f ${file} -P
    out_V3=env BBV_PARAMETERS=$@ ../compile -S gambit -D ${gambit} -V 3 -O3 -f ${file} -P

    checks_V0=$(echo "$out_V0" | grep -oE '\(#.*?[0-9]+\)')
    checks_V3=$(echo "$out_V3" | grep -oE '\(#.*?[0-9]+\)')

    sum_V0=$(echo "$checks_V0" | sed -n 's/.*(\#.* \([0-9]\+\)).*/\1/p' | awk '{s+=$1} END {print s}')
    sum_V3=$(echo "$checks_V3" | sed -n 's/.*(\#.* \([0-9]\+\)).*/\1/p' | awk '{s+=$1} END {print s}')

    ratio=$(echo "$sum_V3 / $sum_V0" | bc -l)
    geomean=$(echo "$geomean * $ratio" | bc -l)
done

geometric_mean=$(echo "scale=10; e(l($geomean)/$order)*100" | bc -l)
rounded_geometric_mean=$(printf "%.0f" "$geometric_mean")

echo $rounded_geometric_mean

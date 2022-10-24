#!/usr/bin/env python3

import argparse
import os
import re
import subprocess

DEFAULT_PRIMITIVE_COUNTER_MARKER = '***primitive-call-counter'
MAX_VLIMIT = 5

def parse_gambit_output(output):
    counter_section = output.split(DEFAULT_PRIMITIVE_COUNTER_MARKER)[1]
    counts = re.findall("\(([^ ]+) (\d+)\)", counter_section)

    return {k: int(v) for k, v in counts}

def call_gsc_with_version_limit(gambit_path, filename, vlimit):
    vlimit_flag = f'-prelude "(declare (version-limit {vlimit}))"'
    command = f"cd {gambit_path};./gsi/gsi -:= gsc/igsc.scm -gvm-interpret {vlimit_flag} {filename}"

    output = subprocess.run(command, capture_output=True, shell=True)

    return parse_gambit_output(output.stdout.decode())

def benchmark(gambit_path, filename, vlimits):
    results = {}

    for vlimit in vlimits:
        counts = call_gsc_with_version_limit(gambit_path, filename, vlimit)
        results[vlimit] = counts

    return results

def print_results(bench_results):
    def print_tabule(table):
        widths = [max(len(l[i]) for l in table) + 1 for i in range(len(table[0]))]

        line = '+' + '+'.join('-' * w for w in widths) + '+'

        for row in table:
            print(line)
            print(*['', *(i.ljust(w) for i, w in zip(row, widths)), ''], sep='|')
        print(line)


    # Get all primitive names across all executions
    all_primitive_names = sorted(set(k for row in bench_results.values() for k in row.keys()))

    # Convert dict to table
    table = [['VERSIONS', *all_primitive_names]]
    baseline = bench_results[min(bench_results)]
    must_add_percent = {n: len(set(b.get(n) for b in bench_results.values())) != 1 for n in all_primitive_names}
    for vlimit, results in bench_results.items():
        row = [str(vlimit)]
        for n in all_primitive_names:
            # Add percentage
            count = results.get(n, 0)
            baseline_count = baseline.get(n, 0)
            ratio = '' if not count or not baseline_count or not must_add_percent[n] else f' ({count * 100 // baseline_count}%)'
            row.append(str(count) + ratio)
        table.append(row)

    # print
    print_tabule(table)


def main(gambit_path, filename):
    gambit_path = os.path.abspath(gambit_path)
    filename = os.path.abspath(filename)

    res = benchmark(gambit_path, filename, range(1, MAX_VLIMIT + 1))
    print_results(res)


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Benchmark primitive usage of Gambit BBV")

    parser.add_argument('gambit_path', help='Gambit root path')
    parser.add_argument('file', help='file to benchmark')

    args = parser.parse_args()

    main(args.gambit_path, args.file)

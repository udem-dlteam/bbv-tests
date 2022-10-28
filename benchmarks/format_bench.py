#!/usr/bin/env python3

import argparse
import csv
import os
import re
import subprocess

DEFAULT_PRIMITIVE_COUNTER_MARKER = '***primitive-call-counter'
DEFAULT_EXECUTION_TIME_MARKER = '***time'

def extract_primitives_count(content):
    if DEFAULT_PRIMITIVE_COUNTER_MARKER in content:
        counter_section = content.split(DEFAULT_PRIMITIVE_COUNTER_MARKER)[1]
        counts = re.findall("\(([^ ]+) (\d+)\)", counter_section)

        return {k: int(v) for k, v in counts}
    else:
        return None

def extract_execution_time(content):
    if DEFAULT_EXECUTION_TIME_MARKER in content:
        time_section = content.split(DEFAULT_EXECUTION_TIME_MARKER)[1]
        counts = re.search("(\d+):(\d+).(\d+)", time_section)

        minutes, seconds, fraction = map(int, counts.group(1, 2, 3))

        return 60 * minutes  + seconds + fraction / 10 ** len(counts.group(3))
    else:
        return None

def results_to_table(bench_results, base_key):
    # Get all primitive names across all executions
    all_primitive_names = []
    for bench in bench_results.values():
        if bench['primitives']  is not None:
            all_primitive_names.extend(bench['primitives'].keys())
    all_primitive_names = sorted(set(all_primitive_names))

    # Convert dict to table
    table = [['VERSIONS', 'TIME', *all_primitive_names]]
    baseline = bench_results[base_key]
    basetime = baseline['time']
    basecount = baseline['primitives']

    for name, results in bench_results.items():
        row = [name, results['time']]
        primitive_count = results['primitives']
        if primitive_count is None:
            row.extend([None] * len(all_primitive_names))
        else:
            for n in all_primitive_names:
                count = primitive_count.get(n, 0)
                row.append(primitive_count.get(n, 0))
        table.append(row)
    return table

def print_table(table):
    table = [[str(e) for e in row] for row in table]
    widths = [max(len(l[i]) for l in table) + 1 for i in range(len(table[0]))]

    line = '+' + '+'.join('-' * w for w in widths) + '+'

    for row in table:
        print(line)
        print(*['', *(i.ljust(w) for i, w in zip(row, widths)), ''], sep='|')
    print(line)

def main(trace_files):
    results = {}
    for filename in trace_files:
        with open(filename) as f:
            name = os.path.basename(filename)
            content = f.read()
            primitive_count = extract_primitives_count(content)
            execution_time = extract_execution_time(content)
            results[name] = {'time': execution_time,
                             'primitives': primitive_count}

    table = results_to_table(results, os.path.basename(trace_files[0]))
    print_table(table)


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Format benchmark results")

    parser.add_argument('trace_files', nargs='+', help='trace files (first one is used as base)')

    args = parser.parse_args()

    main(args.trace_files)

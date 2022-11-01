#!/usr/bin/env python3

import argparse
import csv
import copy
import os
import re
import statistics
import subprocess

DEFAULT_PRIMITIVE_COUNTER_MARKER = '***primitive-call-counter'
DEFAULT_EXECUTION_TIME_MARKER = '***time'

SIMILAR_PRIMITIVES = [['##fx+', '##fx+?'],
                      ['##fx-', '##fx-?'],
                      ['##fx*', '##fx*?']]

SIMILAR_PRIMITIVES_TABLE = {prim: simils for simils in SIMILAR_PRIMITIVES for prim in simils}

def group_similar_primitives(primitives):
    grouped_primitives = {}

    for k, v in primitives.items():
        group = SIMILAR_PRIMITIVES_TABLE.get(k)
        if group:
            group_name = '/'.join(group)
            grouped_primitives.setdefault(group_name, 0)
            grouped_primitives[group_name] += v
        else:
            grouped_primitives[k] = v

    return grouped_primitives

def extract_primitives_count(content):
    if DEFAULT_PRIMITIVE_COUNTER_MARKER in content:
        counter_section = content.split(DEFAULT_PRIMITIVE_COUNTER_MARKER)[1]
        counts = re.findall("\(([^ ]+) (\d+)\)", counter_section)

        return group_similar_primitives({k: int(v) for k, v in counts})
    else:
        return None

def extract_execution_time(content):
    if DEFAULT_EXECUTION_TIME_MARKER in content:
        time_section = content.split(DEFAULT_EXECUTION_TIME_MARKER)[1]
        all_times = re.findall(r"real\s+(\d+):(\d+).(\d+)", time_section) \
                    or re.findall(r"(\d+):(\d+).(\d+)elapsed", time_section) # support both bash and sh time

        times = []

        for t in all_times:
            minutes, seconds, fraction = t
            times.append(60 * int(minutes)  + int(seconds) + int(fraction) / 10 ** len(fraction))

        return times
    else:
        return None

def add_percentages_to_results(bench_results, all_primitive_names):
    def make_getter(*path):
        def get(o, default):
            for k in path[:-1]: o = o[k]
            return o.get(path[-1], default)
        return get

    def make_setter(*path):
        def set(o, v):
            for k in path[:-1]: o = o[k]
            o[path[-1]] = v
        return set

    def add_percentage(values, getter, setter):
        raw_counts = [getter(b, 0) for b in values]
        if len(set(raw_counts)) == 1 or not raw_counts[0]:
            counts = [str(c) for c in raw_counts]
        else:
            base = raw_counts[0]
            counts = [str(base)]
            counts.extend(f'{c} ({c * 100 / base:.1f}%)' for c in raw_counts[1:])

        for i, b in enumerate(bench_results):
            setter(b, counts[i])

    bench_results = copy.deepcopy(bench_results)
    add_percentage(bench_results, make_getter('time'), make_setter('time'))

    for name in all_primitive_names:
        add_percentage(bench_results,
                       make_getter('primitives', name),
                       make_setter('primitives', name))

    return bench_results


def results_to_table(bench_results):
    # Get all primitive names across all executions
    all_primitive_names = []
    for bench in bench_results:
        if bench['primitives']  is not None:
            all_primitive_names.extend(bench['primitives'].keys())
    all_primitive_names = sorted(set(all_primitive_names))

    bench_results = add_percentages_to_results(bench_results, all_primitive_names)

    # Convert dict to table
    table = [['VERSIONS', 'TIME', *all_primitive_names]]
    baseline = bench_results[0]
    basetime = baseline['time']
    basecount = baseline['primitives']

    for results in bench_results:
        name = results['name']
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
    def join(values, sep):
        return sep + sep.join(values) + sep

    table = [[str(e) for e in row] for row in table]
    widths = [max(len(l[i]) for l in table) + 1 for i in range(len(table[0]))]

    line = join(['-' * w for w in widths], '+') + '\n'

    row_texts = []

    for row in table:
        row_text = join([row[0].ljust(widths[0]),
                         *(i.rjust(w) for i, w in zip(row[1:], widths[1:]))],
                        '|')
        row_text += '\n'
        row_texts.append(row_text)

    text = join(row_texts, line)

    print(text, end='')

def main(trace_files):
    results = []
    for filename in trace_files:
        with open(filename) as f:
            name = os.path.basename(filename)
            content = f.read()
            primitive_count = extract_primitives_count(content)
            relevant_execution_times = extract_execution_time(content)[1:]
            average_time = statistics.mean(relevant_execution_times)
            variance =  statistics.pvariance(relevant_execution_times)
            results.append({'name': name,
                            'time': average_time,
                            'variance': variance,
                            'primitives': primitive_count})

    table = results_to_table(results)
    print_table(table)


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Format benchmark results")

    parser.add_argument('trace_files', nargs='+', help='trace files (first one is used as base)')

    args = parser.parse_args()

    main(args.trace_files)

#!/usr/bin/env python3

import argparse
import csv
import copy
import locale
import os
import re
import statistics
import subprocess

locale.setlocale(locale.LC_ALL, 'en_US.UTF-8')

def aton(n):
    try:
        return locale.atoi(n)
    except ValueError:
        return locale.atof(n)

VERBOSE = False
RUN_SCRIPT = "../compile"
DEFAULT_PRIMITIVE_COUNTER_MARKER = '***primitive-call-counter'

TITLES_ORDER = ['version', 'time', 'branch']

SIMILAR_PRIMITIVES = [['##fx+', '##fx+?'],
                      ['##fx-', '##fx-?'],
                      ['##fx*', '##fx*?']]

SIMILAR_PRIMITIVES_TABLE = {prim: simils for simils in SIMILAR_PRIMITIVES for prim in simils}

def verbose(*args, **kwargs):
    if VERBOSE: print(*args, **kwargs)

def group_similar_primitives(primitives):
    grouped_primitives = {}
    seen = {}

    for k, v in primitives.items():
        group = SIMILAR_PRIMITIVES_TABLE.get(k)
        if group:
            group_name = ' & '.join(group)

            grouped_primitives.setdefault(group_name, 0)
            seen.setdefault(group_name, 0)

            grouped_primitives[group_name] += v
            seen[group_name] += 1

    relevant_grouped_primitives = {k: v for k, v in grouped_primitives.items() if seen[k] > 1}

    return {**primitives, **relevant_grouped_primitives}

def extract_primitives_count(content):
    if DEFAULT_PRIMITIVE_COUNTER_MARKER in content:
        counter_section = content.split(DEFAULT_PRIMITIVE_COUNTER_MARKER)[1]
        counts = re.findall("\(([^ ]+) (\d+)\)", counter_section)

        return group_similar_primitives({k: int(v) for k, v in counts})
    else:
        return None

def extract_executable_name(content):
    return re.search(r"\*\*\*executable: (.+)", content).group(1)

def compile(file, system, vlimit, params):
    system_flag = {"bigloo": "-b",
                   "gambit": "-g"}[system]

    run_command = f"{RUN_SCRIPT} {system_flag} -V {vlimit} -P {file}"

    env = os.environ.copy()
    if 'wipgambitdir' in params: env["WIPGAMBITDIR"] = params["wipgambitdir"]

    verbose(run_command)
    output = subprocess.run(run_command, env=env, shell=True, capture_output=True).stdout.decode()

    primitive_count = extract_primitives_count(output)
    executable = extract_executable_name(output)

    return executable, primitive_count

def bench(executable, n, params):
    def get_numbers_on_line_with(text, marker):
        for line in text.splitlines():
            if marker in line:
                numbers = re.findall(r"[\d\.,]+", line)
                return [aton(n) for n in numbers]

        raise OSError(f"no line with '{marker}' in 'perf stat' output (is perf installed?)")


    command = f"perf stat -r {n} {executable}"
    verbose(command)
    output = subprocess.run(command, shell=True, capture_output=True).stderr.decode()
    verbose(output)

    elapsed, delta, pdelta = get_numbers_on_line_with(output, "seconds time elapsed")
    branch_misses, pbranch_misses, pbranch_misses_delta = get_numbers_on_line_with(output, "branch-misses:u")

    return {
        "time": f"{elapsed}s (± {pdelta}%)",
        "branch_misses": f"{branch_misses} (± {pbranch_misses_delta}%)"
        }

def add_percentages_to_results(bench_results, all_primitive_names):
    def make_getter(*path):
        def get(o, default):
            for k in path[:-1]: o = o[k]
            return read_number(o.get(path[-1], default))
        return get

    def make_setter(*path, ratio_only=False):
        def set(o, v, base):
            if ratio_only:
                result = f'{v / base:.3f}x'
            elif v == base:
                result = f'{format_number(v)}'
            else:
                result = f'{format_number(v)} ({v * 100 / base:.1f}%)'

            for k in path[:-1]: o = o[k]
            o[path[-1]] = result
        return set

    def make_getter_setter(*path):
        return make_getter(*path), make_setter(*path)

    def read_number(x):
        if isinstance(x, (int, float)):
            return x
        elif isinstance(x, str):
            return aton(re.search(r"[\d\.,]+", x).group())
        else:
            raise TypeError("cannot read {x} as number")

    def format_number(x):
        if isinstance(x, int):
            return str(x)
        else:
            return f'{x:.3f}'

    def add_percentage(bench_results, getter, setter):
        counts = [getter(b, 0) for b in bench_results]
        base = counts[0]

        for b, c in zip(bench_results, counts):
            setter(b, c, base)

    bench_results = copy.deepcopy(bench_results)

    add_percentage(bench_results, make_getter('time'), make_setter('time_ratio', ratio_only=True))
    add_percentage(bench_results, make_getter('branch_misses'), make_setter('branch_misses_ratio', ratio_only=True))

    for name in all_primitive_names:
        add_percentage(bench_results, *make_getter_setter('primitives', name))

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
    def priority(name):
        for i, k in enumerate(TITLES_ORDER):
            if k.lower() in name.lower():
                return (i, name)
        return (len(TITLES_ORDER), name)

    names = [n for n in bench_results[0].keys() if n != 'primitives']
    names.sort(key=priority)
    table = [[*(n.upper().replace('_', ' ') for n in names), *all_primitive_names]]

    for results in bench_results:
        row = [results[n] for n in names]
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

def main(*, file, system, vlimits, executions, **params):
    results = []

    for v in vlimits:
        executable, primitive_count = compile(file, system, v, params)
        results.append({'versions': v,
                        **bench(executable, executions, params),
                        'primitives': primitive_count})

    table = results_to_table(results)

    csvfile = params.get('csvfile')

    if csvfile:
        with open(csvfile, 'w') as f:
            writer = csv.writer(f)
            writer.writerows(table)
    else:
        print_table(table)

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Benchmark BBV")

    parser.add_argument('-v',
                        dest='verbose',
                        action='store_true',
                        help='verbose')
    parser.add_argument('-l',
                        dest="vlimits",
                        nargs="+",
                        default=(0, 1, 2, 3, 4, 5),
                        type=int,
                        help="BBV versions limits")
    parser.add_argument('-n',
                        dest="executions",
                        default=10,
                        type=int,
                        help="number of executions")
    parser.add_argument('-s',
                        dest='system',
                        default='gambit',
                        choices=['gambit', 'bigloo'],
                        help='system')
    parser.add_argument('-g',
                        dest='wipgambitdir',
                        help='Gambit root')
    parser.add_argument('-csv',
                        dest='csvfile',
                        metavar='FILENAME',
                        help='save results to a CSV')
    parser.add_argument('file',
                        help="Scheme program to benchmark")

    args = parser.parse_args()

    VERBOSE = args.verbose

    args.file = os.path.abspath(args.file)
    args.wipgambitdir = args.wipgambitdir and os.path.abspath(args.wipgambitdir)

    main(**vars(args))

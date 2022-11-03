#!/usr/bin/env python3

import argparse
import csv
import copy
import os
import re
import statistics
import subprocess

VERBOSE = False
RUN_SCRIPT = "../run"
DEFAULT_PRIMITIVE_COUNTER_MARKER = '***primitive-call-counter'

SIMILAR_PRIMITIVES = [['##fx+', '##fx+?'],
                      ['##fx-', '##fx-?'],
                      ['##fx*', '##fx*?']]

SIMILAR_PRIMITIVES_TABLE = {prim: simils for simils in SIMILAR_PRIMITIVES for prim in simils}

def verbose(*args, **kwargs):
    if VERBOSE: print(*args, **kwargs)

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

def bench_one(executable, params):
    command = f"time {executable}"
    if params.get("force_bash"): command = f'bash -c "{command}"'

    verbose(command)
    output = subprocess.run(command, shell=True, capture_output=True).stderr.decode()

    t = re.search(r"real\s+(\d+)m(\d+).(\d+)", output)

    if t:
        minutes, seconds, fraction = t.group(1, 2, 3)
        time = 60 * int(minutes)  + int(seconds) + int(fraction) / 10 ** len(fraction)

        return time
    else:
        raise OSError("unknown format for shell 'time' command (try using --force-bash?)")

def bench(executable, n, params):
    bench_one(executable, params) # warmup
    return [bench_one(executable, params) for _ in range(n)]

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

    def format_number(x):
        if isinstance(x, int):
            return str(x)
        else:
            return f'{x:.3f}'

    def add_percentage(values, getter, setter):
        raw_counts = [getter(b, 0) for b in values]
        if len(set(raw_counts)) == 1 or not raw_counts[0]:
            counts = [format_number(c) for c in raw_counts]
        else:
            base = raw_counts[0]
            counts = [format_number(base)]
            counts.extend(f'{format_number(c)} ({c * 100 / base:.1f}%)' for c in raw_counts[1:])

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
        name = results['versions']
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

def main(*, file, system, vlimits, executions, **params):
    results = []

    for v in vlimits:
        executable, primitive_count = compile(file, system, v, params)
        exec_times = bench(executable, executions, params)

        verbose("execution times:", *exec_times)

        results.append({'versions': v,
                        'time': statistics.mean(exec_times),
                        'variance': statistics.pvariance(exec_times),
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
                        nargs="*",
                        default=(0, 1, 2, 3, 4, 5),
                        type=int,
                        help="BBV versions limits")
    parser.add_argument('-n',
                        dest="executions",
                        default=10,
                        type=int,
                        help="number of executions")
    parser.add_argument("--force-bash",
                        dest="force_bash",
                        action="store_true",
                        help="force the use of the bash time command for benchamrks")
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

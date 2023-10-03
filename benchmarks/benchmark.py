#!/usr/bin/env python3

import argparse
import csv
import copy
import itertools
import locale
import os
import re
import shlex
import statistics
import subprocess

import matplotlib.pyplot as plt

locale.setlocale(locale.LC_ALL, 'en_US.UTF-8')

def aton(n):
    try:
        return locale.atoi(n)
    except ValueError:
        return locale.atof(n)

VERBOSE = False
COMPILE_SCRIPT = "../compile"

def verbose(*args, **kwargs):
    if VERBOSE: print(*args, **kwargs)


class BenchResults:
    def __init__(self, file, system, versions, merge_strategy, repeat, perf_output, primitives):
        self._perf_output = perf_output

        self.file = file
        self.system = system
        self.merge_strategy = merge_strategy
        self.versions = versions
        self.repeat = repeat
        self.primitives = primitives

        self.task_clock = self._get_value("task-clock")
        self.task_clock_unit = "ms"
        self.time_elapsed = self._get_value("seconds time elapsed")
        self.time_elapse_unit = "s"
        self.instructions = self._get_value("instructions")

    @property
    def title(self):
        return os.path.basename(self.file)

    def _get_numbers_on_line_with(self, marker):
        for line in self._perf_output.splitlines():
            if marker in line:
                numbers = re.findall(r"[\d\.,]+", line)
                return [aton(n) for n in numbers]

        raise OSError(f"no line with '{marker}' in 'perf stat' output (is perf installed?)")

    def _get_value(self, name):
        return self._get_numbers_on_line_with(name)[0]


class PrimitivesCount:
    DEFAULT_PRIMITIVE_COUNTER_MARKER = '***primitive-call-counter'
    SIMILAR_PRIMITIVES = [['##fx+', '##fx+?'],
                          ['##fx-', '##fx-?'],
                          ['##fx*', '##fx*?']]
    SIMILAR_PRIMITIVES_TABLE = {prim: simils for simils in SIMILAR_PRIMITIVES for prim in simils}

    def __new__(cls, compiler_output):
        self = super().__new__(cls)

        primitive_count = self._extract_primitives_count(compiler_output)

        if not primitive_count:
            return None

        self.raw_primitives = primitive_count
        self.primitives = self._group_similar_primitives(primitive_count)

        return self

    def get(self, name):
        return self.primitives.get(name, 0)

    @property
    def typechecks(self):
        return self.get('##fixnum?') + self.get('##flonum?')

    def _group_similar_primitives(self, primitives):
        grouped_primitives = {}
        seen = {}

        for k, v in primitives.items():
            group = self.SIMILAR_PRIMITIVES_TABLE.get(k)
            if group:
                group_name = ' & '.join(group)

                grouped_primitives.setdefault(group_name, 0)
                seen.setdefault(group_name, 0)

                grouped_primitives[group_name] += v
                seen[group_name] += 1

        relevant_grouped_primitives = {k: v for k, v in grouped_primitives.items() if seen[k] > 1}

        return {**primitives, **relevant_grouped_primitives}

    def _extract_primitives_count(self, compiler_output):
        if self.DEFAULT_PRIMITIVE_COUNTER_MARKER in compiler_output:
            counter_section = compiler_output.split(self.DEFAULT_PRIMITIVE_COUNTER_MARKER)[1]
            counts = re.findall("\(([^ ]+) (\d+)\)", counter_section)
            return {k: int(v) for k, v in counts}
        else:
            return None

def extract_executable_name(content):
    return re.search(r"\*\*\*executable: (.+)", content).group(1)

def run_command(command, timeout, env):
    if timeout:
        verbose(command, f"(with timeout: {timeout}s)")
    else:
        verbose(command)

    try:
        process = subprocess.Popen(shlex.split(command),
                                   env=env,
                                   stdout=subprocess.PIPE,
                                   stderr=subprocess.STDOUT)
        output, _ = process.communicate(timeout=timeout)
        return output.decode()
    finally:
        if process.poll() is None:
            verbose(f"killing process")
            process.kill()

def compile(file, system, vlimit, merge_strategy, params):
    system_flag = {"bigloo": "-b",
                   "gambit": "-g"}[system]

    primitive_count_flag = "-P" if params["primitive_count"] else ""

    merge_strategy_cmd = f"-M {merge_strategy}" if merge_strategy else ""

    command = f"{COMPILE_SCRIPT} {system_flag} -V {vlimit} {merge_strategy_cmd} {primitive_count_flag} {file}"

    env = os.environ.copy()
    timeout = params['compilation_timeout']
    if 'gambitdir' in params: env["GAMBITDIR"] = params["gambitdir"]

    output = run_command(command, timeout, env)

    verbose(output)

    primitive_count = PrimitivesCount(output)
    executable = extract_executable_name(output)

    return executable, primitive_count

def bench(executable, n, params):
    command = f"perf stat -r {n} {executable}"
    verbose(command)
    return subprocess.run(command, shell=True, capture_output=True).stderr.decode()

chart_selectors = {
    'time': (lambda r: r.task_clock, "Execution Time (ms)"),
    'typechecks': (lambda r: r.primitives.typechecks if r.primitives else 0, "Typechecks"),
    'machine_instructions': (lambda r: r.instructions, "Machine Instructions")
}

def write_chart_file(chartfile, results, selector, selector_name):
    # Group results by merge strategy
    sorted_results = sorted(results, key=lambda r: (r.merge_strategy or '', r.versions))
    bench_by_merge_strategy = itertools.groupby(sorted_results, key=lambda r: r.merge_strategy)
    bench_groups = [list(g) for _, g in bench_by_merge_strategy]
    n_merge_strategies = len(bench_groups)

    # Generate data for x and y axis
    versions = [str(v) for v in sorted(set(r.versions for r in results))]
    rows = [[selector(r) for r in g] for g in bench_groups]

    # Compute some dimension, purely aesthetic
    x = range(len(versions))
    bar_width = 0.8 / n_merge_strategies

    # Initialize the figure
    fig, axis = plt.subplots()
    axis.set_ylabel(selector_name)

    for benchs, offset in zip(bench_groups, range(-n_merge_strategies // 2 + 1, n_merge_strategies // 2 + 2)):
        axis.bar([pos + bar_width * offset for pos in x],
                 [selector(r) for r in benchs],
                 bar_width,
                 label=benchs[0].merge_strategy or '')

    axis.set_xlabel('Number of versions')
    axis.set_xticks(list(x))
    axis.set_xticklabels(versions)
    
    # Add legend only if there were multiple merge strategies
    if n_merge_strategies < 2 and results[0].merge_strategy:
        axis.set_title(f'{results[0].title}/{results[0].merge_strategy}')
    else:
        axis.set_title(results[0].title)

    if n_merge_strategies > 1:
        axis.legend()

    fig.tight_layout()

    plt.savefig(chartfile)

def main(*, file, system, vlimits, executions, **params):
    results = []

    for v in vlimits:
        for strat in (params.get("merge_strategies") or [None]):
            executable, primitive_count = compile(file, system, v, strat, params)
            perf_output = bench(executable, executions, params)
            verbose(perf_output)
            results.append(BenchResults(file,
                                        system,
                                        v,
                                        strat,
                                        executions,
                                        perf_output,
                                        primitive_count))

    csvfile = params.get('csvfile')
    chartfile = params.get('chartfile')

    if csvfile:
        raise NotImplementedError
    if chartfile:
        
        write_chart_file(chartfile, results, *chart_selectors[params['chart_y']])

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Benchmark BBV")

    parser.add_argument('-v',
                        dest='verbose',
                        action='store_true',
                        help='verbose')
    parser.add_argument('-l',
                        dest="vlimits",
                        metavar="LIMIT",
                        nargs="+",
                        default=(0, 1, 2, 3, 4, 5),
                        type=int,
                        help="BBV versions limits")
    parser.add_argument('-n',
                        dest="executions",
                        default=10,
                        type=int,
                        help="number of executions")
    parser.add_argument('-t',
                        dest="compilation_timeout",
                        metavar='TIMEOUT',
                        type=float,
                        help="compilation timeout (in secondes)")
    parser.add_argument('-s',
                        dest='system',
                        default='gambit',
                        choices=['gambit', 'bigloo'],
                        help='system')
    parser.add_argument('-g',
                        dest='gambitdir',
                        help='Gambit root')
    parser.add_argument('-p',
                        dest='primitive_count',
                        action='store_true',
                        help='count primitive calls')
    parser.add_argument('-m',
                        metavar="STRATEGY",
                        dest='merge_strategies',
                        nargs="*",
                        help='merge strategies')
    parser.add_argument('-csv',
                        dest='csvfile',
                        metavar='FILENAME',
                        help='save results to a CSV')

    parser.add_argument('-chart',
                        dest='chartfile',
                        metavar='FILENAME',
                        help='generate a bar chart of the result')

    parser.add_argument('-charty',
                        dest='chart_y',
                        metavar='Y',
                        choices=list(chart_selectors.keys()),
                        default=next(iter(chart_selectors)),
                        help='bar chart y axis')

    parser.add_argument('file',
                        help="Scheme program to benchmark")

    args = parser.parse_args()

    VERBOSE = args.verbose

    args.file = os.path.abspath(args.file)
    args.gambitdir = args.gambitdir and os.path.abspath(args.gambitdir)

    main(**vars(args))

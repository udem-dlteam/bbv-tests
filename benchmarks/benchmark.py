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
import matplotlib as mpl

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

    def __iter__(self):
        yield from self.primitives.keys()

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

def merge_strategy_grouper(results, params):
    sorted_results = sorted(results, key=lambda r: (r.merge_strategy or '', r.versions))
    bench_by_merge_strategy = itertools.groupby(sorted_results, key=lambda r: r.merge_strategy)
    return [list(g) for _, g in bench_by_merge_strategy]

def name_by_merge_strategy(group):
    return group[0].merge_strategy or ''

def primitives_grouper(results, params):
    def prim_count(result, p):
        return result.primitives.get(p) if result.primitives else 0

    results = merge_strategy_grouper(results, params)[0] # ignore merge strategy by taking the first one
    new_results = []

    tracked_primitives = list(params["chart_params"])
    
    if not tracked_primitives:
        tracked_primitives = set()
        for result in results:
            tracked_primitives.update(result.primitives or ())
        tracked_primitives = list(tracked_primitives)

    tracked_primitives.sort(key=lambda p: tuple(prim_count(r, p) for r in results), reverse=True)

    max_prim_count = max(prim_count(r, p) for r in results for p in tracked_primitives)

    tracked_primitives = [p for p in tracked_primitives if max(prim_count(r, p) for r in results) > max_prim_count / 1000]

    for prim in tracked_primitives:
        row = []
        for result in results:
            bench_copy = copy.copy(result)
            bench_copy.__selector_value = bench_copy.primitives.get(prim) if bench_copy.primitives else 0
            bench_copy.__selector_name = prim
            row.append(bench_copy)
        new_results.append(row)
    return new_results
        

chart_modes = {
    'time': (lambda r: r.task_clock, # selector for the y axis
             "Execution Time (ms)",  # y axis label
             name_by_merge_strategy, # function that names each set benchmark result in the legen
             merge_strategy_grouper, # function that groups the results for multi-bar charts
             False),                 # does the mode requires primitive count at compile time
    'typechecks': (lambda r: r.primitives.typechecks if r.primitives else 0,
                   "Typechecks",
                   name_by_merge_strategy,
                   merge_strategy_grouper,
                   False),
    'machine_instructions': (lambda r: r.instructions,
                             "Machine Instructions",
                             name_by_merge_strategy,
                             merge_strategy_grouper,
                             False),
    'primitives': (lambda r: r.__selector_value,
                 "Primitive Calls",
                 lambda group: group[0].__selector_name,
                 primitives_grouper,
                 True),
}

def write_chart_file(chartfile, results, params):
    selector, yname, group_namer, grouper, *_ = chart_modes[params['chart_mode']]

    # Group results by merge strategy
    bench_groups = grouper(results, params)
    n_groups = len(bench_groups)

    # Generate data for x and y axis
    versions = [str(v) for v in sorted(set(r.versions for r in results))]

    # Compute some dimension, purely aesthetic
    x = range(len(versions))
    bar_width = 0.8 / n_groups

    # Initialize the figure
    fig, axis = plt.subplots()
    axis.set_ylabel(yname)

    colors = mpl.colormaps['viridis'].resampled(n_groups).colors

    for benchs, offset, color in zip(bench_groups, range(-n_groups // 2 + 1, n_groups // 2 + 2), colors):
        axis.bar([pos + bar_width * offset for pos in x],
                 [selector(r) for r in benchs],
                 bar_width,
                 color=color,
                 label=group_namer(benchs))

    axis.set_xlabel('Number of versions')
    axis.set_xticks(list(x))
    axis.set_xticklabels(versions)
    
    axis.set_title(results[0].title)

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
        write_chart_file(chartfile, results, params)


def get_chart_mode(args):
    mode = args._chart_params[0]

    if mode not in chart_modes:
        raise ValueError(f"chart-params first argument must be in {', '.join(chart_modes)}")

    return mode

def get_chart_params(args):
    return args._chart_params[1:]

def get_chart_mode_needs_primitives(args):
    return chart_modes[get_chart_mode(args)][4]

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

    parser.add_argument('-chart-params',
                        dest='_chart_params',
                        nargs='+',
                        metavar='PARAM',
                        default=(next(iter(chart_modes)),),
                        help=f'params for the bar chart, available modes: {", ".join(chart_modes)}')

    parser.add_argument('file',
                        help="Scheme program to benchmark")

    args = parser.parse_args()

    VERBOSE = args.verbose

    args.file = os.path.abspath(args.file)
    args.gambitdir = args.gambitdir and os.path.abspath(args.gambitdir)

    args.chart_mode = get_chart_mode(args)
    args.chart_params = get_chart_params(args)
    args.primitive_count = args.primitive_count or get_chart_mode_needs_primitives(args)

    main(**vars(args))

#!/usr/bin/env python3

import argparse
import ast
import datetime
import locale
import logging
import math
import os
import pathlib
import platform
import re
import shlex
import subprocess
import time

try:
    import distro
except ImportError:
    distro = None

import matplotlib.pyplot as plt
import matplotlib as mpl

from pony.orm import *

import psutil

logger = logging.getLogger(__name__)

##############################################################################
## Database
##############################################################################

db = Database()

# Some monkey patching
if not hasattr(db.Entity, "get_or_create"):
    @classmethod
    def get_or_create(cls, **kwargs):
        r = cls.get(**kwargs)
        if r is None:
            return cls(**kwargs), True
        else:
            return r, False
    db.Entity.get_or_create=get_or_create
    del get_or_create


class Compiler(db.Entity):
    name = Required(str)
    commit_sha = Required(str)
    commit_description = Required(str)
    commit_author = Required(str)
    commit_timestamp = Required(int)
    runs = Set('Run')

    @classmethod
    def get_or_create_compiler(cls, compilerdir):
        # Define the format for the commit details we want
        # %H: commit hash, %an: author name, %s: subject, %ct: committer date (Unix timestamp)
        format_str = "%H%n%an%n%s%n%ct"

        output = subprocess.check_output(['git', 'show', '-s', f'--format={format_str}'],
                                         cwd=compilerdir, universal_newlines=True).strip()

        name = compilerdir.name
        sha, author, description, timestamp = output.splitlines()

        logger.debug(f"current compiler is: {name}, {sha}, {author}, {description}, {timestamp}")

        return Compiler.get_or_create(name=name,
                                      commit_sha=sha,
                                      commit_description=description,
                                      commit_author=author,
                                      commit_timestamp=timestamp)

class System(db.Entity):
    name = Required(str)
    os = Required(str)
    distribution = Optional(str)
    ram = Required(str)
    cpu = Required(str)
    runs = Set('Run')

    @staticmethod
    def get_system_data():
        name = platform.node()
        os_name = platform.system()

        if os_name != "Linux":
            raise ValueError('{os_name} not supported, maybe use Linux?')

        if distro:
            distribution = f'{distro.name(pretty=True)} ({distro.lsb_release_info()["codename"]})'
        else:
            distribution = None

        ram = f'{math.ceil(psutil.virtual_memory().total / (1024 ** 3))} GB'

        with open('/proc/cpuinfo') as f:
            # Get the first line that starts with 'model name' or 'Processor'.
            match = re.search(r'model name\s*:\s*(.*)', f.read())
            if match:
                cpu = match.group(1).strip()
            else:
                raise ValueError('could not identify your cpu')

        logger.debug(f"current system is: {os_name}, {distribution}, {ram}, {cpu}")

        return dict(name=name, os=os_name, distribution=distribution, ram=ram, cpu=cpu)

    @classmethod
    def get_current_system(cls):
        return cls.get(**cls.get_system_data())

    @classmethod
    def get_or_create_current_system(cls):
        return cls.get_or_create(**cls.get_system_data())

class Benchmark(db.Entity):
    name = Required(str)
    path = Required(str)
    content = Required(str)
    timestamp = Required(int)
    runs = Set('Run')

class PrimitiveCount(db.Entity):
    name = Required(str)
    count = Required(int, size=64)
    run = Required('Run')

class PerfEvent(db.Entity):
    event = Required(str)
    value = Required(int, size=64)
    run = Required('Run')

class Run(db.Entity):
    benchmark = Required('Benchmark', reverse='runs')
    system = Required('System', reverse='runs')
    compiler = Required('Compiler', reverse='runs')
    version_limit = Required(int)
    repetitions = Required(int)
    merge_strategy = Required(str)
    primitives = Set('PrimitiveCount', reverse='run')
    perf_events = Set('PerfEvent', reverse='run')
    timestamp = Required(int, default=lambda: int(time.time()))


db.bind(provider='sqlite', filename='benchmarks.db', create_db=True)
db.generate_mapping(create_tables=True)

##############################################################################
# Utils
##############################################################################

def run_command(command, timeout, env):
    logger.info(command)

    if timeout is not None:
        logger.info(f"(with timeout: {timeout}s)")

    try:
        process = subprocess.Popen(shlex.split(command),
                                   env=env,
                                   stdout=subprocess.PIPE,
                                   stderr=subprocess.STDOUT)
        output, _ = process.communicate(timeout=timeout)
        logger.debug(output)
        return output.decode()
    finally:
        if process.poll() is None:
            logger.info("killing process")
            process.kill()

##############################################################################
# Data parsers
##############################################################################

locale.setlocale(locale.LC_ALL, 'en_US.UTF-8')

class PrimitivesCountParser:
    DEFAULT_PRIMITIVE_COUNTER_MARKER = '***primitive-call-counter'
    SIMILAR_PRIMITIVES = [['##fx+', '##fx+?'],
                          ['##fx-', '##fx-?'],
                          ['##fx*', '##fx*?']]
    SIMILAR_PRIMITIVES_TABLE = {prim: simils for simils in SIMILAR_PRIMITIVES for prim in simils}

    typechecks = ("##fixnum?", '##flonum?', "##vector?", "##pair?", "##box?", "##procedure?",
                  "##bignum?", "##ratnum?", "##boolean?", "##string?", "##char?",
                  "##bytevector?", "##u8vector?", "##u16vector?", "##u32vector?",
                  "##u64vector?", "##s8vector?", "##s16vector?", "##s32vector?",
                  "##s64vector?", "##f8vector?", "##f16vector?", "##f32vector?",
                  "##f64vector?", "##null?")

    def __new__(cls, compiler_output):
        self = super().__new__(cls)

        if cls.DEFAULT_PRIMITIVE_COUNTER_MARKER not in compiler_output:
            logger.debug(compiler_output)
            raise ValueError(f"{cls.DEFAULT_PRIMITIVE_COUNTER_MARKER} not found in compiler output")

        counter_section = compiler_output.split(self.DEFAULT_PRIMITIVE_COUNTER_MARKER)[1]
        counts = re.findall("\(([^ ]+) (\d+)\)", counter_section)
        self.primitives = {k: int(v) for k, v in counts}
        return self

    def __getitem__(self, name):
        return self.primitives.get(name, 0)

    def keys(self):
        return self.primitives.keys()

    def items(self):
        return self.primitives.items()

    def values(self):
        return self.primitives.values()

    def __iter__(self):
        yield from self.primitives.keys()

    @classmethod
    def is_typecheck(cls, primitive):
        return primitive in cls.typechecks


class BenchResultParser:
    time_event = 'task-clock'
    event_names = [
        time_event,
        "cycles",
        "instructions",
        "branches",
        "branch-misses",
        "cache-references",
        "cache-misses",
        "mem-loads",
        "mem-stores",
        "page-faults",
        "minor-faults",
        "major-faults",
        "context-switches",
        "cpu-migrations",
        "stalled-cycles-frontend",
        "stalled-cycles-backend",
        # CPU Cache events
        "L1-dcache-loads",
        "L1-dcache-load-misses",
        "L1-dcache-stores",
        "L1-dcache-store-misses",
        "L1-icache-loads",
        "L1-icache-load-misses",
        # Branch Prediction
        "branch-load-misses",
        "branch-loads",
    ]

    def __init__(self, perf_output):
        if 'Performance counter stats' not in perf_output:
            raise ValueError(f"wrong 'perf stat' output (is perf installed?)")

        self.events = {}
        
        for e in self.event_names:
            if (result := self._get_value(perf_output, e)) is not None:
                self.events[e] = result

    def keys(self):
        return self.events.keys()

    def values(self):
        return self.events.values()

    def items(self):
        return self.events.items()

    def update(self, other):
        self.events.update(other.events)

    def __getitem__(self, event_name):
        return self.events[event_name]

    def __contains__(self, event_name):
        return event_name in self.events

    @staticmethod
    def _string_to_number(n):
        try:
            return locale.atoi(n)
        except ValueError:
            return locale.atof(n)

    @classmethod
    def _get_numbers_on_line_with(cls, perf_output, marker):
        for line in perf_output.splitlines():
            if marker in line:
                numbers = re.findall(r"[\d\.,]+", line)
                logger.debug(f"found perf stat event {marker} ({numbers})")
                return [cls._string_to_number(n) for n in numbers]

        logger.debug(f"could not find perf stat event {marker}")
        return None

    @classmethod
    def _get_value(cls, perf_output, name):
        results = cls._get_numbers_on_line_with(perf_output, name)
        return results[0] if results else None


##############################################################################
# Benchmark execution
##############################################################################

COMPILE_SCRIPT = "../compile"


def extract_executable_from_compiler_output(content):
    return re.search(r"\*\*\*executable: (.+)", content).group(1)


def compile(compilerdir, file, vlimit, merge_strategy, timeout=None):

    command = f"{COMPILE_SCRIPT} -g -V {vlimit} -M {merge_strategy} -P {file}"

    env = os.environ.copy()
    env["GAMBITDIR"] = compilerdir

    output = run_command(command, timeout, env)

    executable = extract_executable_from_compiler_output(output)

    logger.info(f"executable created at: {executable}")

    primitive_count = PrimitivesCountParser(output)

    if not primitive_count:
        logger.warning("Failed to parse primitive count")
    else:
        logger.debug(f"Primitive count: {dict(primitive_count)}")

    return executable, primitive_count


def run_benchmark(executable, repetitions):
    # Run program to measure time only
    time_command = f"perf stat -e {BenchResultParser.time_event} -r {repetitions} {executable}"
    logger.info(time_command)
    time_output = subprocess.run(time_command, shell=True, capture_output=True).stderr.decode()
    logger.debug(time_output)

    # Run program with all perf stat events on
    other_events = ' '.join(f"-e {e}" for e in BenchResultParser.event_names)
    other_command = f"perf stat {other_events} -r {repetitions} {executable}"
    logger.info(other_command)
    other_output = subprocess.run(other_command, shell=True, capture_output=True).stderr.decode()
    logger.debug(other_output)

    # parse and join outputs
    time_parser = BenchResultParser(time_output)
    other_parser = BenchResultParser(other_output)

    other_parser.update(time_parser)

    for event in BenchResultParser.event_names:
        if event not in other_parser:
            logger.warning(f"Could not find perf stat event {repr(event)} when running {executable}")

    return other_parser

@db_session
def run_and_save_benchmark(compilerdir, file, version_limits, repetitions, merge_strategy, force_execution=False, timeout=None):
    system, _ = System.get_or_create_current_system()
    compiler, _ = Compiler.get_or_create_compiler(compilerdir)

    with open(file) as f:
        name = os.path.splitext(os.path.basename(file))[0]
        timestamp = int(os.path.getmtime(file))
        benchmark, _ = Benchmark.get_or_create(name=name, path=str(file), content=f.read(), timestamp=timestamp)

    for v in version_limits:
        logger.info(f'- benchmark: {file}\n'
                    f'- merge strategy:{merge_strategy}\n'
                    f'- version limit:{v}')

        if not force_execution:
            existing_run = Run.get(benchmark=benchmark,
                                   system=system,
                                   compiler=compiler,
                                   version_limit=v,
                                   repetitions=repetitions,
                                   merge_strategy=merge_strategy)
            if existing_run:
                logger.info('benchmark has an existing run, skip it')
                continue
            else:
                logger.info('no run exists for this benchmark, execute it')

        executable, primitive_count = compile(compilerdir, file, v, merge_strategy, timeout)
        result = run_benchmark(executable, repetitions)

        run = Run(benchmark=benchmark,
                  system=system,
                  compiler=compiler,
                  version_limit=v,
                  repetitions=repetitions,
                  merge_strategy=merge_strategy)

        for event, value in result.items():
            PerfEvent(event=event, value=int(value), run=run)

        for prim, count in primitive_count.items():
            PrimitiveCount(name=prim, count=count, run=run)

##############################################################################
# Chart generation
##############################################################################

@db_session
def plot_benchmarks(benchmark):
    compiler_name = "gambit"

    system = System.get_current_system()
    compiler = select(c for c in Compiler if c.name == compiler_name).order_by(desc(Compiler.commit_timestamp)).first()

    runs = select(r for r in Run if r.system == system and r.compiler ==
                  compiler and r.benchmark.path.endswith("/" + benchmark))

    for run in runs:
        print(run.benchmark.path)


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

    # Get the directory name from the file path
    directory = os.path.dirname(chartfile)

    # If the directory does not exist, create it
    if directory and not os.path.exists(directory):
        os.makedirs(directory)

    plt.savefig(chartfile)



def get_chart_mode(args):
    mode = args._chart_params[0]

    if mode not in chart_modes:
        raise ValueError(f"chart-params first argument must be in {', '.join(chart_modes)}")

    return mode

def get_chart_params(args):
    return args._chart_params[1:]

def get_chart_mode_needs_primitives(args):
    return chart_modes[get_chart_mode(args)][4]


logger = logging.getLogger(__name__)

if __name__ == "__main__":
    # helpers for path parsing
    def directory_path(value):
        path = pathlib.Path(value)
        if path.is_dir():
            return path
        else:
            raise NotADirectoryError(f"{value} is not a valid directory")


    def file_path(value):
        path = pathlib.Path(value)
        if path.is_file():
            return path
        else:
            raise FileNotFoundError(f"{value} is not a valid file")

    class KeyValueAction(argparse.Action):
        def __call__(self, parser, namespace, values, option_string=None):
            params = {}
            for item in values:
                try:
                    key, value = item.split("=")
                except ValueError as e:
                    raise argparse.ArgumentError(self, f"requires format NAME=VALUE, got {repr(item)}")

                params[key] = value.split(',') if ',' in value else value

            setattr(namespace, self.dest, params)

    # Main parser
    parser = argparse.ArgumentParser(description="Benchmark BBV for Gambit")
    subparsers = parser.add_subparsers(dest='command')

    parser.add_argument('-d', '--debug',
                        help="Print lots of debugging statements",
                        action="store_const",
                        dest="loglevel",
                        const=logging.DEBUG,
                        default=logging.WARNING,)

    parser.add_argument('-v', '--verbose',
                        help="Be verbose",
                        action="store_const",
                        dest="loglevel",
                        const=logging.INFO,)

    # Parser for running benchmarks
    benchmark_parser = subparsers.add_parser('benchmark', help='Run benchmark and store results')

    benchmark_parser.add_argument('file', type=file_path)

    benchmark_parser.add_argument('-g', '--gambit-dir',
                                  dest='compilerdir',
                                  type=directory_path,
                                  metavar='PATH',
                                  required=True,
                                  help='Gambit root')

    benchmark_parser.add_argument('-l', '--limit',
                                  dest="version_limits",
                                  metavar="LIMIT",
                                  nargs="+",
                                  default=(0, 1, 2, 3, 4, 5),
                                  type=int,
                                  help="BBV versions limits")

    benchmark_parser.add_argument('-r', '--repetitions',
                                  dest="repetitions",
                                  metavar='N',
                                  default=10,
                                  type=int,
                                  help="Number of repetition when executing")

    benchmark_parser.add_argument('-t', '--timeout',
                                  dest="timeout",
                                  metavar='T',
                                  type=float,
                                  help="Compilation timeout (in secondes)")

    benchmark_parser.add_argument('-m', '--merge-strategy',
                                  metavar="STRATEGY",
                                  dest='merge_strategy',
                                  default='linear',
                                  help='BBV merge strategies')

    benchmark_parser.add_argument('-f', '--force-execution',
                                  dest='force_execution',
                                  action='store_true',
                                  help='rerun benchmark even if results already exist')

    # Parser for running benchmarks
    plot_parser = subparsers.add_parser('plot', help='Plot benchmarks')

    plot_parser.add_argument('-b', '--benchmark',
                             required=True,
                             dest="benchmark",
                             help="benchmark filename")

    args = parser.parse_args()

    # Set logger level
    logger.setLevel(args.loglevel)
    logger.addHandler(logging.StreamHandler())
    
    logger.debug(args)

    if args.command == 'benchmark':
        run_and_save_benchmark(compilerdir=args.compilerdir.resolve(),
                               file=args.file.resolve(),
                               version_limits=args.version_limits,
                               repetitions=args.repetitions,
                               merge_strategy=args.merge_strategy,
                               force_execution=args.force_execution,
                               timeout=args.timeout)
    elif args.command == 'plot':
        plot_benchmarks(benchmark=args.benchmark)

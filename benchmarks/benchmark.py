#!/usr/bin/env python3

import argparse
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

class System(db.Entity):
    compiler = Required(str)
    compiler_commit = Required(str)
    benchmark_commit = Required(str)
    os = Required(str)
    distribution = Optional(str)
    ram = Required(str)
    cpu = Required(str)
    runs = Set('Run')

    @staticmethod
    def get_commit(dirpath):
        return subprocess.check_output(['git', 'rev-parse', 'HEAD'], cwd=dirpath,
                                       universal_newlines=True).strip()

    @classmethod
    def get_current_system(cls, compilerdir):
        # get compiler from 
        compiler = compilerdir.name
        system = platform.system()

        if system != "Linux":
            raise ValueError('{system} not supported, maybe use Linux?')

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

        compiler_commit = cls.get_commit(compilerdir)
        benchmark_commit = cls.get_commit(os.path.curdir)

        return cls.get_or_create(
            compiler=compiler,
            compiler_commit=compiler_commit,
            benchmark_commit=benchmark_commit,
            os=system,
            distribution=distribution,
            ram=ram,
            cpu=cpu)

class Benchmark(db.Entity):
    path = Required(str)
    content = Required(str)
    runs = Set('Run')

    @property
    def filename(self):
        return os.path.basename(self.path)

class PrimitiveCount(db.Entity):
    name = Required(str)
    count = Required(int, size=64)
    run = Required('Run')

class Run(db.Entity):
    benchmark = Required('Benchmark')
    system = Required('System')
    version_limit = Required(int)
    repetitions = Required(int)
    merge_strategy = Required(str)
    primitives = Set('PrimitiveCount')
    machine_instructions = Required(int, size=64)
    time = Required(float)
    timestamp = Required(datetime.datetime, default=datetime.datetime.now)


db.bind(provider='sqlite', filename='benchmarks.db', create_db=True)
db.generate_mapping(create_tables=True)

##############################################################################
# Utils
##############################################################################

def run_command(command, timeout, env):
    logger.info(command)
    logger.debug(env)

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


class BenchResultsParser:
    def __init__(self, perf_output):
        self.time = self._get_value(perf_output, "task-clock")
        self.machine_instructions = self._get_value(perf_output, "instructions")

    @staticmethod
    def string_to_number(n):
        try:
            return locale.atoi(n)
        except ValueError:
            return locale.atof(n)

    @classmethod
    def _get_numbers_on_line_with(cls, perf_output, marker):
        for line in perf_output.splitlines():
            if marker in line:
                numbers = re.findall(r"[\d\.,]+", line)
                return [cls.string_to_number(n) for n in numbers]

        raise ValueError(f"no line with '{marker}' in 'perf stat' output (is perf installed?)")

    @classmethod
    def _get_value(cls, perf_output, name):
        return cls._get_numbers_on_line_with(perf_output, name)[0]


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
    command = f"perf stat -r {repetitions} {executable}"
    logger.info(command)
    output = subprocess.run(command, shell=True, capture_output=True).stderr.decode()
    logger.debug(output)
    return BenchResultsParser(output)

@db_session
def run_and_save_benchmark(compilerdir, file, vlimits, repetitions, merge_strategy, timeout=None):
    system, _ = System.get_current_system(compilerdir)

    for v in vlimits:
        executable, primitive_count = compile(compilerdir, file, v, merge_strategy, timeout)
        result = run_benchmark(executable, repetitions)

        with open(file) as f:
            benchmark, _ = Benchmark.get_or_create(path=str(file), content=f.read())

        run = Run(
            benchmark=benchmark,
            system = system,
            version_limit=v,
            repetitions=repetitions,
            merge_strategy=merge_strategy,
            machine_instructions=result.machine_instructions,
            time=result.time)

        for prim, count in primitive_count.items():
            PrimitiveCount(name=prim, count=count, run=run)



##############################################################################
# Chart generation
##############################################################################

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

    args = parser.parse_args()

    # Set logger level
    logger.setLevel(args.loglevel)
    console = logging.StreamHandler()
    logger.addHandler(console)
    
    logger.debug(args)

    if args.command == 'benchmark':
        run_and_save_benchmark(args.compilerdir.resolve(),
                               args.file.resolve(),
                               args.version_limits,
                               args.repetitions,
                               args.merge_strategy,
                               args.timeout)

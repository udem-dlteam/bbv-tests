#!/usr/bin/env python3

import argparse
import ast
import collections
import datetime
import itertools
import locale
import logging
import math
import operator
import os
import pathlib
import platform
import re
import shlex
import statistics
import string
import subprocess
import time

try:
    import distro
except ImportError:
    distro = None

import matplotlib.pyplot as plt
import matplotlib as mpl

import numpy as np

import pandas as pd

from pony.orm import *

import psutil

import seaborn as sns

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
    def get_current_system_name():
        return platform.node()

    @classmethod
    def get_system_data(cls):
        name = cls.get_current_system_name()
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
    value = Required(int, size=64)
    run = Required('Run')

    @property
    def is_typecheck(self):
        typechecks = ("##fixnum?", '##flonum?', "##vector?", "##pair?", "##box?", "##procedure?",
                "##bignum?", "##ratnum?", "##boolean?", "##string?", "##char?",
                "##bytevector?", "##u8vector?", "##u16vector?", "##u32vector?",
                "##u64vector?", "##s8vector?", "##s16vector?", "##s32vector?",
                "##s64vector?", "##f8vector?", "##f16vector?", "##f32vector?",
                "##f64vector?", "##null?")
        return self.name in typechecks

class PerfEvent(db.Entity):
    event = Required(str)
    value = Required(int, size=64)
    run = Required('Run')

class OtherMeasure(db.Entity):
    name = Required(str)
    value = Required(int, size=64)
    run = Required('Run')

class Run(db.Entity):
    benchmark = Required('Benchmark', reverse='runs')
    system = Required('System', reverse='runs')
    compiler = Required('Compiler', reverse='runs')
    version_limit = Required(int)
    merge_strategy = Required(str)
    primitives = Set('PrimitiveCount', reverse='run')
    perf_events = Set('PerfEvent', reverse='run')
    other_measures = Set('OtherMeasure', reverse='run')
    arguments = Required(str)
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
    SIZE_IN_GVM_INSTRUCTIONS = "total-gvm-instructions"
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

        self.size_in_gvm_instructions = self.primitives.pop(self.SIZE_IN_GVM_INSTRUCTIONS)

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
        if n is None:
            return None

        try:
            return locale.atoi(n)
        except ValueError:
            return locale.atof(n)

    @classmethod
    def _get_value(cls, perf_output, name):
        for line in perf_output.splitlines():
            if name in line:
                # Capture event value (first number) and optional variance (number preceded by "( +-")
                match = re.match(r"\s*([\d\.,]+)[^\(\n]+(?:\(\s*\+-\s+([\d\.,]+)\s*%)?", line)
                if not match:
                    break
                
                value, variance = match.groups()
                logger.debug(f"found perf stat event {name} ({value} +- {variance})")
                return (cls._string_to_number(value), cls._string_to_number(variance))

        logger.debug(f"could not find perf stat event {name}")
        return None


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


def run_benchmark(executable, arguments):
    # Run program to measure time only
    time_command = f"perf stat -e {BenchResultParser.time_event} {executable} {arguments}"
    logger.info(time_command)
    time_output = subprocess.run(time_command, shell=True, capture_output=True).stderr.decode()
    logger.debug(time_output)

    # Run program with all perf stat events on
    other_events = ' '.join(f"-e {e}" for e in BenchResultParser.event_names)
    other_command = f"perf stat {other_events} {executable} {arguments}"
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

default_arguments = "repeat: 20"

benchmark_args = {
    "ack": "repeat: 50 m: 3 n: 9",
    "fib": "repeat: 5 n: 39",
    "fibfp": "repeat: 2 n: 39.0",
    "tak": "repeat: 10000 x: 18 y: 12 z: 6",
    "takl": "repeat: 2000 x: 18 y: 12 z: 6",
    "diviter": "repeat: 2000000 ",
    "divrec": "repeat: 2000000 ",
    "array1": "repeat: 5 n: 200000",
    "browse": "repeat: 2000 ",
    "mazefun": "repeat: 5000 n: 11 m: 11",
    "nqueens": "repeat: 2 n: 13",
    "puzzle": "repeat: 500 n: 511",
    "quicksort": "repeat: 1000 ",
    "sum": "repeat: 200000 n: 10000",
    "sumfp": "repeat: 500 n: 1e6",
    "triangl": "repeat: 50 i: 22 depth: 1",
    "almabench": "repeat: 1 K: 36525",
}

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
                                   merge_strategy=merge_strategy)
            if existing_run:
                logger.info('benchmark has an existing run, skip it')
                continue
            else:
                logger.info('no run exists for this benchmark, execute it')

        base_arguments = benchmark_args.get(benchmark.name)
        if base_arguments is None:
            logger.warn(f"no CLI argument for {repr(benchmark.name)}")
            base_arguments = default_arguments

        run = Run(benchmark=benchmark,
                  system=system,
                  compiler=compiler,
                  version_limit=v,
                  merge_strategy=merge_strategy,
                  arguments=base_arguments)

        executable, primitive_count = compile(compilerdir, file, v, merge_strategy, timeout)

        align_stack_step = 3
        for rep in range(repetitions):
            arguments = f"{base_arguments} align-stack: {rep * align_stack_step}"
            for event, (value, variance) in run_benchmark(executable, arguments).items():
                PerfEvent(event=event, value=int(value), run=run)

        typechecks = 0

        for prim, value in primitive_count.items():
            p = PrimitiveCount(name=prim, value=value, run=run)
            typechecks += p.is_typecheck * value

        logger.debug(f"number of typechecks: {typechecks}")
        OtherMeasure(name="typechecks", value=typechecks, run=run)

        total_primitives = sum(primitive_count.values())
        logger.debug(f"executed primitives: {total_primitives}")
        OtherMeasure(name='primitives', value=total_primitives, run=run)

        logger.debug(f"gvm instruction size: {primitive_count.size_in_gvm_instructions}")
        OtherMeasure(name='gvm-size', value=primitive_count.size_in_gvm_instructions, run=run)

        executable_size = os.path.getsize(executable)
        logger.debug(f"executable size: {executable_size}")
        OtherMeasure(name='executable-size', value=executable_size, run=run)

##############################################################################
# Chart generation
##############################################################################

class ChartValue:
    def __init__(self, value, stdev=0):
        self.value = value
        self.stdev = stdev

    def scale(self, ratio):
        return ChartValue(self.name, self.value * ratio, self.stdev * ratio)

    @classmethod
    @property
    def zero(cls):
        return cls(0)

    def __repr__(self):
        return f"ChartValue(value={self.value}, stdev={self.stdev})"

    def __truediv__(self, other):
        if not isinstance(other, ChartValue):
            return NotImplemented

        if other.value == 0:
            # Use pseudo-ratio
            return ChartValue(self.value + 1, self.stdev) / ChartValue(1, other.stdev)

        new_value = self.value / other.value

        # Convert standard deviation back to variance for calculation
        variance_self = self.stdev ** 2
        variance_other = other.stdev ** 2

        # Calculate the variance of the quotient
        new_variance = ((variance_self / (other.value ** 2)) +
                        ((self.value ** 2) * variance_other) / (other.value ** 4))

        return ChartValue(new_value, math.sqrt(new_variance))


def sanitize_filename(filename, valid_chars="-_.()" + string.ascii_letters + string.digits):
    return ''.join(c for c in filename if c in valid_chars)


def ensure_directory_exists(filepath):
    dir_name = os.path.dirname(filepath)
    if not os.path.exists(dir_name):
        os.makedirs(dir_name)


def choose_path(base, output, system_name, compiler_name,
                valid_chars="-_.()" + string.ascii_letters + string.digits):
    path = pathlib.Path(output or '.').resolve()
    suffix = path.suffix

    if not suffix:
        # No extension means the output is a folder where to output the plot
        logger.debug(f"output into folder {path}")

        # build default filename
        filename = f"{base}_{compiler_name}_{system_name}.png"

        # sanitize filename
        filename = sanitize_filename(filename)

        path = path / filename
        logger.info(f"output to file {path}")
        return path
    elif suffix == ".png":
        logger.info(f"output to file {path}")
        return path
    else:
        raise ValueError(f"output must be a folder of .png target, got {output}")


def choose_barchart_output_path(output, system_name, compiler_name, benchmark, perf_event_names, primitive_names):
    primitive_segment = f"_{len(primitive_names)}primitives" if primitive_names else ""
    base = f"{benchmark}_{'_'.join(sorted(perf_event_names))}{primitive_segment}"
    return choose_path(base, output, system_name, compiler_name)


def select_primitives_names(runs, primitive_names_or_amount):
    if isinstance(primitive_names_or_amount, int):
        # Recover the most common primitives
        primitives = select(prim for prim in PrimitiveCount if prim.run in runs).order_by(desc(PrimitiveCount.value))
        return list(select(prim.name for prim in primitives if prim.name !=
                      "##identity").distinct()[:primitive_names_or_amount])
    else:
        return primitive_names_or_amount


def get_system_from_name_or_default(system_name=None):
    system = System.get_current_system() if system_name is None else System.get(name=system_name)

    if not system:
        raise ValueError(f"could not find system {repr(system_name or System.get_current_system_name())}")

    return system


def get_compiler_from_name(compiler_name):
    compiler = select(c for c in Compiler if c.name == compiler_name).order_by(desc(Compiler.commit_timestamp)).first()

    if not compiler:
        raise ValueError(f"could not find compiler {repr(compiler_name)}")

    return compiler


@db_session
def plot_benchmarks(system_name, compiler_name, benchmark, perf_event_names, primitive_names_or_amount, other_measure_names, output):
    system = get_system_from_name_or_default(system_name)
    compiler = get_compiler_from_name(compiler_name)

    # Select only the latest runs for a given version limit and merge strategy
    runs = select(
        r for r in Run
        if r.system == system and r.compiler == compiler and r.benchmark.name == benchmark
        and r.timestamp == max(select(
            r2.timestamp for r2 in Run
            if r2.system == system and r2.compiler == compiler
            and r2.benchmark.name == benchmark
            and r2.version_limit == r.version_limit
            and r2.merge_strategy == r.merge_strategy)))\
        .order_by(Run.merge_strategy).order_by(Run.version_limit)

    logger.info(f"found {len(runs)} (only latest)")

    primitive_names = select_primitives_names(runs, primitive_names_or_amount)

    if primitive_names:
        logger.info(f"primitives to plot: {', '.join(primitive_names)}")

    data = extract_data_from_runs(runs, perf_event_names, primitive_names, other_measure_names)
    output_path = choose_barchart_output_path(output=output,
                                     system_name=system.name,
                                     compiler_name=compiler.name,
                                     benchmark=benchmark,
                                     perf_event_names=perf_event_names,
                                     primitive_names=primitive_names)

    logger.info(f"output to {output_path}")

    plot_data(data, output_path)


_sentinel = object()
def get_perf_event_statistics(run, name, default=_sentinel):
    events = select(e for e in PerfEvent if e.event == name and e.run == run)
    values = [e.value for e in events]

    if not values:
        if default is _sentinel:
            raise ValueError(f'Cannot find {repr(name)}')
        else:
            return default

    mean = statistics.mean(values)
    stdev = statistics.stdev(values) if len(values) > 1 else 0

    return ChartValue(mean, stdev)

def extract_data_from_runs(runs, perf_event_names, primitive_names, other_measure_names):
    data = []

    for run in runs:
        perf_events = {}

        for name in perf_event_names:
            perf_events[name] = get_perf_event_statistics(run, name)

        primitive_counts = {}

        for name in primitive_names:
            if prim := PrimitiveCount.get(run=run, name=name):
                primitive_counts[name] = ChartValue(prim.value)
            else:
                logger.debug(f"{name} primitive not found, defaulting to 0")
                primitive_counts[name] = ChartValue(0)

        other_measures = {}

        for name in other_measure_names:
            if not (measure := OtherMeasure.get(run=run, name=name)):
                raise ValueError(f'Cannot find {repr(name)}')
            other_measures[name] = ChartValue(measure.value)

        data.append((run, perf_events, primitive_counts, other_measures))

    return data


def plot_data(data, output_path):
    # Compute colors
    n_perf_stats = max((len(p) for _, p, _, _ in data), default=0)
    n_primitive_stats = max((len(p) for _, _, p, _ in data), default=0)
    n_other_measures = max((len(m) for _, _, _, m in data), default=0)
    n_measurments = n_perf_stats + n_primitive_stats + n_other_measures

    perf_colors = mpl.colormaps.get_cmap('winter')(np.linspace(0.3, 0.8, n_perf_stats))
    primitive_colors = mpl.colormaps.get_cmap('autumn')(np.linspace(0.3, 0.8, n_primitive_stats))
    other_measure_colors = mpl.colormaps.get_cmap('summer')(np.linspace(0.3, 0.8, n_other_measures))

    # Format data for matplotlib
    data_no_bbv = [d for d in data if d[0].version_limit == 0][:1] # only keep one execution
    data_with_bbv = [d for d in data if d[0].version_limit > 0]
    data_with_bbv.sort(key=lambda d: (d[0].version_limit, d[0].merge_strategy))

    data = data_no_bbv + data_with_bbv

    versions = ["No BBV"] + [f"{r.merge_strategy} {r.version_limit}" for r, *_ in data_with_bbv]
    measures = collections.defaultdict(list)

    first = data_no_bbv[0]

    def get_first_point(name):
        for measures in first[1:]:
            if measure := measures.get(name):
                return measure
        return ChartValue.zero

    for run, *measure_sets in data:
        for m in measure_sets:
            for name, measure in sorted(m.items(), key=operator.itemgetter(0)):
                measures[name].append(measure / get_first_point(name))

    def category_order(name):
        for i, group in enumerate(first[1:]):
            for datum_name in group.keys():
                if name.startswith(datum_name):
                    logger.debug(f"{name} goes into bar group {i}")
                    return i
        raise ValueError(f"could not order {name}")

    # Label location and bar width
    n_versions = len(versions)
    x = np.arange(len(versions))
    num_bars = n_versions * len(measures)

    width = 18 / (num_bars + n_versions)
 
    multiplier = 0

    # Plot
    fig, ax = plt.subplots(layout='constrained')

    for (attribute, measurement), color in zip(measures.items(),
                                               itertools.chain(perf_colors, primitive_colors, other_measure_colors)):
        offset = width * multiplier
        rects = ax.bar(x + offset, [m.value for m in measurement],
                       width=width, label=attribute, color=color,
                       yerr=[m.stdev or math.nan for m in measurement],
                       capsize=width * 7)
        multiplier += 1

    ax.set_xticks(x + width * (n_measurments / 2 - 0.5), versions, rotation=45, rotation_mode="anchor", ha='right')

    plt.legend(loc='lower center', bbox_to_anchor=(0.5, 1.1), ncol=3)

    plt.title(first[0].benchmark.name.title())

    plt.tight_layout()

    ensure_directory_exists(output_path)
    plt.savefig(output_path)


##############################################################################
# Correlation analysis
##############################################################################

def choose_analysis_output_path(output, merge_strategy, system_name, compiler_name):
    return choose_path(f'analysis_{merge_strategy}', output, system_name, compiler_name)


def compute_ratio_dataframe(benchmark_runs, perf_attributes, other_attributes, pseudo_ratio_offset=1):
    benchmark_runs = list(benchmark_runs)
    rows = {}

    logger.info(f"Computing ratios for {benchmark_runs[0].benchmark.name}")

    for run in benchmark_runs:
        row = []
        rows[run.version_limit] = row

        for perf_attr in perf_attributes:
            row.append(get_perf_event_statistics(run, perf_attr).value)

        for other_attr in other_attributes:
            row.append(OtherMeasure.get(name=other_attr, run=run).value)

    df = pd.DataFrame(np.nan, index=range(max(rows) + 1), columns=perf_attributes + other_attributes)

    for i, row_data in rows.items():
        df.loc[i] = row_data

    if df.isna().any().any():
        logger.warning(f"NaN found in dataframe, there seems to be missing data")

    # Use pseudo ratio (d[i] + pseudo_ratio_offset) / (d[0] + pseudo_ratio_offset)
    # for columns which contain a zero
    cols_with_zero = df.columns[(df == 0).any()]
    df[cols_with_zero] = df[cols_with_zero] + pseudo_ratio_offset

    logger.info(f"Zero values found, using pseudo-ratio for the following columns: {', '.join(cols_with_zero)}")

    # Compute ratios based on the first column
    return df.div(df.iloc[0])

@db_session
def analyze_merge_strategy(merge_strategy, benchmark_names, system_name, compiler_name, output):
    system = get_system_from_name_or_default(system_name)
    compiler = get_compiler_from_name(compiler_name)

    output_path = choose_analysis_output_path(output=output,
                                              merge_strategy=merge_strategy,
                                              system_name=system.name,
                                              compiler_name=compiler.name)

    # Select benchmarks for analysis
    if benchmark_names is None:
        benchmarks_filter = list(select(b.name for b in Benchmark).distinct())
    else:
        benchmarks_filter = benchmark_names

    for benchmark_name in benchmarks_filter[:]:
        bench = Benchmark.get(name=benchmark_name)

        if not bench:
            benchmarks_filter.remove(benchmark_name)
            logger.warning(f"benchmark does not exist: {repr(benchmark_name)}")

        elif not exists(r for r in Run if r.benchmark == bench
                      and r.system == system and r.compiler == compiler
                      and r.merge_strategy == merge_strategy):
            benchmarks_filter.remove(benchmark_name)
            logger.warning(f"benchmark does not exist for the provided settings: {repr(benchmark_name)}")

    logger.debug(f"benchmarks used: {', '.join(benchmarks_filter)}")

    # Select only the latest runs for a each benchmark
    runs = list(select(
        r for r in Run
        if r.system == system and r.compiler == compiler and r.merge_strategy == merge_strategy
        and r.benchmark.name in benchmarks_filter
        and r.timestamp == max(select(
            r2.timestamp for r2 in Run
            if r2.system == system and r2.compiler == compiler
            and r2.merge_strategy == merge_strategy and r2.benchmark == r.benchmark
            and r2.version_limit == r.version_limit))).order_by(Run.benchmark))

    logger.info(f"found {len(runs)} (only latest)")

    # Format data in a pandas dataframe
    perf_attributes = list(select(e.event for e in PerfEvent if e.run in runs).distinct())

    # Remove attributes that perf could not recover on all runs
    perf_attributes = [p for p in perf_attributes if all(get_perf_event_statistics(run, p, None) for run in runs)]

    logger.info(f"perf attributes in analysis: {', '.join(perf_attributes)}")

    other_attributes = list(select(m.name for m in OtherMeasure if m.run in runs).distinct())
    logger.info(f"other measures in analysis: {', '.join(perf_attributes)}")

    data_points = {}

    benchmarks_groups = itertools.groupby(runs, key=lambda r: r.benchmark.id)  # group runs by version

    ratio_dataframes = [compute_ratio_dataframe(group, perf_attributes, other_attributes)
                        for _, group in benchmarks_groups]

    # Compute the mean along the third axis, ignoring NaN values
    df_average_ratio = pd.DataFrame(np.prod([df.values for df in ratio_dataframes], axis=0),
                                    columns=ratio_dataframes[0].columns) ** (1 / len(ratio_dataframes))

    # Plot
    fig, ax = plt.subplots(figsize=(15, 5))

    def clamp(x):
        return min(2, max(0, x))

    vmin = clamp(np.nanmin(df_average_ratio.values))
    vmax = clamp(np.nanmax(df_average_ratio.values))

    heatmap_ax = sns.heatmap(df_average_ratio, annot=True, fmt='.2g', cmap="coolwarm", linewidths=.5, vmin=vmin, vmax=vmax, center=1)

    for text in heatmap_ax.texts:
        text.set_size(7)

    plt.title(f"Mean ratio for {repr(merge_strategy)} strategy", fontsize=16, fontweight='bold')

    ax.xaxis.tick_top()
    plt.xticks(rotation=35, rotation_mode="anchor", ha='left')
    
    text_size = 12 if len(benchmarks_filter) < 5 else 12 / (len(benchmarks_filter) / 15)
    ax.text(0.5, -0.1, f"benchmarks: {', '.join(benchmarks_filter)}",
            size=text_size, ha="center", transform=ax.transAxes)

    plt.tight_layout()

    ensure_directory_exists(output_path)
    plt.savefig(output_path)


##############################################################################
# Perf data deistribution
##############################################################################

def choose_distribution_output_path(output, merge_strategy, system_name, compiler_name, benchmark, version_limit, perf_event):
    base = f"distribution_{benchmark}_{merge_strategy}{version_limit}_{perf_event}"
    return choose_path(base, output, system_name, compiler_name)

@db_session
def perf_distribution(merge_strategy,
                      system_name,
                      compiler_name,
                      benchmark_name,
                      event,
                      version_limit,
                      output):
    system = get_system_from_name_or_default(system_name)
    compiler = get_compiler_from_name(compiler_name)

    output_path = choose_distribution_output_path(output, merge_strategy, system.name,
                                                  compiler.name, benchmark_name, version_limit, event)

    benchmark = Benchmark.get(name=benchmark_name)

    run = Run.select(lambda r: r.system == system and
                     r.compiler == compiler and
                     r.benchmark == benchmark and
                     r.version_limit == version_limit and
                     r.merge_strategy == merge_strategy).order_by(desc(Run.timestamp)).first()

    values = sorted([event.value for event in select(e for e in PerfEvent if e.event == event and e.run == run)])

    plt.bar(range(len(values)), values)

    plt.savefig(output_path)
    

##############################################################################
# Command line interface
##############################################################################

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

    class PrimitivesInput(argparse.Action):
        def __call__(self, parser, namespace, values, option_string=None):
            if len(values) == 1:
                try:
                    setattr(namespace, self.dest, int(values[0]))
                    return
                except ValueError:
                    pass
            setattr(namespace, self.dest, values)

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

    # Parser for plotting benchmarks
    plot_parser = subparsers.add_parser('plot', help='Plot benchmarks')

    plot_parser.add_argument('-b', '--benchmark',
                             required=True,
                             dest="benchmark",
                             help="benchmark name or path")

    plot_parser.add_argument('-s', '--systen',
                             dest="system_name",
                             help="plot benchmark from this system")

    plot_parser.add_argument('-c', '--compiler',
                             dest="compiler_name",
                             default="gambit",
                             help="plot benchmark of this compiler")

    plot_parser.add_argument('-e', '--perf-events',
                             nargs="+",
                             default=(),
                             dest="perf_event_names",
                             help="perf stat events to plot")

    plot_parser.add_argument('-m', '--other-measures',
                             nargs="+",
                             default=(),
                             dest="other_measure_names",
                             help="other measures to plot")

    plot_parser.add_argument('-p', '--primitives',
                             nargs="+",
                             default=(),
                             dest="primitive_names_or_amount",
                             action=PrimitivesInput,
                             help="primitive calls to plot")

    plot_parser.add_argument('-o', '--output',
                             dest="output",
                             help="where to output the chart (file or folder)")

    # Parser for comparing merge strategy with correlation matrices
    analysis_parser = subparsers.add_parser('analysis', help='Plot correlation matrice for a merge strategy')

    analysis_parser.add_argument('-m', '--merge-strategy',
                             required=True,
                             dest="merge_strategy",
                             help="merge strategy to analyze")

    analysis_parser.add_argument('-s', '--systen',
                             dest="system_name",
                             help="analyze benchmarks from this system")

    analysis_parser.add_argument('-c', '--compiler',
                             dest="compiler_name",
                             default="gambit",
                             help="analyze benchmarks of this compiler")

    analysis_parser.add_argument('-b', '--benchmarks',
                                 dest="benchmark_names",
                                 nargs='+',
                                 help="benchmarks used for analysis (all if none given)")

    analysis_parser.add_argument('-o', '--output',
                                dest="output",
                                help="where to output the chart (file or folder)")

    perf_distribution_parser = subparsers.add_parser('perf_distribution', help='Plot perf distribution histogram')

    perf_distribution_parser.add_argument('-m', '--merge-strategy',
                                          dest="merge_strategy",
                                          default='linear',
                                          help="merge strategy to plot")

    perf_distribution_parser.add_argument('-s', '--systen',
                                          dest="system_name",
                                          help="analyze benchmarks from this system")

    perf_distribution_parser.add_argument('-c', '--compiler',
                                          dest="compiler_name",
                                          default="gambit",
                                          help="analyze benchmarks of this compiler")

    perf_distribution_parser.add_argument('-b', '--benchmark',
                                          required=True,
                                          dest="benchmark_name",
                                          help="benchmark name or path")

    perf_distribution_parser.add_argument('-e', '--event',
                                          required=True,
                                          dest="event",
                                          help="perf stat event name")

    perf_distribution_parser.add_argument('-l', '--limit',
                                          dest="version_limit",
                                          metavar="LIMIT",
                                          default=0,
                                          type=int,
                                          help="BBV versions limit to plot")

    perf_distribution_parser.add_argument('-o', '--output',
                                          dest="output",
                                          help="where to output the histogram (file or folder)")


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
        plot_benchmarks(benchmark=args.benchmark,
                        compiler_name=args.compiler_name,
                        system_name=args.system_name,
                        perf_event_names=args.perf_event_names,
                        primitive_names_or_amount=args.primitive_names_or_amount,
                        other_measure_names=args.other_measure_names,
                        output=args.output)
    elif args.command == 'analysis':
        analyze_merge_strategy(
            merge_strategy=args.merge_strategy,
            system_name=args.system_name,
            compiler_name=args.compiler_name,
            benchmark_names=args.benchmark_names,
            output=args.output)
    elif args.command == 'perf_distribution':
        perf_distribution(merge_strategy=args.merge_strategy,
                          system_name=args.system_name,
                          compiler_name=args.compiler_name,
                          benchmark_name=args.benchmark_name,
                          event=args.event,
                          version_limit=args.version_limit,
                          output=args.output)
    else:
        parser.print_help()


#!/usr/bin/env python3

import argparse
import ast
import collections
import datetime
import itertools
import locale
import logging
import math
import os
import pathlib
import platform
import re
import shlex
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


def sanitize_filename(filename, valid_chars="-_.()" + string.ascii_letters + string.digits):
    return ''.join(c for c in filename if c in valid_chars)


def ensure_directory_exists(filepath):
    dir_name = os.path.dirname(filepath)
    if not os.path.exists(dir_name):
        os.makedirs(dir_name)


def choose_barchart_output_path(output, system_name, compiler_name, benchmark, perf_event_names, primitive_names):
    path = pathlib.Path(output or '.').resolve()
    suffix = path.suffix
    
    if not suffix:
        # No extension means the output is a folder where to output the plot
        logger.debug(f"output into folder {path}")

        # build default filename
        primitive_segment = f"_{len(primitive_names)}primitives" if primitive_names else ""
        filename = f"{benchmark}_{'_'.join(sorted(perf_event_names))}{primitive_segment}_{compiler_name}_{system_name}.png"

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


def select_primitives_names(runs, primitive_names_or_amount):
    if isinstance(primitive_names_or_amount, int):
        # Recover the most common primitives
        primitives = select(prim for prim in PrimitiveCount if prim.run in runs).order_by(desc(PrimitiveCount.count))
        return list(select(prim.name for prim in primitives if prim.name !=
                      "##identity").distinct()[:primitive_names_or_amount])
    else:
        return primitive_names_or_amount


def get_system_from_name_or_default(system_name=None):
    system = System.get_current_system() if system_name is None else System.get(name=system_name)

    if not system:
        raise ValueError(f"could not find system {repr(system_name) or ''}")

    return system


def get_compiler_from_name(compiler_name):
    compiler = select(c for c in Compiler if c.name == compiler_name).order_by(desc(Compiler.commit_timestamp)).first()

    if not compiler:
        raise ValueError(f"could not find compiler {repr(compiler_name)}")

    return compiler


@db_session
def plot_benchmarks(system_name, compiler_name, benchmark, perf_event_names, primitive_names_or_amount, output):
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

    data = extract_data_from_runs(runs, perf_event_names, primitive_names)
    output_path = choose_barchart_output_path(output=output,
                                     system_name=system.name,
                                     compiler_name=compiler.name,
                                     benchmark=benchmark,
                                     perf_event_names=perf_event_names,
                                     primitive_names=primitive_names)

    logger.info(f"output to {output_path}")

    plot_data(data, output_path)


def extract_data_from_runs(runs, perf_event_names, primitive_names):
    data = []

    for run in runs:
        perf_events = {}

        for name in perf_event_names:
            if not (event := PerfEvent.get(run=run, event=name)):
                raise ValueError(f'Cannot find {repr(name)}')
            perf_events[name] = event.value

        primitive_counts = {}

        for name in primitive_names:
            if prim := PrimitiveCount.get(run=run, name=name):
                primitive_counts[name] = prim.count
            else:
                logger.debug(f"{name} primitive not found, defaulting to 0")
                primitive_counts[name] = 0

        data.append((run, perf_events, primitive_counts))

    return data


def plot_data(data, output_path):
    def run_label(run):
        return f"{run.merge_strategy} {run.version_limit}"

    def get_magnitude_shift(number, target_range):
        if number == 0:
            return 0

        if target_range[0] <= number <= target_range[1]:
            return 0

        # Find by how many magnitude (powers of ten) the number is bigger than the target_range
        low = math.log10(target_range[0] / number)
        high = math.log10(target_range[1] / number)
        return -(math.ceil(low) if number < target_range[0] else math.floor(high))

    def add_scaling_to_label(name, magnitude_shifts):
        shift = magnitude_shifts[name]

        unit = "msec" if name == BenchResultParser.time_event else ""

        if shift == 0:
            return f"{name} ({unit})" if unit else name
        else:
            superscript_translation = str.maketrans(string.digits, '⁰¹²³⁴⁵⁶⁷⁸⁹')
            unit = f" {unit}" if unit else ""
            return f"{name} (10{str(shift).translate(superscript_translation)}{unit})"

    def get_magnitude():
        values = set()

        for _, perf, prim in data:
            values.update(perf.values())
            values.update(prim.values())

        smallest = min(v for v in values if v > 0)
        highest = max(values)
        base = 10 ** math.floor(math.log10(smallest))

        return base, base * 10

    # Compute colors
    n_perf_stats = max(len(p) for _, p, _ in data)
    n_primitive_stats = max(len(p) for _, _, p in data)
    n_measurments = n_perf_stats + n_primitive_stats

    perf_colors = mpl.colormaps.get_cmap('viridis')(np.linspace(0.3, 0.8, n_perf_stats))
    primitive_colors = mpl.colormaps.get_cmap('inferno')(np.linspace(0.3, 0.8, n_primitive_stats))

    # Format data for matplotlib
    data = sorted(data, key=lambda d: (d[0].version_limit, d[0].merge_strategy))

    versions = [run_label(r) for r, *_ in data]
    measures = collections.defaultdict(list)

    first = data[0]
    first_perf_events = first[1]
    first_primitive_counts = first[2]

    def perf_key(p):
        name = p[0]
        if name == BenchResultParser.time_event:
            return math.inf
        else:
            return first_perf_events[name]

    sorted_first_perf_events = sorted(first_perf_events.items(), key=perf_key, reverse=True)
    if len(first_perf_events) == 0:
        last_perf_event = None
    else:
        last_perf_event = sorted_first_perf_events[-1][0]

    for run, perf_events, primitive_counts in data:
        perf_events = sorted(perf_events.items(), key=perf_key, reverse=True)
        primitive_counts = sorted(primitive_counts.items(), key=lambda p: first_primitive_counts[p[0]], reverse=True)
        for name, value in itertools.chain(perf_events, primitive_counts):
            measures[name].append(value)

    # Adjust magnitude of data so they fit in a single graph
    target_range = get_magnitude()
    magnitude_shifts = {k: get_magnitude_shift(max(v), target_range) for k, v in measures.items()}
    scaled_measures = {}

    for name in measures:
        name_with_scaling = add_scaling_to_label(name, magnitude_shifts)
        scaled_measures[name_with_scaling] = [v / (10 ** magnitude_shifts[name]) for v in measures[name]]

    # Label location and bar width
    n_versions = len(versions)
    x = np.arange(len(versions))
    width = 18 / n_versions * (0.75 / (len(first_perf_events) + len(first_primitive_counts) + 1) ** 0.95) # it just looks good
    multiplier = 0
    spacing_between_perf_and_primitives = width / 0.5

    # Plot
    fig, ax = plt.subplots(layout='constrained')

    for (attribute, measurement), color in zip(scaled_measures.items(),
                                               itertools.chain(perf_colors, primitive_colors)):
        offset = width * multiplier
        rects = ax.bar(x + offset, measurement, width, label=attribute, color=color)
        multiplier += 1 + (last_perf_event in attribute) * spacing_between_perf_and_primitives

    ax.set_xticks(x + width * (n_measurments / 2 - 0.5), versions, rotation=45, rotation_mode="anchor", ha='right')

    ax.legend(loc='upper right', ncols=2)
    plt.title(first[0].benchmark.name.title())

    ensure_directory_exists(output_path)
    plt.savefig(output_path)


##############################################################################
# Correlation analysis
##############################################################################

def choose_analysis_output_path(output, merge_strategy, system_name, compiler_name,
                                valid_chars="-_.()" + string.ascii_letters + string.digits):
    path = pathlib.Path(output or '.').resolve()
    suffix = path.suffix

    if not suffix:
        # No extension means the output is a folder where to output the plot
        logger.debug(f"output into folder {path}")

        # build default filename
        filename = f"analysis_{merge_strategy}_{compiler_name}_{system_name}.png"

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
    runs = select(
        r for r in Run
        if r.system == system and r.compiler == compiler and r.merge_strategy == merge_strategy
        and r.benchmark.name in benchmarks_filter
        and r.timestamp == max(select(
            r2.timestamp for r2 in Run
            if r2.system == system and r2.compiler == compiler
            and r2.merge_strategy == merge_strategy and r2.benchmark == r.benchmark
            and r2.version_limit == r.version_limit))).order_by(Run.version_limit)

    logger.info(f"found {len(runs)} (only latest)")

    # group runs by version
    runs_groups = itertools.groupby(runs, key=lambda r: r.version_limit)

    # Format data in a pandas dataframe
    perf_attributes = select(e.event for e in PerfEvent if e.run in runs).distinct()[:]

    all_attributes = [*perf_attributes, 'typechecks']

    logger.info(f"attributes in analysis: {', '.join(all_attributes)}")

    data_points = {}

    for version_limit, group in runs_groups:
        data_points[version_limit] = [0] * len(all_attributes)

        group = list(group)

        for r in group:
            for i, perf_attr in enumerate(perf_attributes):
                data_points[version_limit][i] += PerfEvent.get(event=perf_attr, run=r).value

            i = all_attributes.index('typechecks')

            primitive_counts = select(p for p in PrimitiveCount if p.run in group)

            for prim_count in primitive_counts:
                if prim_count.is_typecheck:
                    data_points[version_limit][i] += prim_count.count


    df = pd.DataFrame(list(data_points.values()), columns=all_attributes)

    print(df)

    # Compute ratios based on the first non-zero occurence through versions
    df_ratio = df.copy()
    for col in df.columns:
        for i, val in enumerate(df[col]):
            if val != 0:
                df_ratio[col] = df[col] / val
                df_ratio.loc[:i-1, col] = np.nan
                break
            else:
                df_ratio.loc[:, col] = np.nan

    # Plot
    fig, ax = plt.subplots(figsize=(15, 5))

    vmin = max(np.nanmin(df_ratio.values), 0.5)
    vmax = max(np.nanmax(df_ratio.values), 1.2)

    sns.heatmap(df_ratio, cmap="coolwarm", linewidths=.5, vmin=vmin, vmax=vmax, center=1)

    plt.title(f"Ratio for {repr(merge_strategy)} strategy", fontsize=16, fontweight='bold')

    ax.xaxis.tick_top()
    plt.xticks(rotation=35, rotation_mode="anchor", ha='left')
    
    text_size = 12 if len(benchmarks_filter) < 5 else 12 / (len(benchmarks_filter) / 15)
    ax.text(0.5, -0.1, f"benchmarks: {', '.join(benchmarks_filter)}",
            size=text_size, ha="center", transform=ax.transAxes)

    plt.tight_layout()

    ensure_directory_exists(output_path)
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
                        output=args.output)
    elif args.command == 'analysis':
        analyze_merge_strategy(
            merge_strategy=args.merge_strategy,
            system_name=args.system_name,
            compiler_name=args.compiler_name,
            benchmark_names=args.benchmark_names,
            output=args.output)

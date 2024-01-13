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
    content = Required(str)
    timestamp = Required(int)
    runs = Set('Run')

class PrimitiveCount(db.Entity):
    name = Required(str)
    value = Required(int, size=64)
    run = Required('Run')

    @property
    def is_typecheck(self):
        typechecks = (
                # Gambit
                "##fixnum?", '##flonum?', "##vector?", "##pair?", "##box?", "##procedure?",
                "##bignum?", "##ratnum?", "##boolean?", "##string?", "##char?",
                "##bytevector?", "##u8vector?", "##u16vector?", "##u32vector?",
                "##u64vector?", "##s8vector?", "##s16vector?", "##s32vector?",
                "##s64vector?", "##f8vector?", "##f16vector?", "##f32vector?",
                "##f64vector?", "##null?")
        typechecks = typechecks + tuple(n.replace("##", "") for n in typechecks) + \
            tuple(n.replace("##", "$") for n in typechecks)
        return self.name in typechecks

class PerfEvent(db.Entity):
    event = Required(str)
    value = Required(int, size=64)
    run = Required('Run')

class OtherMeasure(db.Entity):
    name = Required(str)
    value = Required(float)
    run = Required('Run')

class Run(db.Entity):
    benchmark = Required('Benchmark', reverse='runs')
    system = Required('System', reverse='runs')
    compiler = Required('Compiler', reverse='runs')
    compiler_optimizations = Required(bool)
    version_limit = Required(int)
    safe_arithmetic = Required(bool)
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

def run_command(command, timeout, env=None):
    logger.info(command)

    env = env = os.environ.copy() if env is None else env

    if timeout is not None:
        logger.info(f"(with timeout: {timeout}s)")

    process = None

    try:
        process = subprocess.Popen(shlex.split(command),
                                   env=env,
                                   stdout=subprocess.PIPE,
                                   stderr=subprocess.STDOUT)
        output, _ = process.communicate(timeout=timeout)
        logger.debug(output)
        return output.decode()
    finally:
        if process and process.poll() is None:
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

        self.size_in_gvm_instructions = self.primitives.pop(self.SIZE_IN_GVM_INSTRUCTIONS, None)

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


class PerfResultParser:
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

        #logger.debug(f"could not find perf stat event {name}")
        return None

class SchemeStatsParser:
    def __init__(self, scheme_output):
        self.stats = {}

        for name, value in re.findall(r'\((\S+) ([\d\.]+)\)', scheme_output):
            self.stats[f'scheme-{name}'] = value

    def keys(self):
        return self.stats.keys()

    def values(self):
        return self.stats.values()

    def items(self):
        return self.stats.items()

    def update(self, other):
        self.stats.update(other.stats)

    def __getitem__(self, event_name):
        return self.stats[event_name]

    def __contains__(self, event_name):
        return event_name in self.stats


##############################################################################
# Benchmark execution
##############################################################################

COMPILE_SCRIPT = "../compile"


def extract_executable_from_compiler_output(content):
    return re.search(r"\*\*\*executable: (.+)", content).group(1)


def compile_gambit(gambitdir, file, vlimit, safe_arithmetic, compiler_optimizations, timeout=None):
    env = os.environ.copy()


    optimization_flag = "-O3" if compiler_optimizations else ""
    arithmetic_flag = "-U" if not safe_arithmetic else ""

    command = f"{COMPILE_SCRIPT} -S gambit -D {gambitdir} -V {vlimit} {arithmetic_flag} {optimization_flag} -P -f {file}"

    output = run_command(command, timeout, env)

    executable = extract_executable_from_compiler_output(output)

    logger.info(f"executable created at: {executable}")

    primitive_count = PrimitivesCountParser(output)

    if not primitive_count:
        logger.warning("Failed to parse primitive count")
    else:
        logger.debug(f"Primitive count: {dict(primitive_count)}")

    return executable, primitive_count


def compile_bigloo(file, vlimit, safe_arithmetic, compiler_optimizations, timeout=None):
    def get_command(primitive_count):
        optimization_flag = "-O3" if compiler_optimizations else ""
        primitive_count_flag = '-P' if primitive_count else ''
        arithmetic_flag = '-U' if not safe_arithmetic else ''
        return f"{COMPILE_SCRIPT} -S bigloo -V {vlimit} {arithmetic_flag} {optimization_flag} {primitive_count_flag} -f {file}"

    # First execution with primitive count
    command_with_primitives = get_command(True)

    output = run_command(command_with_primitives, timeout)

    executable = extract_executable_from_compiler_output(output)

    executable_output = run_command(executable, timeout)

    primitive_count = PrimitivesCountParser(executable_output)

    # Second execution without primitive count
    command = get_command(False)

    output = run_command(command, timeout)

    executable = extract_executable_from_compiler_output(output)

    logger.info(f"executable created at: {executable}")

    if not primitive_count:
        logger.warning("Failed to parse primitive count")
    else:
        logger.debug(f"Primitive count: {dict(primitive_count)}")

    return executable, primitive_count


def compile(compiler_execution_data, file, vlimit, safe_arithmetic, compiler_optimizations, timeout=None):
    gambitdir = compiler_execution_data.get('gambitdir')
    use_bigloo = compiler_execution_data.get('use_bigloo')

    if use_bigloo:
        return compile_bigloo(file, vlimit, safe_arithmetic, compiler_optimizations, timeout)
    elif gambitdir:
        return compile_gambit(gambitdir, file, vlimit, safe_arithmetic, compiler_optimizations, timeout)
    else:
        raise NotImplementedError


def run_benchmark(executable, arguments):
    # Run program to measure time only
    time_command = f"perf stat -e {PerfResultParser.time_event} {executable} {arguments}"
    logger.info(time_command)
    time_output = subprocess.run(time_command, shell=True, capture_output=True)
    time_output_stderr = time_output.stderr.decode()
    time_output_stdout = time_output.stdout.decode()
    logger.debug(time_output_stderr)
    logger.debug(time_output_stdout)

    # Run program with all perf stat events on
    other_events = ' '.join(f"-e {e}" for e in PerfResultParser.event_names)
    other_command = f"perf stat {other_events} {executable} {arguments}"
    logger.info(other_command)
    other_output = subprocess.run(other_command, shell=True, capture_output=True)
    other_output_stderr = other_output.stderr.decode()
    logger.debug(other_output_stderr)

    # parse and join outputs
    time_parser = PerfResultParser(time_output_stderr)
    other_perf_parser = PerfResultParser(other_output_stderr)

    other_perf_parser.update(time_parser)

    for event in PerfResultParser.event_names:
        if event not in other_perf_parser:
            logger.info(f"Could not find perf stat event {repr(event)} when running {executable}")

    scheme_parser = SchemeStatsParser(time_output_stdout)

    return other_perf_parser, scheme_parser

default_arguments = "repeat: 20"

benchmark_args = {
    "ack": "repeat: 50 m: 3 n: 9",
    "bague": "repeat: 1",
    "fib": "repeat: 3 n: 39",
    "fibfp": "repeat: 2 n: 39.0",
    "tak": "repeat: 5000 x: 18 y: 12 z: 6",
    "takl": "repeat: 1000 x: 18 y: 12 z: 6",
    "diviter": "repeat: 100000 ",
    "divrec": "repeat: 100000 ",
    "array1": "repeat: 5 n: 200000",
    "browse": "repeat: 1000 ",
    "mazefun": "repeat: 2000 n: 11 m: 11",
    "nqueens": "repeat: 2 n: 13",
    "puzzle": "repeat: 200 n: 511",
    "quicksort": "repeat: 500 ",
    "sum": "repeat: 10000 n: 10000",
    "sumfp": "repeat: 200 n: 1e6",
    "triangl": "repeat: 20 i: 22 depth: 1",
    "almabench": "repeat: 1 K: 36525",
    "fft": "repeat: 1 n: 1048576",
    "boyer": "repeat: 50",
}

def get_gambit_program_size(executable, benchmark):
    objdump_command = f"objdump --disassemble {executable} | sed -e '/ <.*>:/!d'"
    logger.info(objdump_command)
    objdump_output = subprocess.run(objdump_command, shell=True, capture_output=True).stdout.decode()
    logger.debug(objdump_output)
    lines = objdump_output.splitlines()

    start_marker = f"<___H_{benchmark.name}>"
    end_marker = f"<___LNK_{benchmark.name}>"

    logger.debug(f"looking for {start_marker} and {end_marker}")

    def parse(marker, it):
        for line in it:
            if marker in line:
                return int(line.split()[0], 16)
        return None

    line_iterator = iter(objdump_output.splitlines())
    start = parse(start_marker, line_iterator)
    end = parse(end_marker, line_iterator)

    if not (start and end):
        return None

    return end - start

def get_bigloo_program_size(executable):
    o_file_path = os.path.abspath(os.path.join(os.getcwd(), "../bigloo/bbv.o"))

    if not os.path.exists(o_file_path):
        raise FileNotFoundError(f"could not find {o_file_path}")

    objdump_command = f"objdump --disassemble {o_file_path} | sed -e '/ <.*>:/!d'"
    logger.info(objdump_command)
    objdump_output = subprocess.run(objdump_command, shell=True, capture_output=True).stdout.decode()
    logger.debug(objdump_output)
    
    marker = "<bigloo_abort>"

    match = re.search(fr"([0-9a-fA-F]+)\s+{marker}", objdump_output)

    if not match:
        raise ValueError(f"could not find {repr(marker)} label in {o_file_path}")

    logger.info(f"found size from marker: {match.group(0)}")

    return int(match.group(1), 16)

def get_program_size(executable, benchmark, compiler):
    if compiler.name == 'gambit':
        return get_gambit_program_size(executable, benchmark)
    else:
        return get_bigloo_program_size(executable)

def get_or_create_bigloo_or_gambit(gambitdir, use_bigloo):
    if gambitdir:
        compiler, _ = Compiler.get_or_create_compiler(gambitdir)
        return compiler
    
    if use_bigloo:
        compiler, _ = Compiler.get_or_create(name='bigloo',
                                             commit_sha='?',
                                             commit_description='?',
                                             commit_author='?',
                                             commit_timestamp=-1)
        return compiler

    raise NotImplementedError

@db_session
def run_and_save_benchmark(gambitdir, use_bigloo, file, version_limits, safe_arithmetic, repetitions, compiler_optimizations, force_execution=False, timeout=None):
    system, _ = System.get_or_create_current_system()
    compiler = get_or_create_bigloo_or_gambit(gambitdir, use_bigloo)

    compiler_execution_data = {'gambitdir': gambitdir, 'use_bigloo': use_bigloo, 'compiler': compiler}

    with open(file) as f:
        name = os.path.splitext(os.path.basename(file))[0]
        timestamp = int(os.path.getmtime(file))
        benchmark, _ = Benchmark.get_or_create(name=name, content=f.read(), timestamp=timestamp)

    for v in version_limits:
        logger.info(f'- benchmark: {file}\n'
                    f'- version limit: {v}\n'
                    f'- safe arithmetic: {safe_arithmetic}')

        base_arguments = benchmark_args.get(benchmark.name)
        if base_arguments is None:
            logger.warn(f"no CLI argument for {repr(benchmark.name)}")
            base_arguments = default_arguments
        if compiler.name == 'gambit':
            base_arguments = f"-:m100M {base_arguments}"

        if not force_execution:
            existing_run = Run.get(benchmark=benchmark,
                                   system=system,
                                   compiler=compiler,
                                   version_limit=v,
                                   safe_arithmetic=safe_arithmetic,
                                   compiler_optimizations=compiler_optimizations,
                                   arguments=base_arguments)
            if existing_run:
                logger.info('benchmark has an existing run, skip it')
                continue
            else:
                logger.info('no run exists for this benchmark, execute it')

        run = Run(benchmark=benchmark,
                  system=system,
                  compiler=compiler,
                  version_limit=v,
                  safe_arithmetic=safe_arithmetic,
                  compiler_optimizations=compiler_optimizations,
                  arguments=base_arguments)

        executable, primitive_count = compile(compiler_execution_data, file, v, safe_arithmetic, compiler_optimizations, timeout)

        align_stack_step = 3
        for rep in range(repetitions):
            arguments = f"{base_arguments} align-stack: {rep * align_stack_step}"
            perf_results, scheme_stats = run_benchmark(executable, arguments)
            for event, (value, variance) in perf_results.items():
                PerfEvent(event=event, value=int(value), run=run)

            for name, value in scheme_stats.items():
                logger.info(f"saving scheme stat {repr(name)}: {value}")
                OtherMeasure(name=name, value=value, run=run)

        typechecks = 0

        for prim, value in primitive_count.items():
            p = PrimitiveCount(name=prim, value=value, run=run)
            typechecks += p.is_typecheck * value

        logger.debug(f"number of typechecks: {typechecks}")
        OtherMeasure(name="typechecks", value=typechecks, run=run)

        total_primitives = sum(primitive_count.values())
        logger.debug(f"executed primitives: {total_primitives}")
        OtherMeasure(name='primitives', value=total_primitives, run=run)

        if primitive_count.size_in_gvm_instructions:
            logger.debug(f"gvm instruction size: {primitive_count.size_in_gvm_instructions}")
            OtherMeasure(name='gvm-size', value=primitive_count.size_in_gvm_instructions, run=run)
        else:
            logger.debug("Did not find count for total GVM instructions")

        size = get_program_size(executable, benchmark, compiler)
        if size:
            logger.debug(f"program size: {size}")
            OtherMeasure(name='program-size', value=size, run=run)
        else:
            logger.warning(f"could not resolve size of {repr(executable)}")

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


def choose_path(base, output, system_name, compiler_name, safe_arithmetic,
                valid_chars="-_.()" + string.ascii_letters + string.digits):
    path = pathlib.Path(output or '.').resolve()
    suffix = path.suffix

    if not suffix:
        # No extension means the output is a folder where to output the plot
        logger.debug(f"output into folder {path}")

        arithmetic_segment = "safe" if safe_arithmetic else "unsafe"

        # build default filename
        filename = f"{base}_{compiler_name}_{system_name}_{arithmetic_segment}.png"

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


def choose_barchart_output_path(output, system_name, compiler_name, safe_arithmetic, benchmark, perf_event_names, primitive_names):
    primitive_segment = f"_{len(primitive_names)}primitives" if primitive_names else ""
    base = f"{benchmark}_{'_'.join(sorted(perf_event_names))}{primitive_segment}"
    return choose_path(base, output, system_name, compiler_name, safe_arithmetic)


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
def plot_benchmarks(system_name, compiler_name, benchmark, safe_arithmetic, perf_event_names, primitive_names_or_amount, other_measure_names, output):
    system = get_system_from_name_or_default(system_name)
    compiler = get_compiler_from_name(compiler_name)

    # Select only the latest runs for a given version limit
    runs = select(
        r for r in Run
        if r.system == system and r.compiler == compiler and r.benchmark.name == benchmark
           and r.safe_arithmetic == safe_arithmetic
           and (not r.compiler_optimizations or r.version_limit == 0)
        and r.timestamp == max(select(
            r2.timestamp for r2 in Run
            if r2.system == system and r2.compiler == compiler
            and r2.benchmark.name == benchmark
            and r2.version_limit == r.version_limit
            and r2.safe_arithmetic == safe_arithmetic
            and r2.compiler_optimizations == r.compiler_optimizations)))\
        .order_by(Run.version_limit)

    logger.debug(f"found {len(runs)} (only latest)")

    primitive_names = select_primitives_names(runs, primitive_names_or_amount)

    if primitive_names:
        logger.info(f"primitives to plot: {', '.join(primitive_names)}")

    data = extract_data_from_runs(runs, perf_event_names, primitive_names, other_measure_names)
    output_path = choose_barchart_output_path(output=output,
                                     system_name=system.name,
                                     compiler_name=compiler.name,
                                     safe_arithmetic=safe_arithmetic,
                                     benchmark=benchmark,
                                     perf_event_names=perf_event_names,
                                     primitive_names=primitive_names)

    logger.info(f"output to {output_path}")

    plot_data(data, output_path)


_sentinel = object()
def sanitize_stats(name, values, default=_sentinel, _logged=[False]):
    if not values:
        if default is _sentinel:
            raise ValueError(f'Cannot find {repr(name)}')
        else:
            return default

    percentage = 0.1

    if not _logged[0]:
        logger.debug(f"trimming {int(percentage * 100)}% of outliers")
        _logged[0]= True

    values.sort()
    values = values[math.floor(len(values) * percentage):math.ceil(len(values) * (1 - percentage)) + 1]

    mean = statistics.mean(values)
    stdev = statistics.stdev(values) if len(values) > 1 else 0

    return ChartValue(mean, stdev)

def get_perf_event_statistics(run, name, default=_sentinel):
    events = select(e for e in PerfEvent if e.event == name and e.run == run)
    values = [e.value for e in events]
    return sanitize_stats(name, values, default)


def get_other_measure_statistics(run, name, default=_sentinel):
    events = select(e for e in OtherMeasure if e.name == name and e.run == run)
    values = [e.value for e in events]
    return sanitize_stats(name, values, default)


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
    data_no_bbv_no_optim = [d for d in data if d[0].version_limit == 0
                                               and not d[0].compiler_optimizations][:1]
    data_no_bbv_optim = [d for d in data if d[0].version_limit == 0
                                            and d[0].compiler_optimizations][:1]
    data_with_bbv = [d for d in data if d[0].version_limit > 0]
    data_with_bbv.sort(key=lambda d: d[0].version_limit)

    data = data_no_bbv_no_optim + data_no_bbv_optim + data_with_bbv

    def pick_version_name(run):
        if run.version_limit == 0:
            base = "No BBV"
        else:
            base = f"{run.version_limit}"

        if run.compiler_optimizations:
            base += "/optim"

        return base

    versions = [pick_version_name(r) for r, *_ in data]
    measures = collections.defaultdict(list)

    first = data[0]

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

    if (num_bars + n_versions) < 30:
        width = 0.2
    elif (num_bars + n_versions) < 60:
        width = 0.23
    else:
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

def choose_analysis_output_path(output, system_name, compiler_name, safe_arithmetic):
    return choose_path(f'analysis', output, system_name, compiler_name, safe_arithmetic)


def choose_specific_metric_output_path(output, metric, system_name, compiler_name, safe_arithmetic):
    return choose_path(f'{metric}_analysis', output, system_name, compiler_name, safe_arithmetic)

def get_row_name(run):
    base = ""

    if (limit := run.version_limit) == 0:
        base += "0"
    else:
        base += str(limit)

    if run.compiler_optimizations:
        base += "+"

    return base

def row_name_key(name):
    if name.endswith("+"):
        return int(name[:-1]), True
    else:
        return int(name), False

def compute_ratio_dataframe(benchmark_runs, perf_attributes, other_attributes, pseudo_ratio_offset=1):
    benchmark_runs = list(benchmark_runs)
    rows = {}

    logger.info(f"Computing ratios for {benchmark_runs[0].benchmark.name}")

    for run in benchmark_runs:
        row = []
        rows[get_row_name(run)] = row

        for perf_attr in perf_attributes:
            row.append(get_perf_event_statistics(run, perf_attr).value)

        for other_attr in other_attributes:
            row.append(get_other_measure_statistics(run, other_attr).value)

    rows = {k: rows[k] for k in sorted(rows, key=row_name_key)}

    df = pd.DataFrame(np.nan, index=rows.keys(), columns=perf_attributes + other_attributes)

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


def analyze_specific_metric(output, system, compiler, safe_arithmetic, runs, perf_attributes, other_attributes):
    def _get_result(run, attr, kind):
        if kind is PerfEvent:
            events = select(p for p in PerfEvent if p.event == attr and p.run == run)[:]
        elif kind is OtherMeasure:
            events = select(p for p in OtherMeasure if p.name == attr and p.run == run)[:]
        else:
            raise NotImplementedError

        mean = statistics.mean(e.value for e in events)
        stdev = statistics.stdev(e.value for e in events) if len(events) > 2 else 0
        return ChartValue(mean, stdev)
    
    for event, get_result in [(p, lambda run, attr: _get_result(run, attr, PerfEvent)) for p in perf_attributes] + \
                 [(p, lambda run, attr: _get_result(run, attr, OtherMeasure)) for p in other_attributes]:
        output_path = choose_specific_metric_output_path(output=output,
                                                         metric=event,
                                                         system_name=system.name,
                                                         compiler_name=compiler.name,
                                                         safe_arithmetic=safe_arithmetic)

        benchmarks = list(set(r.benchmark.name for r in runs))

        def get_bench_index(benchmark_name):
            return benchmarks.index(benchmark_name)

        def get_mean_index():
            return -1

        def pseudo_ratio(x, y):
            return (x + 1) / (y + 1)

        def geometric_mean(values):
            return math.prod(values) ** (1 / len(values))

        rows = collections.defaultdict(lambda: [None] * (len(benchmarks) + 1))

        for benchmark_name in benchmarks:
            benchmark_runs = sorted([r for r in runs if r.benchmark.name == benchmark_name],
                                    key=lambda r: (r.version_limit, r.compiler_optimizations))
            bench_index = get_bench_index(benchmark_name)

            first_result = get_result(benchmark_runs[0], event)

            for run in benchmark_runs:
                result = get_result(run, event)
                row_name = get_row_name(run)
                rows[row_name][bench_index] = pseudo_ratio(result.value, first_result.value)

        for row in rows.values():
            row[get_mean_index()] = geometric_mean(row[:-1])

        # Plot
        fig, ax = plt.subplots(figsize=(15, 5))

        def clamp(x):
            return min(2, max(0, x))

        vmin = clamp(min(value for value in row for row in rows.values()))
        vmax = clamp(max(value for value in row for row in rows.values()))

        sorted_row_names = sorted(rows.keys(), key=row_name_key)
        table = [rows[k] for k in sorted_row_names]

        table = pd.DataFrame(table, columns=[*benchmarks, "Geometric mean"], index=sorted_row_names)

        heatmap_ax = sns.heatmap(table, annot=True, fmt='.2g', cmap="coolwarm",
                                 linewidths=.5, vmin=vmin, vmax=vmax, center=1)

        for text in heatmap_ax.texts:
            text.set_size(7)

        plt.title(f"Mean ratio for {repr(event)}", fontsize=16, fontweight='bold')

        ax.xaxis.tick_top()
        plt.xticks(rotation=35, rotation_mode="anchor", ha='left')

        text_size = 12 if len(benchmarks) < 5 else 12 / (len(benchmarks) / 8)
        ax.text(0.5, -0.1, f"benchmarks: {', '.join(benchmarks)}",
                size=text_size, ha="center", transform=ax.transAxes)

        plt.tight_layout()

        ensure_directory_exists(output_path)
        plt.savefig(output_path)


@db_session
def analyze(benchmark_names, system_name, compiler_name, safe_arithmetic, output):
    system = get_system_from_name_or_default(system_name)
    compiler = get_compiler_from_name(compiler_name)

    output_path = choose_analysis_output_path(output=output,
                                              system_name=system.name,
                                              compiler_name=compiler.name,
                                              safe_arithmetic=safe_arithmetic)

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
                      and r.system == system and r.compiler == compiler):
            benchmarks_filter.remove(benchmark_name)
            logger.warning(f"benchmark does not exist for the provided settings: {repr(benchmark_name)}")

    logger.debug(f"benchmarks used: {', '.join(benchmarks_filter)}")

    # Select only the latest runs for a each benchmark
    runs = list(select(
        r for r in Run
        if r.system == system and r.compiler == compiler
        and r.version_limit == 0
        and r.benchmark.name in benchmarks_filter
        and r.safe_arithmetic == safe_arithmetic
        #and (not r.compiler_optimizations or r.version_limit == 0)
        and r.timestamp == max(select(
            r2.timestamp for r2 in Run
            if r2.system == system and r2.compiler == compiler
            and r2.version_limit == 0
            and r2.benchmark == r.benchmark
            and r2.version_limit == r.version_limit
            and r2.safe_arithmetic == safe_arithmetic
            and r2.compiler_optimizations == r.compiler_optimizations))).order_by(Run.benchmark))

    runs = [r for r in runs if r.benchmark.name not in ("mbrot", "array1", "sumfp", "sum")]

    found_benchmark_names = sorted(set(r.benchmark.name for r in runs))

    logger.info(f"found {len(runs)} (only latest)")

    # Format data in a pandas dataframe
    perf_attributes = list(select(e.event for e in PerfEvent if e.run in runs).distinct())

    # Remove attributes that perf could not recover on all runs
    perf_attributes = [p for p in perf_attributes if all(get_perf_event_statistics(run, p, None) for run in runs)]

    logger.info(f"perf attributes in analysis: {', '.join(perf_attributes)}")

    other_attributes = list(select(m.name for m in OtherMeasure if m.run in runs).distinct())

    other_attributes = [o for o in other_attributes if not o.startswith('scheme')]

    logger.info(f"other measures in analysis: {', '.join(perf_attributes)}")

    analyze_specific_metric(output, system, compiler, safe_arithmetic, runs, perf_attributes, other_attributes)

    data_points = {}

    benchmarks_groups = itertools.groupby(runs, key=lambda r: r.benchmark.id)  # group runs by version

    ratio_dataframes = [compute_ratio_dataframe(group, perf_attributes, other_attributes)
                        for _, group in benchmarks_groups]

    # Compute the mean along the third axis, ignoring NaN values
    df_average_ratio = pd.DataFrame(np.prod([df.values for df in ratio_dataframes], axis=0),
                                    columns=ratio_dataframes[0].columns) ** (1 / len(ratio_dataframes))
    df_average_ratio.index = ratio_dataframes[0].index

    # Plot
    fig, ax = plt.subplots(figsize=(15, 5))

    def clamp(x):
        return min(2, max(0, x))

    vmin = clamp(np.nanmin(df_average_ratio.values))
    vmax = clamp(np.nanmax(df_average_ratio.values))

    heatmap_ax = sns.heatmap(df_average_ratio, annot=True, fmt='.2g', cmap="coolwarm", linewidths=.5, vmin=vmin, vmax=vmax, center=1)

    for text in heatmap_ax.texts:
        text.set_size(7)

    plt.title(f"Mean ratio", fontsize=16, fontweight='bold')

    ax.xaxis.tick_top()
    plt.xticks(rotation=35, rotation_mode="anchor", ha='left')
    
    text_size = 12 if len(found_benchmark_names) < 5 else 12 / (len(found_benchmark_names) / 5)
    ax.text(0.5, -0.1, f"benchmarks: {', '.join(found_benchmark_names)}",
            size=text_size, ha="center", transform=ax.transAxes)

    plt.tight_layout()

    ensure_directory_exists(output_path)
    plt.savefig(output_path)


##############################################################################
# Perf data distribution
##############################################################################

def choose_distribution_output_path(output, system_name, compiler_name, safe_arithmetic, benchmark, version_limit, perf_event):
    base = f"distribution_{benchmark}_V{version_limit}_{perf_event}"
    return choose_path(base, output, system_name, compiler_name, safe_arithmetic)

@db_session
def perf_distribution(system_name,
                      compiler_name,
                      benchmark_name,
                      event,
                      version_limit,
                      output):
    system = get_system_from_name_or_default(system_name)
    compiler = get_compiler_from_name(compiler_name)

    output_path = choose_distribution_output_path(output, system.name,
                                                  compiler.name, benchmark_name, version_limit, event)

    benchmark = Benchmark.get(name=benchmark_name)

    run = Run.select(lambda r: r.system == system and
                     r.compiler == compiler and
                     r.benchmark == benchmark and
                     r.version_limit == version_limit).order_by(desc(Run.timestamp)).first()

    # sorted = lambda x: x
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
            return path.resolve()
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

    compiler_parser_group = benchmark_parser.add_mutually_exclusive_group(required=True)

    compiler_parser_group.add_argument('-g', '--gambit-dir',
                                       dest='gambitdir',
                                       type=directory_path,
                                       metavar='PATH',
                                       help='Gambit root')

    compiler_parser_group.add_argument('-b', '--bigloo',
                                       dest='use_bigloo',
                                       action='store_true',
                                       help='Use Bigloo')

    benchmark_parser.add_argument('-l', '--limit',
                                  dest="version_limits",
                                  metavar="LIMIT",
                                  nargs="+",
                                  default=(0, 1, 2, 3, 4, 5),
                                  type=int,
                                  help="BBV versions limits")

    benchmark_parser.add_argument('-u', '--unsafe',
                                  dest="safe_arithmetic",
                                  metavar='U',
                                  action='store_false',
                                  default=True,
                                  type=bool,
                                  help="Execute benchmark with unsafe arithmetic")

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

    benchmark_parser.add_argument('-O', '--compiler-optimizations',
                                  dest='compiler_optimizations',
                                  action='store_true',
                                  default=False,
                                  help='Compile benchmark with compiler optimizations')

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

    plot_parser.add_argument('-u', '--unsafe',
                             dest="safe_arithmetic",
                             metavar='U',
                             action='store_false',
                             default=True,
                             type=bool,
                             help="benchmarks with unsafe arithmetic")

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

    analysis_parser.add_argument('-s', '--systen',
                             dest="system_name",
                             help="analyze benchmarks from this system")

    analysis_parser.add_argument('-c', '--compiler',
                             dest="compiler_name",
                             default="gambit",
                             help="analyze benchmarks of this compiler")

    analysis_parser.add_argument('-u', '--unsafe',
                                 dest="safe_arithmetic",
                                 metavar='U',
                                 action='store_false',
                                 default=True,
                                 type=bool,
                                 help="Benchmark with unsafe arithmetic")

    analysis_parser.add_argument('-b', '--benchmarks',
                                 dest="benchmark_names",
                                 nargs='+',
                                 help="benchmarks used for analysis (all if none given)")

    analysis_parser.add_argument('-o', '--output',
                                dest="output",
                                help="where to output the chart (file or folder)")

    perf_distribution_parser = subparsers.add_parser('perf_distribution', help='Plot perf distribution histogram')

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
        run_and_save_benchmark(gambitdir=args.gambitdir,
                               use_bigloo=args.use_bigloo,
                               file=args.file,
                               version_limits=args.version_limits,
                               safe_arithmetic=safe_arithmetic,
                               repetitions=args.repetitions,
                               compiler_optimizations=args.compiler_optimizations,
                               force_execution=args.force_execution,
                               timeout=args.timeout)
    elif args.command == 'plot':
        plot_benchmarks(benchmark=args.benchmark,
                        compiler_name=args.compiler_name,
                        system_name=args.system_name,
                        safe_arithmetic=safe_arithmetic,
                        perf_event_names=args.perf_event_names,
                        primitive_names_or_amount=args.primitive_names_or_amount,
                        other_measure_names=args.other_measure_names,
                        output=args.output)
    elif args.command == 'analysis':
        analyze(
            system_name=args.system_name,
            compiler_name=args.compiler_name,
            benchmark_names=args.benchmark_names,
            safe_arithmetic=safe_arithmetic,
            output=args.output)
    elif args.command == 'perf_distribution':
        perf_distribution(system_name=args.system_name,
                          compiler_name=args.compiler_name,
                          benchmark_name=args.benchmark_name,
                          event=args.event,
                          version_limit=args.version_limit,
                          output=args.output)
    else:
        parser.print_help()


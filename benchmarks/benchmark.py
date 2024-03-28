#!/usr/bin/env python3

import argparse
import ast
import collections
import csv
import datetime
import itertools
import json
import locale
import logging
import math
import operator
import os
import pathlib
import platform
import re
import shlex
import stat
import statistics
import string
import subprocess
import sys
import time

try:
    import distro
except ImportError:
    distro = None

import matplotlib.pyplot as plt
import matplotlib as mpl
from matplotlib import colors
from matplotlib.ticker import FuncFormatter
from matplotlib.ticker import LogLocator

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
    path = Optional(str)
    commit_sha = Required(str)
    commit_description = Required(str)
    commit_author = Required(str)
    commit_timestamp = Required(int)
    runs = Set('Run')

    @classmethod
    def get_or_create_compiler(cls, compiler):
        compilerdir = compiler.path
        # Define the format for the commit details we want
        # %H: commit hash, %an: author name, %s: subject, %ct: committer date (Unix timestamp)
        format_str = "%H%n%an%n%s%n%ct"
        name = compiler.name

        try:
            output = subprocess.check_output(['git', 'show', '-s', f'--format={format_str}'],
                                            cwd=compilerdir, universal_newlines=True).strip()
            sha, author, description, timestamp = output.splitlines()
        except (ValueError, TypeError):
            sha, author, description, timestamp = "????"

        logger.debug(f"current compiler is: {name}, {sha}, {author}, {description}, {timestamp}")

        return Compiler.get_or_create(name=name,
                                      path=compiler.path,
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

TYPECHECK_NAMES = (
        # Gambit             # Bigloo
        # Typechecks
        "##fixnum?",
        '##flonum?',
        "##vector?",
        "##pair?",         
        "##procedure?",
        "##bignum?",
        "##boolean?",
        "##string?",
        "##symbol?",
        "##char?",
        "##null?",
        # Overflow checks
        "##fx+?",            "add/ov",
        "##fx-?",            "sub/ov",
        "##fx*?",            "mul/ov",
        "##fxabs?",
        "##fxarithmetic-shift-left?",
        "##fxarithmetic-shift-right?",
        "##fxarithmetic-shift?",
        "##fxsquare?",
        "##fxwraparithmetic-shift-left?",
        "##fxwraparithmetic-shift?",
        "##fxwraplogical-shift-right?",)
TYPECHECK_NAMES = TYPECHECK_NAMES + tuple(n.replace("##", "") for n in TYPECHECK_NAMES) + \
                                    tuple(n.replace("##", "$") for n in TYPECHECK_NAMES)

class PrimitiveCount(db.Entity):
    name = Required(str)
    value = Required(int, size=64)
    run = Required('Run')

    @property
    def is_typecheck(self):
        return self.name in TYPECHECK_NAMES

class PerfEvent(db.Entity):
    event = Required(str)
    value = Required(float)
    rep = Required('Repetition')

    @property
    def run(self):
        return self.rep.run

class CompilerStatistics(db.Entity):
    name = Required(str)
    value = Required(float)
    rep = Required('Repetition')

    @property
    def run(self):
        return self.rep.run

class StaticMeasure(db.Entity):
    name = Required(str)
    value = Required(float)
    run = Required('Run')

class Repetition(db.Entity):
    run = Required('Run')
    perf_events = Set('PerfEvent', reverse='rep')
    compiler_statistics = Set('CompilerStatistics', reverse='rep')
    timestamp = Required(int, default=lambda: int(time.time()))

class Run(db.Entity):
    benchmark = Required('Benchmark', reverse='runs')
    system = Required('System', reverse='runs')
    compiler = Required('Compiler', reverse='runs')
    compiler_optimizations = Required(bool)
    version_limit = Required(int)
    safe_arithmetic = Required(bool)
    reps = Set('Repetition', reverse='run')
    primitives = Set('PrimitiveCount', reverse='run')
    static_measures = Set('StaticMeasure', reverse='run')
    arguments = Required(str)
    timestamp = Required(int, default=lambda: int(time.time()))

    @classmethod
    def get_latest(cls, **kwargs):
        entities = list(cls.select(**kwargs))
        if not entities:
            logger.debug(f"get_latest(**{kwargs})")
            raise ValueError("Cannot get latest, none exists")
        return max(entities, key=lambda e: e.timestamp)


db.bind(provider='sqlite', filename='benchmarks.db', create_db=True)
db.generate_mapping(create_tables=True)

##############################################################################
# Utils
##############################################################################

def run_command(command, timeout, env=None, extend_env=None):
    logger.info(command)

    env = env = os.environ.copy() if env is None else env.copy()

    if extend_env:
        env.update(extend_env)

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
            logger.error(f"{cls.DEFAULT_PRIMITIVE_COUNTER_MARKER} not found in compiler output")
            counter_section = ""
        else:
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
    time_event = 'real-time'
    _time_event_real_name = 'time elapsed'
    event_names = [
        "task-clock",
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

        self.events[self.time_event] = self._get_value(perf_output, self._time_event_real_name)
        
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
                value_number = cls._string_to_number(value)
                variance_number = cls._string_to_number(variance)
                logger.debug(f"converted to ({value_number} +- {variance_number})")
                return value_number, variance_number

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


SCHEME_COMPILE_TIME = "scheme-compile-time"
C_COMPILE_TIME = "c-compile-time"
TOTAL_COMPILE_TIME = "compile-time"

def extract_compile_times_from_compiler_output(content, dummy=False):
    if dummy:
        return {
        SCHEME_COMPILE_TIME: 0,
        C_COMPILE_TIME: 0
        }

    scheme_time = re.search(r"\*\*\*scheme-compile-time: (.+)", content).group(1)
    c_time = re.search(r"\*\*\*c-compile-time: (.+)", content).group(1)
    return {
        SCHEME_COMPILE_TIME: float(scheme_time),
        C_COMPILE_TIME: float(c_time)
        }


def compile_gambit(compiler, file, vlimit, safe_arithmetic, compiler_optimizations, timeout=None):
    env = os.environ.copy()

    base_command = get_compiler_command(compiler, file, vlimit, safe_arithmetic, compiler_optimizations, False)
    command_with_primitives = get_compiler_command(compiler, file, vlimit, safe_arithmetic, compiler_optimizations, True)

    output_with_primitives = run_command(command_with_primitives, timeout, env)
    primitive_count = PrimitivesCountParser(output_with_primitives)

    if not primitive_count:
        logger.warning("Failed to parse primitive count")
    else:
        logger.debug(f"Primitive count: {dict(primitive_count)}")

    timed_command = f"perf stat {base_command}"
    output = run_command(timed_command, timeout, env)
    compile_times = extract_compile_times_from_compiler_output(output)
    executable = extract_executable_from_compiler_output(output)

    logger.info(f"executable created at: {executable}")    

    return executable, primitive_count, compile_times

def get_compiler_command(compiler, file, vlimit, safe_arithmetic, compiler_optimizations, primitive_count):
    optimization_flag = "-O3" if compiler_optimizations else ""
    primitive_count_flag = '-P' if primitive_count else ''
    arithmetic_flag = '-U' if not safe_arithmetic else ''
    path_flag = f'-D {compiler.path}' if compiler.path else ''
    return f"{COMPILE_SCRIPT} -S {compiler.name} {path_flag} -V {vlimit} {arithmetic_flag} {optimization_flag} {primitive_count_flag} -f {file}"

def compile_bigloo(compiler, file, vlimit, safe_arithmetic, compiler_optimizations, timeout=None):
    # First execution with primitive count
    command_with_primitives = get_compiler_command(compiler, file, vlimit, safe_arithmetic, compiler_optimizations, True)

    output = run_command(command_with_primitives, timeout)

    executable = extract_executable_from_compiler_output(output)

    executable_output = run_command(executable, timeout)

    primitive_count = PrimitivesCountParser(executable_output)

    # Second execution without primitive count
    base_command = get_compiler_command(compiler, file, vlimit, safe_arithmetic, compiler_optimizations, False)
    timed_command = f"perf stat {base_command}"
    output = run_command(timed_command, timeout)
    compile_times = extract_compile_times_from_compiler_output(output)
    executable = extract_executable_from_compiler_output(output)

    logger.info(f"executable created at: {executable}")

    if not primitive_count:
        logger.warning("Failed to parse primitive count")
    else:
        logger.debug(f"Primitive count: {dict(primitive_count)}")

    return executable, primitive_count, compile_times

def compile_other(compiler, file, vlimit, safe_arithmetic, compiler_optimizations, timeout=None):
    command = get_compiler_command(compiler, file, vlimit, safe_arithmetic, compiler_optimizations, False)
    output = run_command(command, timeout)
    executable = extract_executable_from_compiler_output(output)
    return executable, PrimitivesCountParser(""), extract_compile_times_from_compiler_output("", dummy=True)

def compile(compiler, file, vlimit, safe_arithmetic, compiler_optimizations, timeout=None):
    if compiler.name == "bigloo":
        return compile_bigloo(compiler, file, vlimit, safe_arithmetic, compiler_optimizations, timeout)
    elif compiler.name == "gambit":
        return compile_gambit(compiler, file, vlimit, safe_arithmetic, compiler_optimizations, timeout)
    else:
        return compile_other(compiler, file, vlimit, safe_arithmetic, compiler_optimizations, timeout)

def run_benchmark(executable, arguments, timeout=None, env=None):
    # Run program to measure time only
    time_command = f"perf stat {executable} {arguments}"
    time_output = run_command(time_command, timeout, extend_env=env)

    # Run program with all perf stat events on
    all_events = ' '.join(f"-e {e}" for e in PerfResultParser.event_names)
    all_command = f"perf stat {all_events} {executable} {arguments}"
    all_output = run_command(all_command, timeout, extend_env=env)

    # parse and join outputs
    time_parser = PerfResultParser(time_output)
    all_perf_parser = PerfResultParser(all_output)

    all_perf_parser.update(time_parser)

    for event in PerfResultParser.event_names:
        if event not in all_perf_parser:
            logger.debug(f"Could not find perf stat event {repr(event)} when running {executable}")

    scheme_parser = SchemeStatsParser(time_output)

    return all_perf_parser, scheme_parser

default_arguments = "repeat: 10"

benchmark_args = {
    "ack": "repeat: 50 m: 3 n: 9",
    "bague": "repeat: 1 nombre-de-pierres: 28",
    "fib": "repeat: 3 n: 39",
    "fibfp": "repeat: 2 n: 39.0",
    "tak": "repeat: 10000 x: 18 y: 12 z: 6",
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
    "primes": "repeat: 1000000",
    "rev": "repeat: 100000000",
    "vlen": "repeat: 100000000",
    "boyer": "repeat: 1 n: 500",
    "earley": "repeat: 1 n: 10000",
    "compiler": "repeat: 1 n: 2000",
    "dynamic": "repeat: 1 n: 200",
    "scheme": "repeat: 1 n: 100000",
    "nucleic": "repeat: 1 n: 50",
    "conform": "repeat: 1 n: 1000",
    "maze": "repeat: 1 n: 50000",
    "peval": "repeat: 1 n: 3000",
    "leval": "repeat: 1 n: 60",
    "slatex": "repeat: 1 n: 10000",
}

def convert_to_node_arguments(arguments):
    arg_map = dict([a.split(':') for a in arguments.replace(": ", ":").split()])
    return repr(json.dumps({k.replace("-", "_"): v for k, v in arg_map.items()}))

def get_gambit_program_size(executable, benchmark):
    objdump_command = f"objdump --disassemble {executable} | sed -e '/ <.*>:/!d'"
    logger.info(objdump_command)
    objdump_output = subprocess.run(objdump_command, shell=True, capture_output=True).stdout.decode()
    logger.debug(objdump_output)
    lines = objdump_output.splitlines()

    marker_name = benchmark.name.replace("-", "_2d_") # TODO: support all special characters

    start_marker = f"<___H_{marker_name}>"
    end_marker = f"<___LNK_{marker_name}>"

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
    if 'gambit' in compiler.name:
        return get_gambit_program_size(executable, benchmark)
    elif 'bigloo' in compiler.name:
        return get_bigloo_program_size(executable)
    else:
        logger.error("unknown compiler, cannot retrieve size")
        return 0

def get_or_create_run_compiler(compiler):
    if compiler.path:
        compiler, _ = Compiler.get_or_create_compiler(compiler)
        return compiler
    else:
        compiler, _ = Compiler.get_or_create(name=compiler.name,
                                             commit_sha='?',
                                             commit_description='?',
                                             commit_author='?',
                                             commit_timestamp=-1)
        return compiler

@db_session
def run_and_save_benchmark(compiler, file, version_limits, safe_arithmetic, repetitions, compiler_optimizations, force_execution=False, timeout=None):
    system, _ = System.get_or_create_current_system()
    compiler = get_or_create_run_compiler(compiler)

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
            logger.warning(f"no CLI argument for {repr(benchmark.name)}")
            base_arguments = default_arguments

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

        executable, primitive_count, compile_times = compile(compiler, file, v, safe_arithmetic, compiler_optimizations, timeout)

        scheme_compile_time = compile_times[SCHEME_COMPILE_TIME]
        c_compile_time = compile_times[C_COMPILE_TIME]
        total_compile_time = scheme_compile_time + c_compile_time

        logger.debug(f"compilation time: {total_compile_time}")
        StaticMeasure(name=TOTAL_COMPILE_TIME, value=total_compile_time, run=run)
        StaticMeasure(name=SCHEME_COMPILE_TIME, value=scheme_compile_time, run=run)
        StaticMeasure(name=C_COMPILE_TIME, value=c_compile_time, run=run)

        # Set heap size of all benchmarks to 100M
        env=None
        if compiler.name == "gambit":
            arguments = f"-:m100M {arguments}"
        elif compiler.name == "bigloo":
            env = {"BIGLOOHEAP": 100}
        elif compiler.name == 'node':
            pass # Done by compilation script
        elif compiler.name == 'chez':
            pass # cannot adjust heap
        elif compiler.name == 'racket':
            pass # cannot adjust heap
        else:
            logger.error(f'cannot set heap size for {compiler.name}')

        align_stack_step = 3
        for i in range(repetitions):
            arguments = f"{base_arguments} align-stack: {i * align_stack_step}"
            if compiler.name == "node":
                arguments = convert_to_node_arguments(arguments)
            perf_results, scheme_stats = run_benchmark(executable, arguments, timeout=timeout, env=env)

            rep = Repetition(run=run)

            for event, (value, variance) in perf_results.items():
                PerfEvent(event=event, value=value, rep=rep)

            for name, value in scheme_stats.items():
                logger.info(f"saving scheme stat {repr(name)}: {value}")
                CompilerStatistics(name=name, value=value, rep=rep)

        typechecks = 0

        for prim, value in primitive_count.items():
            p = PrimitiveCount(name=prim, value=value, run=run)
            typechecks += p.is_typecheck * value

        logger.debug(f"number of typechecks: {typechecks}")
        StaticMeasure(name="typechecks", value=typechecks, run=run)

        total_primitives = sum(primitive_count.values())
        logger.debug(f"executed primitives: {total_primitives}")
        StaticMeasure(name='primitives', value=total_primitives, run=run)

        if primitive_count.size_in_gvm_instructions:
            logger.debug(f"gvm instruction size: {primitive_count.size_in_gvm_instructions}")
            StaticMeasure(name='gvm-size', value=primitive_count.size_in_gvm_instructions, run=run)
        else:
            logger.debug("Did not find count for total GVM instructions")

        size = get_program_size(executable, benchmark, compiler)
        if size:
            logger.debug(f"program size: {size}")
            StaticMeasure(name='program-size', value=size, run=run)
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
def plot_benchmarks(system_name, compiler_name, benchmark, safe_arithmetic, perf_event_names, primitive_names_or_amount, static_measures_names, output):
    system = get_system_from_name_or_default(system_name)
    compiler = get_compiler_from_name(compiler_name)

    # Select only the latest runs for a given version limit
    runs = select(
        r for r in Run
        if r.system == system and r.compiler == compiler and r.benchmark.name == benchmark
           and r.safe_arithmetic == safe_arithmetic
        and r.compiler_optimizations
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

    data = extract_data_from_runs(runs, perf_event_names, primitive_names, static_measures_names)
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


def get_static_measures_statistics(run, name, default=_sentinel):
    events = select(e for e in StaticMeasure if e.name == name and e.run == run)
    values = [e.value for e in events]
    return sanitize_stats(name, values, default)


def extract_data_from_runs(runs, perf_event_names, primitive_names, static_measures_names):
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

        static_measures = {}

        for name in static_measures_names:
            if not (measure := StaticMeasure.get(run=run, name=name)):
                raise ValueError(f'Cannot find {repr(name)}')
            static_measures[name] = ChartValue(measure.value)

        data.append((run, perf_events, primitive_counts, static_measures))

    return data


def plot_data(data, output_path):
    # Compute colors
    n_perf_stats = max((len(p) for _, p, _, _ in data), default=0)
    n_primitive_stats = max((len(p) for _, _, p, _ in data), default=0)
    n_static_measures = max((len(m) for _, _, _, m in data), default=0)
    n_measurments = n_perf_stats + n_primitive_stats + n_static_measures

    perf_colors = mpl.colormaps.get_cmap('winter')(np.linspace(0.3, 0.8, n_perf_stats))
    primitive_colors = mpl.colormaps.get_cmap('autumn')(np.linspace(0.3, 0.8, n_primitive_stats))
    static_measures_colors = mpl.colormaps.get_cmap('summer')(np.linspace(0.3, 0.8, n_static_measures))

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
                                               itertools.chain(perf_colors, primitive_colors, static_measures_colors)):
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

def compute_ratio_dataframe(benchmark_runs, perf_attributes, static_attributes, pseudo_ratio_offset=1):
    benchmark_runs = list(benchmark_runs)
    rows = {}

    logger.info(f"Computing ratios for {benchmark_runs[0].benchmark.name}")

    for run in benchmark_runs:
        row = []
        rows[get_row_name(run)] = row

        for perf_attr in perf_attributes:
            row.append(get_perf_event_statistics(run, perf_attr).value)

        for static_attr in static_attributes:
            row.append(get_static_measures_statistics(run, static_attr).value)

    rows = {k: rows[k] for k in sorted(rows, key=row_name_key)}

    df = pd.DataFrame(np.nan, index=rows.keys(), columns=perf_attributes + static_attributes)

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


def analyze_specific_metric(output, system, compiler, safe_arithmetic, runs, perf_attributes, static_attributes):
    def _get_result(run, attr, kind):
        if kind is PerfEvent:
            events = select(p for p in PerfEvent if p.event == attr and p.run == run)[:]
        elif kind is StaticMeasure:
            events = select(p for p in StaticMeasure if p.name == attr and p.run == run)[:]
        else:
            raise NotImplementedError

        mean = statistics.mean(e.value for e in events)
        stdev = statistics.stdev(e.value for e in events) if len(events) > 2 else 0
        return ChartValue(mean, stdev)
    
    for event, get_result in [(p, lambda run, attr: _get_result(run, attr, PerfEvent)) for p in perf_attributes] + \
                 [(p, lambda run, attr: _get_result(run, attr, StaticMeasure)) for p in static_attributes]:
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
                        and r.system == system and r.compiler.name == compiler_name):
            benchmarks_filter.remove(benchmark_name)
            logger.warning(f"benchmark does not exist for the provided settings: {repr(benchmark_name)}")

    logger.debug(f"benchmarks used: {', '.join(benchmarks_filter)}")

    # Select only the latest runs for a each benchmark
    runs = list(select(
        r for r in Run
        if r.system == system and r.compiler.name == compiler_name
        and r.benchmark.name in benchmarks_filter
        and r.safe_arithmetic == safe_arithmetic
        and r.compiler_optimizations
        and r.timestamp == max(select(
            r2.timestamp for r2 in Run
            if r2.system == system and r2.compiler.name == compiler_name
            and r2.version_limit == r.version_limit
            and r2.benchmark == r.benchmark
            and r2.version_limit == r.version_limit
            and r2.safe_arithmetic == safe_arithmetic
            and r2.compiler_optimizations))).order_by(Run.benchmark))

    runs = [r for r in runs if r.benchmark.name not in ("mbrot", "array1", "sumfp", "sum")]

    found_benchmark_names = sorted(set(r.benchmark.name for r in runs))

    logger.info(f"found {len(runs)} (only latest)")

    # Format data in a pandas dataframe
    perf_attributes = list(select(e.event for e in PerfEvent if e.run in runs).distinct())

    # Remove attributes that perf could not recover on all runs
    perf_attributes = [p for p in perf_attributes if all(get_perf_event_statistics(run, p, None) for run in runs)]

    logger.info(f"perf attributes in analysis: {', '.join(perf_attributes)}")

    static_attributes = list(select(m.name for m in StaticMeasure if m.run in runs).distinct())

    logger.info(f"static measures in analysis: {', '.join(perf_attributes)}")

    analyze_specific_metric(output, system, compiler, safe_arithmetic, runs, perf_attributes, static_attributes)

    data_points = {}

    benchmarks_groups = itertools.groupby(runs, key=lambda r: r.benchmark.id)  # group runs by version

    ratio_dataframes = [compute_ratio_dataframe(group, perf_attributes, static_attributes)
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
# CSV dump
##############################################################################
    

def choose_csv_output_path(output, system_name):
    path = pathlib.Path(output or '.').resolve()
    suffix = path.suffix

    if not suffix:
                # No extension means the output is a folder where to output the plot
        logger.debug(f"output into folder {path}")

        # build default filename
        filename = f"results_{system_name}.csv"

        # sanitize filename
        filename = sanitize_filename(filename)

        path = path / filename
        logger.info(f"output to file {path}")
        return path
    elif suffix == ".csv":
        logger.info(f"output to file {path}")
        return path
    else:
        raise ValueError(f"output must be a folder of .csv target, got {output}")


def is_macro(name):
    return name in ('almabench', 'boyer', 'compiler', 'conform', 'dynamic',
                    'earley', 'leval', 'maze', 'nucleic', 'peval', 'scheme', 'slatex')

def run_is_macro(run):
    return is_macro(run.benchmark.name)

def nan_on(*exceptions):
    def wrapper(getter):
        def inner_wrapper(*args, **kwargs):
            try:
                return getter(*args, **kwargs)
            except exceptions:
                return math.nan
        return inner_wrapper
    return wrapper

def average_time(run):
    results = select(e.value for e in PerfEvent if e.event == PerfResultParser.time_event and e.run == run)
    return statistics.mean(results)

def compile_time(run):
    results = StaticMeasure.get(name=TOTAL_COMPILE_TIME, run=run)
    return results.value

def scheme_compile_time(run):
    results = StaticMeasure.get(name=SCHEME_COMPILE_TIME, run=run)
    return results.value


def c_compile_time(run):
    results = StaticMeasure.get(name=C_COMPILE_TIME, run=run)
    return results.value

def stdev_time(run):
    results = select(e.value for e in PerfEvent if e.event == PerfResultParser.time_event and e.run == run)
    return 0 if len(results) == 1 else statistics.stdev(results)

def sum_checks(run):
    primitive_counts = select(e for e in PrimitiveCount if e.run == run)
    results = [p.value for p in primitive_counts if p.is_typecheck]
    return sum(results)

@nan_on(ValueError)
def sum_removable_checks(run):
    unsafe_run = Run.get_latest(compiler=run.compiler, system=run.system, safe_arithmetic=False,
                         version_limit=0, compiler_optimizations=run.compiler_optimizations,
                         benchmark=run.benchmark)

    return sum_checks(run) - sum_checks(unsafe_run)

def run_program_size(run):
    m = StaticMeasure.get(name='program-size', run=run)
    return m.value if m else math.nan

@db_session
def to_csv(system_name, version_limits, output):
    def one_csv_result(compiler_name):
        system = get_system_from_name_or_default(system_name)
        compiler = get_compiler_from_name(compiler_name)

        runs = list(select(
            r for r in Run
            if r.system == system
            and r.compiler.name == compiler_name # TODO switch back to using compiler when no commit-split
            and r.version_limit in version_limits
            and r.compiler_optimizations
            and r.safe_arithmetic
            and r.timestamp == max(select(
                r2.timestamp for r2 in Run
                if r2.system == system
                and r2.compiler.name == compiler_name
                and r2.version_limit == r.version_limit
                and r2.benchmark == r.benchmark
                and r2.version_limit == r.version_limit
                and r2.safe_arithmetic == r.safe_arithmetic
                and r2.compiler_optimizations == r.compiler_optimizations))).order_by(Run.benchmark))

        def get_column_name(limit):
            return f"V={limit}"
        
        def get_run_column_name(run):
            return get_column_name(run.version_limit)

        benchmark_names = set(run.benchmark.name for run in runs)
        benchmark_names = sorted(benchmark_names, key=lambda n: (not is_macro(n), n))

        column_names = ["Benchmark"]
        column_names += [get_column_name(l) for l in version_limits]

        logger.debug(f"columns in csv: {','.join(column_names)}")

        data = []

        def get_measure_data(title, get_measure, get_stdev=lambda r: 0):

            measure_data = []
            measure_data.append([f"{title} - {compiler_name.title()}"])
            measure_data.append(column_names)

            for name in benchmark_names:
                measure_data.append([name] + [math.nan] * (len(column_names) - 1))

            for run in runs:
                row = benchmark_names.index(run.benchmark.name)
                res_col = column_names.index(get_run_column_name(run))
                measure_data[row + 2][res_col] = get_measure(run)

            return measure_data  

        # Time
        logger.debug("Appending 'Execution time'")
        data += get_measure_data("Execution time", average_time, stdev_time)

        # primitive count
        if compiler_name in ("gambit", "bigloo"):
            logger.debug("Appending 'Runtime removable checks'")
            data += get_measure_data("Runtime removable checks", sum_removable_checks)

        return data

    output_path = choose_csv_output_path(output, system_name)
    
    data = []
    for compiler_name in ('bigloo', 'gambit', 'chez', 'node', 'racket'):
        data.extend(one_csv_result(compiler_name))
    
    with open(output_path, 'w') as csv_file:
        csvwriter = csv.writer(csv_file)
        csvwriter.writerows(data)

##############################################################################
# Heatmaps
##############################################################################


def choose_heatmap_output_path(output, measure, system_name, compiler_name):
    path = pathlib.Path(output or '.').resolve()
    suffix = path.suffix

    if not suffix:
        # No extension means the output is a folder where to output the plot
        logger.debug(f"output into folder {path}")

        # build default filename
        filename = f"heatmap_{measure}_{compiler_name}_{system_name}.png"

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
def make_heatmap(system_name, compiler_name, benchmark_names, version_limits, output):
    system = get_system_from_name_or_default(system_name)
    compiler = get_compiler_from_name(compiler_name)

    if benchmark_names is None:
        benchmark_names = list(select(b.name for b in Benchmark).distinct())

    # Select only the latest runs for a each benchmark
    runs = list(select(
        r for r in Run
        if r.system == system and r.compiler.name == compiler_name
        and r.benchmark.name in benchmark_names
        and r.version_limit in version_limits
        and r.safe_arithmetic
        and r.compiler_optimizations
        and r.timestamp == max(select(
            r2.timestamp for r2 in Run
            if r2.system == system and r2.compiler.name == compiler_name
            and r2.version_limit == r.version_limit
            and r2.benchmark == r.benchmark
            and r2.version_limit == r.version_limit
            and r2.safe_arithmetic
            and r2.compiler_optimizations))))

    def get_col_name(run):
        loc = run.benchmark.content.count("\n")        
        loc = round(math.ceil(loc / 10) * 10)
        return f"{run.benchmark.name} ({loc:,} LOC)"

    def get_version_limit_name(v):
        return "No SBBV" if v == 0 else str(v) + " "

    runs.sort(key=lambda r: (not is_macro(r.benchmark.name), r.benchmark.name))

    column_names = [] 
    for r in runs:
        col_name = get_col_name(r)
        if col_name not in column_names:
            column_names.append(col_name)

    row_names = [get_version_limit_name(v) for v in set(r.version_limit for r in runs)]

    def get_pos(run):
        row = row_names.index(get_version_limit_name(run.version_limit))
        column = column_names.index(get_col_name(run))

        return row, column

    def init_data():
        return [[math.nan] * len(column_names) for _ in range(len(row_names))]

    def one_heatmap(path_base, measure, best=False,
                                        include_macro=True,
                                        include_micro=True,
                                        subtract_unsafe_run=False,
                                        unsafe_runs=None,
                                        base_runs=None,
                                        include_geometric_mean=True,
                                        absolute=False,
                                        title=False):
        if base_runs is None:
            base_runs = [r for r in runs if r.version_limit == 0]

        if unsafe_runs:
            unsafe_base_runs = sorted([r for r in unsafe_runs if r.version_limit == 0],
                                      key=lambda r: column_names.index(get_col_name(r)))
        else:
            unsafe_base_runs = None

        if absolute:
            def ratio(value, base, base_run=None):
                return value
        else:
            def ratio(value, base, base_run=None):
                try:
                    return value / base
                except ZeroDivisionError:
                    benchmark_name = "?" if not base_run else (f"{base_run.benchmark.name} "
                                                               f"V={base_run.version_limit} "
                                                               f"U={not base_run.safe_arithmetic} ")
                    logger.error(f"base value is zero for {path_base} on {compiler_name} {benchmark_name}")
                    return math.nan

        if subtract_unsafe_run:
            @nan_on(StopIteration)
            def _measure(run, base_run):
                base_unsafe_run = next(r for r in unsafe_base_runs if run.benchmark == r.benchmark)
                base_unsafe_measure = measure(base_unsafe_run)
                return ratio((measure(run) - base_unsafe_measure), (measure(base_run) - base_unsafe_measure), base_run=base_run)

        else:
            def _measure(run, base_run):
                return ratio(measure(run), measure(base_run))

        
        base_runs = sorted(base_runs,
                           key=lambda r: column_names.index(get_col_name(r)))

        heatmap_row_names = row_names.copy()

        data = init_data()

        if best:
            heatmap_row_names.append("Best")
            data.append([math.nan] * len(column_names))

        for run in runs:
            row, col = get_pos(run)
            base = next((b for b in base_runs if b.benchmark == run.benchmark), None)
            res = _measure(run, base) if base else math.nan
            data[row][col] = res
            if best:
                data[-1][col] = min(res, data[-1][col]) if data[-1][col] != math.nan else res

        df = pd.DataFrame(data, columns=column_names, index=heatmap_row_names)

        if not include_micro:
            cols = df.columns.tolist()
            cols = [n for n in cols if is_macro(n.split()[0])]
            df = df[cols]

        if not include_macro:
            cols = df.columns.tolist()
            cols = [n for n in cols if not is_macro(n.split()[0])]
            df = df[cols]

        mean_name = "Mean" if absolute else "Geometric Mean"
        _mean = statistics.mean if absolute else statistics.geometric_mean

        @nan_on(statistics.StatisticsError)
        def mean(it):
            it = list(it)
            return _mean(it)

        if include_geometric_mean:
            means = [mean(x for x in row if not math.isnan(x)) for i, row in df.iterrows()]
            geo_df = pd.DataFrame(means,
                                  columns=[mean_name],
                         index=heatmap_row_names)
            df = df.join(geo_df)

        
        # Reorder columns according to last entry
        #cols = df.columns.tolist()
        #cols.sort(key=lambda c: (not is_macro(c), c))

        #df = df[cols]

        plt.rc('font', size=10.5)

        fig, ax = plt.subplots(figsize=(15, 5))

        # I made some ranges wider because it adds nice edges to the bar in the legend


        def truncate_colormap(cmap, minval=0.0, maxval=1.0, n=256):
            '''
            https://stackoverflow.com/a/18926541
            '''
            if isinstance(cmap, str): cmap = plt.get_cmap(cmap)
            new_cmap = colors.LinearSegmentedColormap.from_list(
                'trunc({n},{a:.2f},{b:.2f})'.format(n=cmap.name, a=minval, b=maxval),
                cmap(np.linspace(minval, maxval, n)))
            return new_cmap

        cmap_brightness = 0.15
        cmap_base = 'coolwarm'
        dark_cmap = truncate_colormap(cmap_base, 0, 1)
        light_cmap = truncate_colormap(cmap_base, cmap_brightness, 1 - cmap_brightness)

        cmap = light_cmap

        ticks = None
        vmin = df.min().min()
        vmax = df.max().max()
        linthresh = 0.1
        extra_ticks = []

        def remove_red():
            nonlocal cmap
            checks_interp = np.interp(
                np.linspace(0, 1),
                [0, 0.5, 1],
                [0, 0.5, 0.5])
            cmap = colors.LinearSegmentedColormap.from_list("checks_cmap", cmap(checks_interp))

        if path_base == 'time':
            vmin, vmax = 0.7, 1.1
            extra_ticks = [0.8, 0.9]
        elif path_base == 'micro_time':
            vmin, vmax = 0.5, 1.0
            extra_ticks = [0.75]
            remove_red()
        elif path_base == 'program_size':
            vmin, vmax = 0.5, 8
        elif path_base == 'checks':
            vmin, vmax = 0.1, 1.1
            # Remove red from cmap. It must not appear in color bar since it cannot happen
            remove_red()
        elif path_base == "compile_time":
            vmin, vmax = 0.5, 32
            
        locator = LogLocator(base=2)
        ticks = [t for t in locator.tick_values(vmin, vmax) if vmin <= t <= vmax]
        ticks = sorted({1, *ticks})
        if len(ticks) == 1:
            ticks = sorted({1, vmin, vmax})
        ticks.extend(extra_ticks)

        center = 1
        tick_min = min(ticks)
        tick_max = max(ticks)

        norm = colors.SymLogNorm(linthresh=tick_min, vmin=tick_min, vmax=tick_max)

        # shift the color map accordingly because SymLobNorm cannot be centered
        norm_center = norm(1)

        color_interpolation = np.interp(
            np.linspace(0, 1),
            [0, norm_center, 1],
            [0, 0.5, 1])

        shifted_cmap = colors.LinearSegmentedColormap.from_list("shifted", cmap(color_interpolation))

        fmt=".2f"
        cbar_kws = {
            'format': f'{{x:{fmt}}}',
            'ticks': ticks,
            'extend': 'both',
            'extendrect': True,
        }

        heatmap_ax = sns.heatmap(df, annot=True, fmt=fmt, cmap=shifted_cmap,
                                 linewidths=.5,
                                 norm=norm,
                                 vmin=tick_min, vmax=tick_max,
                                 annot_kws={"size": 11.5,
                                            'color': 'black'},
                                 cbar_kws=cbar_kws)

        cbar = ax.collections[0].colorbar
        cbar.set_ticks([], minor=True)

        ax.xaxis.tick_top()
        plt.xticks(rotation=35, rotation_mode="anchor", ha='left')
        plt.yticks(rotation=0, rotation_mode="anchor", ha='right')

        ax.xaxis.set_label_position('top')
        plt.xlabel('Benchmark')
        plt.ylabel('Version limit')

        if include_geometric_mean:
            ax.axvline(df.shape[1]-1, color='white', lw=3)
            labels = ax.get_xticklabels()
            labels[-1].set_fontweight('bold')

        if title:
            plt.title(f'{path_base} {compiler.name}')

        plt.tight_layout()

        output_path = choose_heatmap_output_path(output, path_base, compiler_name, system_name)

        plt.tight_layout()

        ensure_directory_exists(output_path)
        plt.savefig(output_path)

    # Execution time
    one_heatmap("time", average_time, include_micro=False)
    one_heatmap("micro_time", average_time, include_macro=False)

    # old way to compute checks
    # one_heatmap("checks", sum_checks)

    # Program size
    one_heatmap("program_size", run_program_size)

    # Execution time vs unsafe
    unsafe_base_runs = list(select(
        r for r in Run
        if r.system == system and r.compiler.name == compiler_name
        and r.benchmark.name in benchmark_names
        and r.version_limit == 0
        and not r.safe_arithmetic
        and r.compiler_optimizations
        and r.timestamp == max(select(
            r2.timestamp for r2 in Run
            if r2.system == system and r2.compiler.name == compiler_name
            and r2.version_limit == r.version_limit
            and r2.benchmark == r.benchmark
            and r2.version_limit == 0
            and not r2.safe_arithmetic
            and r2.compiler_optimizations))))

    # Use Gambit as base runs for checks
    checks_compiler_name = 'gambit'
    checks_base_runs = list(select(
        r for r in Run
        if r.compiler.name == checks_compiler_name
        and r.benchmark.name in benchmark_names
        and r.version_limit == 0
        and r.safe_arithmetic
        and not r.compiler_optimizations
        and r.timestamp == max(select(
            r2.timestamp for r2 in Run
            if r.system == r2.system and r2.compiler.name == checks_compiler_name
            and r2.version_limit == r.version_limit
            and r2.benchmark == r.benchmark
            and r2.version_limit == 0
            and r2.safe_arithmetic
            and not r2.compiler_optimizations))))

    if unsafe_base_runs:
        one_heatmap("checks", sum_checks, subtract_unsafe_run=True, base_runs=checks_base_runs,
                    unsafe_runs=unsafe_base_runs)

    one_heatmap("compile_time", compile_time)
    one_heatmap("scheme_compile_time", scheme_compile_time)
    one_heatmap("c_compile_time", c_compile_time)

    #find_correlations(system, compiler, runs, output)
    

def find_correlations(system, compiler, runs, output):
    runs = [r for r in runs if run_is_macro(r) and r.version_limit > 0]

    def get_base_run(run, safe=True):
        base_runs = Run.select(compiler=run.compiler,
                       system=run.system,
                       benchmark=run.benchmark,
                       version_limit=0,
                       compiler_optimizations=run.compiler_optimizations,
                       safe_arithmetic=safe)

        return max(base_runs, key=lambda r: r.timestamp)

    measures = {}

    def register(f, name=None):
        if isinstance(f, str):
            return lambda g: register(g, name=f)
        else:
            measures[name or f.__name__] = f
            return f

    def execution_time(run):
        times = list(select(p.value for p in PerfEvent if p.run == run and p.event == 'real-time'))
        return statistics.mean(times)

    @register("execution_time")
    def etime(run):
        return execution_time(run) / execution_time(get_base_run(run))

    @register
    def size_factor(run):
        return run_program_size(run) / run_program_size(get_base_run(run))

    @register
    def version_limit(run):
        return run.version_limit

    def get_perf_event(run, event_name):
        perf_events = list(select(p for p in PerfEvent if p.event == event_name and p.run == run))
        if not perf_events:
            return math.nan
        else:
            return statistics.mean(p.value for p in perf_events)

    for event in PerfResultParser.event_names:
        @register(f"perf:{event}")
        def perf(run, event=event):
            base = get_perf_event(get_base_run(run), event)
            value = get_perf_event(run, event)
            if base == 0:
                base += 1
            return value / base

    def get_prim_count(run, prim_name):
        prim_count = PrimitiveCount.get(run=run, name=prim_name)
        base = PrimitiveCount.get(run=get_base_run(run), name=prim_name)
        base_value = base.value if base else 0
        if not prim_count:
            return 0
        else:
            return prim_count.value - base_value

    prim_names = list(select(prim.name for prim in PrimitiveCount).distinct())
    for prim_name in prim_names:
        @register(f"primitive:{prim_name}")
        def prim(run, prim_name=prim_name):
            base = get_prim_count(get_base_run(run), prim_name)
            value = get_prim_count(run, prim_name)
            if base == 0:
                base += 1
                value += 1
            return value / base

    def get_static(run, name):
        measures = list(StaticMeasure.select(run=run, name=name))
        if not measures:
            return math.nan
        else:
            return statistics.mean(m.value for m in measures)

    static_names = list(select(prim.name for prim in StaticMeasure).distinct())
    for static_name in static_names:
        @register(f"static:{static_name}")
        def prim(run, static_name=static_name):
            base = get_static(get_base_run(run), static_name)
            value = get_static(run, static_name)
            if base == 0:
                base += 1
                value += 1
            return value / base


    col_names = list(measures)

    data = [[measures[n](r) for n in col_names] for r in runs]

    df = pd.DataFrame(data, columns=col_names)

    corr = df.corr().dropna(axis=0, how='all').dropna(axis=1, how='all')

    fig, ax = plt.subplots(figsize=(50, 20))

    vmin = corr.min().min()
    vmax = corr.max().max()

    heatmap_ax = sns.heatmap(corr, annot=True, fmt='.2f', cmap="coolwarm_r",
                                linewidths=.5, vmin=vmin, vmax=vmax, center=0)

    ax.xaxis.tick_top()
    plt.xticks(rotation=35, rotation_mode="anchor", ha='left')
    plt.yticks(rotation=0, rotation_mode="anchor", ha='right')

    ax.xaxis.set_label_position('top')

    plt.title(f"Correlation matrix - {runs[0].compiler.name}", fontsize=32, fontweight='bold')

    plt.tight_layout()

    output_path = choose_heatmap_output_path(output,
                                             "correlation",
                                             runs[0].compiler.name,
                                             runs[0].system.name)

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
            return path.resolve()
        else:
            raise NotADirectoryError(f"{value} is not a valid directory")

    def file_path(value):
        path = pathlib.Path(value)
        if path.is_file():
            return path.resolve()
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

    class CompilerArg:
        def __init__(self, name, path):
            self.name = name
            self.path = path

    class StoreFlagAndOptionalArg(argparse.Action):
        def __call__(self, parser, namespace, values, option_string=None):
            # Store a tuple with the flag's name (or a simplified version of it) and the optional argument
            setattr(namespace, self.dest, CompilerArg(option_string.replace('--', ''), values if values else None))

    compiler_parser_group = benchmark_parser.add_mutually_exclusive_group()

    compiler_parser_group.add_argument('--gambit', dest='compiler', metavar="path", action=StoreFlagAndOptionalArg, nargs='?', type=str, help='Use Gambit compiler. Optional directory path can follow.')
    compiler_parser_group.add_argument('--bigloo', dest='compiler', metavar="path", action=StoreFlagAndOptionalArg, nargs='?', type=str, help='Use Bigloo compiler. Optional directory path can follow.')
    compiler_parser_group.add_argument('--chez',   dest='compiler', metavar="path", action=StoreFlagAndOptionalArg, nargs='?', type=str, help='Use Chez compiler. Optional directory path can follow.')
    compiler_parser_group.add_argument('--node',   dest='compiler', metavar="path", action=StoreFlagAndOptionalArg, nargs='?', type=str, help='Use NodeJS compiler. Optional directory path can follow.')
    compiler_parser_group.add_argument('--racket', dest='compiler', metavar="path", action=StoreFlagAndOptionalArg, nargs='?', type=str, help='Use Racket compiler. Optional directory path can follow.')

    benchmark_parser.add_argument('-l', '--limit',
                                  dest="version_limits",
                                  metavar="LIMIT",
                                  nargs="+",
                                  default=(0, 1, 2, 3, 4, 5),
                                  type=int,
                                  help="BBV versions limits")

    benchmark_parser.add_argument('-u', '--unsafe',
                                  dest="safe_arithmetic",
                                  action='store_false',
                                  default=True,
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
                             action='store_false',
                             default=True,
                             help="benchmarks with unsafe arithmetic")

    plot_parser.add_argument('-e', '--perf-events',
                             nargs="+",
                             default=(),
                             dest="perf_event_names",
                             help="perf stat events to plot")

    plot_parser.add_argument('-m', '--static-measures',
                             nargs="+",
                             default=(),
                             dest="static_measures_names",
                             help="static measures to plot")

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
                                 action='store_false',
                                 default=True,
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

    # Parser for dumping results in CSV
    csv_parser = subparsers.add_parser('csv', help='Dumps benchmark data in csv')

    csv_parser.add_argument('-s', '--systen',
                            metavar="SYSTEM",
                            dest="system_name",
                            help="Benchmarks system")

    csv_parser.add_argument('-l', '--limit',
                            dest="version_limits",
                            metavar="LIMIT",
                            nargs="+",
                            default=None,
                            type=int,
                            help="BBV versions limits")

    csv_parser.add_argument('-o', '--output',
                            dest="output",
                            help="where to output the csv (file or folder)")

    # Parser for dumping results in CSV
    heatmap_parser = subparsers.add_parser('heatmap', help='Create heatmap')

    heatmap_parser.add_argument('-s', '--systen',
                            metavar="SYSTEM",
                            dest="system_name",
                            help="Benchmarks system")

    heatmap_parser.add_argument('-c', '--compiler',
                            metavar="COMPILER",
                            dest="compiler_name",
                            default="gambit",
                            help="Benchmark compiler")
    
    heatmap_parser.add_argument('-b', '--benchmarks',
                            metavar="BENCHMARK",
                            nargs="+",
                            dest="benchmark_names",
                            help="benchmarks")

    heatmap_parser.add_argument('-l', '--limit',
                            dest="version_limits",
                            metavar="LIMIT",
                            nargs="+",
                            default=None,
                            type=int,
                            help="BBV versions limits")

    heatmap_parser.add_argument('-o', '--output',
                            dest="output",
                            help="where to output the png (file or folder)")


    args = parser.parse_args()

    # Set logger level
    logger.setLevel(args.loglevel)
    logger.addHandler(logging.StreamHandler())
    
    logger.debug(args)

    if args.command == 'benchmark':
        if not args.compiler:
            raise benchmark_parser.error("a compiler is required")
        run_and_save_benchmark(compiler=args.compiler,
                               file=args.file,
                               version_limits=args.version_limits,
                               safe_arithmetic=args.safe_arithmetic,
                               repetitions=args.repetitions,
                               compiler_optimizations=args.compiler_optimizations,
                               force_execution=args.force_execution,
                               timeout=args.timeout)
    elif args.command == 'plot':
        plot_benchmarks(benchmark=args.benchmark,
                        compiler_name=args.compiler_name,
                        system_name=args.system_name,
                        safe_arithmetic=args.safe_arithmetic,
                        perf_event_names=args.perf_event_names,
                        primitive_names_or_amount=args.primitive_names_or_amount,
                        static_measures_names=args.static_measures_names,
                        output=args.output)
    elif args.command == 'analysis':
        analyze(
            system_name=args.system_name,
            compiler_name=args.compiler_name,
            benchmark_names=args.benchmark_names,
            safe_arithmetic=args.safe_arithmetic,
            output=args.output)
    elif args.command == 'perf_distribution':
        perf_distribution(system_name=args.system_name,
                          compiler_name=args.compiler_name,
                          benchmark_name=args.benchmark_name,
                          event=args.event,
                          version_limit=args.version_limit,
                          output=args.output)
    elif args.command == 'csv':
        to_csv(system_name=args.system_name,
               version_limits=args.version_limits,
               output=args.output)
    elif args.command == 'heatmap':
        make_heatmap(system_name=args.system_name,
                      compiler_name=args.compiler_name,
                      benchmark_names=args.benchmark_names,
                      version_limits=args.version_limits,
                      output=args.output)
    else:
        parser.print_help()


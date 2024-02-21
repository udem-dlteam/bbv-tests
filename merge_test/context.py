import operator

from enum import auto, Flag
from functools import total_ordering
from sys import maxsize


class NonFixnumType(Flag):
    flonum = auto()
    bignum = auto()
    string = auto()
    vector = auto()
    procedure = auto()
    ret = auto()

    @classmethod
    @property
    def bot(cls):
        return cls(0)

    @classmethod
    @property
    def top(cls):
        return cls(sum(member.value for member in cls))

class FixnumOverflow(Exception):
    pass


def binary_fixnum_operation(op):
    def wrapper(self, other):
        return op(self, Fixnum(other))
    return wrapper


@total_ordering
class Fixnum:
    _max_fixnum_value = 2 ** 61 - 1
    _min_fixnum_value = - 2 ** 61

    def __init__(self, value):
        if isinstance(value, Fixnum):
            self.value = value.value
            return

        value = operator.index(value)

        if self._min_fixnum_value <= value <= self._max_fixnum_value:
            self.value = value
        else:
            raise FixnumOverflow

    @classmethod
    @property
    def max_fixnum(cls):
        return cls(cls._max_fixnum_value)

    @classmethod
    @property
    def min_fixnum(cls):
        return cls(cls._min_fixnum_value)

    @binary_fixnum_operation
    def __add__(self, other):
        return Fixnum(self.value + other.value)

    @binary_fixnum_operation
    def __radd__(self, other):
        return Fixnum(self.value + other.value)

    @binary_fixnum_operation
    def __sub__(self, other):
        return Fixnum(self.value - other.value)

    @binary_fixnum_operation
    def __rsub__(self, other):
        return Fixnum(self.value - other.value)

    @binary_fixnum_operation
    def __eq__(self, other):
        return self.value == other.value

    @binary_fixnum_operation
    def __lt__(self, other):
        return self.value < other.value

    def _value_repr(self):
        value = self.value

        if value == self._min_fixnum_value:
            return '>='
        elif value == self._min_fixnum_value + 1:
            return '>'
        elif value == self._max_fixnum_value:
            return '<='
        elif value == self._max_fixnum_value - 1:
            return '<'
        else:
            return str(value)

    def __repr__(self):
        type_name = type(self).__name__
        return f'{type_name}({self._value_repr()})'


class FixnumRange:
    def __init__(self, low, high):
        self.low = Fixnum(low)
        self.high = Fixnum(high)

    @classmethod
    @property
    def bot(cls):
        return cls(0, -1)

    @classmethod
    @property
    def top(cls):
        return cls(Fixnum.min_fixnum, Fixnum.max_fixnum)

    def is_empty(self):
        return self.low > self.high

    def __or__(self, other):
        if self.is_empty():
            return other
        elif other.is_empty():
            return self
        else:
            return FixnumRange(min(self.low, other.low), max(self.high, other.high))

    def _interval_repr(self):
        if self.is_empty():
            return "()"
        else:
            return f"{self.low._value_repr()}, {self.high._value_repr()}"

    def __repr__(self):
        type_name = type(self).__name__
        return f"{type_name}({self._interval_repr()})"

    def __len__(self):
        return len(range(self.low.value, self.high.value + 1))

    def __contains__(self, other):
        if other.is_empty():
            return True
        else:
            return other.low >= self.low and other.high <= self.high

class Type:
    def __init__(self, fixnum_range, types):
        self.range = fixnum_range
        self.types = types

    def _type_repr(self):
        return f"{self.range._interval_repr()}, {self.types}"

    def __repr__(self):
        type_name = type(self).__name__
        return f"{type_name}({self._type_repr()})"

    def __contains__(self, other):
        return other.range in self.range and other.types in self.types

    def __or__(self, other):
        return Type(self.range | other.range, self.types | other.types)

    @classmethod
    @property
    def bot(cls):
        return cls(FixnumRange.bot, NonFixnumType.bot)

    @classmethod
    @property
    def top(cls):
        return cls(FixnumRange.top, NonFixnumType.top)

class Context:
    def __init__(self, *types, registers=0, frame=0, closure=0):
        if len(types) != registers + frame + closure:
            raise ValueError("invalid number of types")

        closure_start = registers + frame
        self.registers = types[0:registers]
        self.frame = types[registers:closure_start]
        self.closure = types[closure_start:closure_start + closure]

    def __iter__(self):
        yield from self.registers
        yield from self.frame
        yield from self.closure

    def __or__(self, other):
        if not self.size_match(other):
            raise ValueError("Context size does not match")

        new_types = []

        for t1, t2 in zip(self, other):
            new_types.append(t1 | t2)

        return Context(*new_types, registers=len(self.registers), frame=len(self.frame), closure=len(self.closure))

    def size_match(self, other):
        return len(self.registers) == len(other.registers) \
           and len(self.frame) == len(other.frame) \
           and len(self.closure) == len(other.closure)

    def pretty(self):
        lines = []

        for name, container in ("reg", self.registers), ("fra", self.frame), ("clo", self.closure):
            for i, t in enumerate(container):
                lines.append(f"{name}[{i}]:{t._type_repr()}")

        return '\n'.join(lines)

    

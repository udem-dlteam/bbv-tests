import operator

from enum import auto, Flag
from functools import total_ordering
from itertools import pairwise
from sys import maxsize


class NonFixnumType(Flag):
    flonum = auto()
    bignum = auto()
    string = auto()
    vector = auto()
    procedure = auto()
    ret = auto()

    NB_EXTRA_TYPES = 9
    locals().update({f"t{i}": auto() for i in range(1, NB_EXTRA_TYPES + 1)})

    @classmethod
    @property
    def bot(cls):
        return cls(0)

    @classmethod
    @property
    def top(cls):
        return cls(sum(member.value for member in cls))

    def is_top(self):
        return self == self.top

    def is_bot(self):
        return self == self.bot

    def __len__(self):
        return self.value.bit_count()

    def __str__(self):
        return '|'.join(name[:2] for name in self.name.split('|'))


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

    def is_top(self):
        return self == self.top

    def __or__(self, other):
        if self.is_empty():
            return other
        elif other.is_empty():
            return self
        else:
            return FixnumRange(min(self.low, other.low), max(self.high, other.high))

    def __str__(self):
        if self.is_empty():
            return "∅"
        elif self.is_top():
            return "fx"
        else:
            return f"{self.low._value_repr()}..{self.high._value_repr()}"

    def __len__(self):
        return len(range(self.low.value, self.high.value + 1))

    def __contains__(self, other):
        if other.is_empty():
            return True
        else:
            return other.low >= self.low and other.high <= self.high

    def __eq__(self, other):
        if self.is_empty() and other.is_empty():
            return True
        else:
            return self.low == other.low and self.high == other.high

class Type:
    def __init__(self, fixnum_range, types):
        self.range = fixnum_range
        self.types = types

    def __str__(self):
        if self.is_top():
            return "T"
        elif self.is_bot():
            return "_"
        elif self.range.is_empty():
            return str(self.types)
        elif self.types.is_bot():
            return str(self.range)
        else:
            return f"{self.range}|{self.types}"

    def __contains__(self, other):
        return other.range in self.range and other.types in self.types

    def __or__(self, other):
        return Type(self.range | other.range, self.types | other.types)

    def __eq__(self, other):
        return self.types == other.types and self.range == other.range

    @classmethod
    @property
    def bot(cls):
        return cls(FixnumRange.bot, NonFixnumType.bot)

    @classmethod
    @property
    def top(cls):
        return cls(FixnumRange.top, NonFixnumType.top)

    def is_top(self):
        return self == self.top

    def is_bot(self):
        return self == self.bot

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

    def size_match(self, *others):
        for other in others:
            if    len(self.registers) != len(other.registers) \
               or len(self.frame) != len(other.frame) \
               or len(self.closure) != len(other.closure):
               return False
        return True

def format_contexts(contexts):
    first = contexts[0]
    if not first.size_match(*contexts[1:]):
        raise ValueError("Context size does not match")

    regs = len(first.registers)
    frame = len(first.frame)
    closure = len(first.closure)

    col_names = [f"r{r}" for r in range(1, regs + 1)]
    col_names += [f"frame[{f}]" for f in range(1, frame + 1)]
    col_names += [f"clo[{f}]" for c in range(1, closure + 1)]
    
    rows = [col_names]
    for context in contexts:
        rows.append([str(type) for type in context])

    widths = [max(len(rows[i][j]) for i in range(len(rows))) for j in range(len(rows[0]))]

    table = "\n".join(" ; ".join(cell.ljust(width) for width, cell in zip(widths, row)) for row in rows)

    return table

def format_context(context):
    return format_contexts([context])
    
# Common types

fixnums = Type(FixnumRange.top, NonFixnumType.bot)
non_negative_fixnums = Type(FixnumRange(0, Fixnum.max_fixnum), NonFixnumType.bot)
not_fixnums = Type(FixnumRange.bot, NonFixnumType.top)
top = Type.top
bot = Type.bot
fixnums_not_maxfixnum = Type(FixnumRange(Fixnum.min_fixnum, Fixnum.max_fixnum - 1), NonFixnumType.bot)

import itertools

from .context import NonFixnumType, Context, fixnums, not_fixnums, top, bot, fixnums_not_maxfixnum, non_negative_fixnums, \
                     format_context, format_contexts

def make_distance_merge_function(distance):
    def merge_function(contexts, version_limit):
        new_contexts = list(contexts)
        merged = []
        result = None
        while len(new_contexts)> version_limit:
            c1, c2 = min(itertools.combinations(new_contexts, 2), key=lambda p: distance(*p))
            new_contexts.remove(c1)
            new_contexts.remove(c2)
            if c1 in contexts and c1 not in merged:
                merged.append(c1)
            if c2 in contexts and c2 not in merged:
                merged.append(c2)
            result = c1 | c2
            new_contexts.append(result)
        return merged, result, new_contexts
    return merge_function

def linear_distance(c1, c2):
    same_bits = 0
    for type1, type2 in zip(c1, c2):
        same_bits += len(~(type1.types ^ type2.types) & NonFixnumType.top)
        same_bits += type1.range.is_empty() == type2.range.is_empty()
    return same_bits

merge_closest = make_distance_merge_function(linear_distance)


## Tests
contexts = [
    Context(fixnums, fixnums, fixnums_not_maxfixnum, registers=3),
    Context(fixnums, not_fixnums, fixnums_not_maxfixnum, registers=3),
    Context(fixnums, fixnums, top, registers=3),
    Context(not_fixnums, fixnums, top, registers=3),
    Context(top, not_fixnums, top, registers=3)
]

print("Before Merge")

print(format_contexts(contexts))
print()

merged, result, new_contexts = merge_closest(contexts, version_limit=4)

print("Merged:", *(contexts.index(c) for c in merged), "->\n", format_context(result))
print()

print("After merge")

print(format_contexts(new_contexts))

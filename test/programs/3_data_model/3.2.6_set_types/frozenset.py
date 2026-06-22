# 3.2.6 Set types — frozenset: an immutable, hashable set. It supports set
# algebra and subset/superset comparisons within and across set, compares equal
# to a set with the same elements, and can be a dict key or set element.
fs = frozenset([3, 1, 2, 2])
print(type(fs).__name__, len(fs), sorted(fs))
print(frozenset(), repr(frozenset()), repr(frozenset([1])))
print(frozenset([1, 2]) == {1, 2}, {1, 2} == frozenset([1, 2]))
print(isinstance(fs, frozenset), isinstance(fs, set), isinstance({1}, frozenset))
print(sorted(frozenset([1, 2, 3]) | {3, 4}), sorted(frozenset([1, 2, 3]) & {2, 3, 4}))
print(frozenset([1, 2]) <= frozenset([1, 2, 3]), frozenset([1, 2]) < {1, 2})
print(2 in fs, 9 in fs)

# Hashable: usable as a dict key and as a set element (de-duplicated).
d = {frozenset([1, 2]): "a", frozenset([3]): "b"}
print(d[frozenset([2, 1])])
s = {frozenset([1]), frozenset([1]), frozenset([2])}
print(len(s))

# The type of a binary set op follows the left operand.
print(type(frozenset([1]) | {2}).__name__, type({1} | frozenset([2])).__name__)

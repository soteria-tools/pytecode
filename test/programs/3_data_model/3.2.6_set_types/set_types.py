# 3.2.6 Set types: unordered collections of unique, hashable objects; len()
# counts them; they are iterable but not subscriptable. (Results are sorted
# before printing because iteration order is unspecified.)
s = {3, 1, 2, 3, 1}
print(len(s), sorted(s))

# Numbers that compare equal are the same element (1, 1.0 and True coincide),
# and the first one inserted is the one kept.
print(sorted({1, 1.0, 2}), len({1, 1.0, True}))

# A set is not subscriptable.
try:
    s[0]
except TypeError as e:
    print("TypeError:", e)

# Membership and iteration.
print(2 in s, 9 in s)
print(sorted(x * 10 for x in s))

# Mutable: add/discard modify the set in place.
s.add(4)
s.discard(1)
print(sorted(s))

# Set algebra: union, intersection, difference, symmetric difference.
a, b = {1, 2, 3}, {2, 3, 4}
print(sorted(a | b), sorted(a & b), sorted(a - b), sorted(a ^ b))

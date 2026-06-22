s = {3, 1, 2, 3}
print(len(s), sorted(s))
s.add(4)
s.discard(1)
print(sorted(s))
a, b = {1, 2, 3}, {2, 3, 4}
print(sorted(a | b), sorted(a & b), sorted(a - b), sorted(a ^ b))
print(2 in a, 9 in a)
print({1, 2} <= {1, 2, 3}, {1, 4} <= {1, 2, 3})

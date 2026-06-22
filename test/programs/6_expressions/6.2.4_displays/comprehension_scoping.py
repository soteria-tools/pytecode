# 6.2.4 Displays/comprehensions run in an implicitly nested scope: the target
# names do not leak into the enclosing scope, while names from the enclosing
# scope remain visible.
result = [i * 2 for i in range(5)]
print(result)
try:
    print(i)
except NameError:
    print("i not leaked")

# Later for-clauses depend on values from earlier (leftmost) ones.
print([x * y for x in range(1, 4) for y in range(x, x + 2)])

# A comprehension iterating a generator expression, with a filter.
print([y for y in (z * z for z in range(5)) if y % 2 == 0])

# Enclosing-scope names are visible inside the comprehension.
n = 10
print([n + k for k in range(3)])

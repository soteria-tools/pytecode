a = {1, 2, 3, 4}
b = {2, 4, 6}
print(sorted(a - b))
print(sorted(b - a))
print(sorted(a - a))
c = {1, 2, 3}
c -= {2}
print(sorted(c))
print(sorted({1, 2, 3} - {3}))

for i, c in enumerate("abc"):
    print(i, c)
for i, c in enumerate("xy", start=10):
    print(i, c)
print(list(zip([1, 2, 3], "abc")))
print(list(zip([1, 2], [3, 4], [5, 6])))
print(list(zip([1, 2, 3], [4])))
a, b = zip(*[(1, "x"), (2, "y")])
print(a, b)

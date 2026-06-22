def f(a, b, /, c, *, d, e=5):
    return a, b, c, d, e
print(f(1, 2, 3, d=4))
print(f(1, 2, c=3, d=4, e=6))
try:
    f(1, 2, 3, 4, d=5)
except TypeError:
    print("TypeError")

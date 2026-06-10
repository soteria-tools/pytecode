def f():
    return 1
f.tag = "special"
f.count = 0
f.count += 1
print(f.__name__, f.tag, f.count)
print(f())
def g(a, b=2):
    "docstring here"
    return a + b
print(g.__doc__)
print(g.__defaults__)

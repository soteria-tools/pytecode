def f(a, b, c, d=10):
    return a, b, c, d
args = (1, 2)
kwargs = {"d": 40}
print(f(*args, 3))
print(f(*args, 3, **kwargs))
print(f(*[1, 2], **{"c": 3, "d": 4}))
print([*range(3), *"ab"])
print({**{"a": 1}, **{"b": 2}, "a": 9})

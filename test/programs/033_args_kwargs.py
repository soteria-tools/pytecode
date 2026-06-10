def f(*args, **kwargs):
    return len(args), sorted(kwargs.items())
print(f())
print(f(1, 2, 3))
print(f(1, a=2, b=3))
def g(a, *rest, sep="-"):
    return sep.join([str(a)] + [str(r) for r in rest])
print(g(1, 2, 3))
print(g(1, 2, sep="+"))

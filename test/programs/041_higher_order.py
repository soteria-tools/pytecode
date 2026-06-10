def compose(f, g):
    return lambda x: f(g(x))
inc = lambda x: x + 1
dbl = lambda x: x * 2
print(compose(inc, dbl)(10))
print(compose(dbl, inc)(10))
print(list(map(inc, [1, 2, 3])))
print(list(filter(lambda x: x % 2, range(10))))

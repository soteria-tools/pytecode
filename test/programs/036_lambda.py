sq = lambda x: x * x
print(sq(7))
add = lambda a, b=10: a + b
print(add(1), add(1, 2))
fns = [lambda x, i=i: x + i for i in range(3)]
print([f(100) for f in fns])

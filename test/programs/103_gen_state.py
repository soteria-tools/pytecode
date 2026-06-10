def fib_gen():
    a, b = 0, 1
    while True:
        yield a
        a, b = b, a + b
g = fib_gen()
print([next(g) for _ in range(10)])
def make_counters():
    return [iter(range(i, i + 2)) for i in range(2)]
c1, c2 = make_counters()
print(next(c1), next(c2), next(c1), next(c2))

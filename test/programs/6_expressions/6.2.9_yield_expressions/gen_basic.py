def gen():
    print("start")
    yield 1
    print("middle")
    yield 2
    print("end")
g = gen()
print(next(g))
print(next(g))
try:
    next(g)
except StopIteration:
    print("StopIteration")

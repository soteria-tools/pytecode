def chunks(xs, size):
    for i in range(0, len(xs), size):
        yield xs[i:i + size]
print(list(chunks([1, 2, 3, 4, 5], 2)))
def running_total(it):
    total = 0
    for x in it:
        total += x
        yield total
print(list(running_total(squares for squares in [1, 4, 9])))
pipeline = running_total(x * 10 for x in range(4))
print([v for v in pipeline])
def gen_with_finally():
    try:
        yield 1
        yield 2
    finally:
        print("cleanup")
print(list(gen_with_finally()))

xs = list(range(10))
print(xs[2:5], xs[:3], xs[7:], xs[:])
print(xs[::2], xs[1::3], xs[::-1])
print(xs[-3:], xs[:-7], xs[-1:-5:-1])
print("hello world"[6:], "hello"[::-1])
xs[2:4] = [20, 30, 40]
print(xs)
del xs[0:2]
print(xs)

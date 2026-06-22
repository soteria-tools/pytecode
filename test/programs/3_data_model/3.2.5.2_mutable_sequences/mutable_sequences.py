# 3.2.5.2 Mutable sequences (list): subscription and slicing can be the target
# of assignment and del statements.
xs = [1, 2, 3, 4, 5]
xs[0] = 10
print(xs)
xs[1:3] = [20, 30, 40]
print(xs)
del xs[0]
print(xs)
del xs[1:3]
print(xs)
xs[:] = [9, 8, 7]
print(xs)

# Extended-slice assignment and deletion.
ys = [0, 1, 2, 3, 4, 5]
ys[::2] = [-1, -2, -3]
print(ys)
del ys[::2]
print(ys)

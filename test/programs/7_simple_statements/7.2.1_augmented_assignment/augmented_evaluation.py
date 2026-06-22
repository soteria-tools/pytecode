# 7.2.1 Augmented assignment: the target is evaluated once and, unlike normal
# assignment, the left-hand side is evaluated before the right-hand side; the
# operation is in-place when the type supports it.
log = []


def idx(i):
    log.append(("idx", i))
    return i


def rhs(v):
    log.append(("rhs", v))
    return v


a = [10, 20, 30]
a[idx(1)] += rhs(5)
print(a, log)

# += on a list mutates the same object in place; + creates a new one.
xs = [1]
ys = xs
xs += [2, 3]
print(xs, ys, xs is ys)

zs = [1]
ws = zs
zs = zs + [2]
print(zs, ws, zs is ws)

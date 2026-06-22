# 6.3.4 Calls: keyword arguments fill named slots, defaults fill the rest,
# *iterable and **mapping unpack into positional/keyword arguments, and the
# various binding errors raise TypeError.
def f(a, b, c=3):
    return (a, b, c)


print(f(1, 2))
print(f(1, 2, 4))
print(f(1, c=9, b=2))
print(f(*[1, 2], **{"c": 7}))


def g(*args, **kwargs):
    return (args, sorted(kwargs.items()))


print(g(1, 2, x=3, y=4))


# *expression is processed before keyword arguments.
def h(a, b):
    return (a, b)


print(h(b=1, *(2,)))


def show(thunk):
    try:
        thunk()
    except TypeError as e:
        print("TypeError:", e)


show(lambda: f(1, 2, a=5))
show(lambda: f(1))
show(lambda: f(1, 2, z=9))
show(lambda: f(1, 2, 3, 4))

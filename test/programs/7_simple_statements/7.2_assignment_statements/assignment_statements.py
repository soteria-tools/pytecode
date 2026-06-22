# 7.2 Assignment statements: chained assignment binds the one object to each
# target; tuple targets unpack; targets are assigned left to right.
a = b = c = []
a.append(1)
print(a, b, c, a is b is c)

x, y = 1, 2
x, y = y, x
print(x, y)

# Overlaps within the targets resolve left to right: i is set, then lst[i].
lst = [0, 1]
i = 0
i, lst[i] = 1, 2
print(lst, i)


# Attribute target: assigning inst.x creates an instance attribute; the class
# variable is unchanged.
class Cls:
    x = 3


inst = Cls()
inst.x = inst.x + 1
print(inst.x, Cls.x)

# Starred and nested unpacking.
first, *mid, last = [1, 2, 3, 4, 5]
print(first, mid, last)
(p, q), r = (1, 2), 3
print(p, q, r)
*init, tail = "abc"
print(init, tail)


def show(thunk):
    try:
        thunk()
    except ValueError as e:
        print("ValueError:", e)


def too_many():
    a, b = 1, 2, 3


def too_few():
    a, b, c = 1, 2


show(too_many)
show(too_few)

# 3.1 Objects, values and types.
# type() returns an object's type; `is` compares identity.
print(type(3) is int, type(3.0) is float, type("x") is str)
print(type([]) is list, type({}) is dict, type(()) is tuple)
print(type(True) is bool, type(None) is type(None))

# Mutable objects: two distinct literals are never the same object,
# `e = f = []` binds the *same* object to both names.
c = []
d = []
print(c is d, c == d)
e = f = []
print(e is f)

# Aliasing: mutation through one name is visible through the other.
g = c
g.append(1)
print(c, c is g)

# An immutable container (tuple) holding a mutable object: the container's
# value can change when the contained mutable object is changed.
t = ([],)
t[0].append(9)
print(t)

# Type and identity are unchangeable for an object's lifetime.
x = 5
print(type(x) is int, x is x)

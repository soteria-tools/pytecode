# 6.15 Expression lists: a comma yields a tuple; * unpacks an iterable into the
# new tuple/list/set; a trailing comma is required only for a one-item tuple.
t = 1, 2, 3
print(t, type(t).__name__)
print((*[1, 2], 3, *(4, 5)))
print([*range(3), *[9, 8]])
print(sorted({*[1, 2], *[2, 3]}))
print((*"ab", *"cd"))
print(type((1,)).__name__, type((1)).__name__)

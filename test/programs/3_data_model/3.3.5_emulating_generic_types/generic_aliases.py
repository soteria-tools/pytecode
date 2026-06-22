# 3.3.5 Emulating generic types: subscripting a built-in container class (via
# its __class_getitem__) produces a types.GenericAlias recording the origin
# class and the supplied arguments.
ga = list[int]
print(ga)
print(type(ga).__name__)
print(ga.__origin__, ga.__args__)
print(dict[str, int])
print(tuple[int, ...])
print(tuple[int, str, float])
print(set[bytes])
print(frozenset[int])
print(list[list[int]])  # nested
print(list[int].__args__ == (int,))

# A GenericAlias delegates attribute access to its origin class.
print(list[int].__name__)


# Subscripting a type that is not subscriptable is a TypeError.
def show(thunk):
    try:
        thunk()
    except TypeError as e:
        print("TypeError:", e)


show(lambda: int[str])

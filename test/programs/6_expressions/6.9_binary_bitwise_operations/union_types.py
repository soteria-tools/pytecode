# 6.9 / PEP 604: the | operator on two types builds a types.UnionType. None is
# accepted as a shorthand for type(None), members are flattened and de-duplicated,
# and a single remaining member collapses back to the bare type.
u = int | str
print(u)
print(type(u).__name__)
print(u.__args__)
print(int | None)
print((int | None).__args__)
print(int | str | bytes)  # flattening
print((int | str) | bytes)
print(int | (str | bytes))
print(int | int)  # de-dup -> a single type, not a union
print(type(int | int).__name__)
print(int | str | int)  # de-dup keeps order
print(list[int] | None)  # a GenericAlias is a valid member

# isinstance accepts a union as its second argument.
print(isinstance(3, int | str))
print(isinstance("x", int | str))
print(isinstance(3.0, int | str))
print(isinstance(None, int | None))
print(isinstance(5, int | None))


# A non-type operand is a TypeError.
def show(thunk):
    try:
        thunk()
    except TypeError as e:
        print("TypeError:", e)


show(lambda: int | 5)
show(lambda: "x" | str)

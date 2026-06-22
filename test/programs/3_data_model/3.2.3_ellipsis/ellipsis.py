# 3.2.3 Ellipsis: a type with a single value, accessed through the literal ...
# or the built-in name Ellipsis; its truth value is true.
print(...)
print(Ellipsis)
print(repr(...))
print(type(...).__name__)
print(bool(...))
print(... is Ellipsis, Ellipsis is ...)
print(... is ...)


def stub():
    ...


print(stub())
print("yes" if ... else "no")
d = {...: "dots"}
print(d[...])

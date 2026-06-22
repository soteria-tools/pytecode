# 8.10 Type parameter lists (PEP 695): a `[T]` list on a function or type alias
# introduces TypeVars, exposed via __type_params__. The annotations may reference
# the type variables. (Generic *classes* — class C[T] — depend on typing.Generic
# and are out of scope here.)
def first[T](items):
    return items[0]


print(first([1, 2, 3]), first(["a", "b"]))
print(first.__type_params__)
print(first.__type_params__[0].__name__, repr(first.__type_params__[0]))
print(type(first.__type_params__[0]).__name__)


def pair[T, U](a, b):
    return (a, b)


print(pair.__type_params__, pair(1, "x"))


# A bound type variable: [T: int].
def bounded[T: int](x):
    return x


tv = bounded.__type_params__[0]
print(tv.__name__, tv.__bound__, tv.__constraints__)


# Constrained type variable: [T: (int, str)].
def constrained[T: (int, str)](x):
    return x


cv = constrained.__type_params__[0]
print(cv.__bound__, cv.__constraints__)


# A generic type alias.
type Vec[T] = list[T]
print(Vec.__type_params__, Vec.__value__, Vec.__name__)


# A function without type parameters has an empty __type_params__.
def plain(x):
    return x


print(plain.__type_params__)

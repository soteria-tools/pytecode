# 7.2.2 Annotated assignment: `x: T = v` assigns v and records the annotation; at
# module/class scope annotations populate __annotations__. A bare `x: T` records
# the annotation without binding x. Function-scope annotations are not stored.
count: int = 5
print(count)
print(__annotations__["count"])

ratio: float
print("count" in __annotations__, "ratio" in __annotations__)
try:
    print(ratio)
except NameError:
    print("ratio unbound")


class C:
    x: int = 1
    y: str


print(C.x, sorted(C.__annotations__))


def f():
    z: int = 9
    return z


print(f())

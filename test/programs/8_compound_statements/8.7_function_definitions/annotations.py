# 8.7 Function definitions: parameter and return annotations are collected into
# the function's __annotations__ dict (in definition order); the expressions are
# evaluated at definition time. A function without annotations has an empty dict.
def f(a: int, b: "str" = "z", *args: float, c: bool, **kw: bytes) -> None:
    return None


print(f.__annotations__)


def plain(x, y):
    return x + y


print(plain.__annotations__)

# Annotations evaluate their expressions at definition time.
seen = []


def mark(tag):
    seen.append(tag)
    return tag


def g(x: mark("x-ann")) -> mark("ret-ann"):
    return x


print(seen)
print(g.__annotations__)

# A lambda has no annotations.
print((lambda x: x).__annotations__)

# Annotations combine with decorators and default values.
def deco(fn):
    fn.decorated = True
    return fn


@deco
def h(n: int = 3) -> int:
    return n * 2


print(h(), h.decorated, h.__annotations__, h.__defaults__)

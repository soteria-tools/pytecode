# 3.2.8 Callable types: the objects to which a function call can be applied.

# --- User-defined functions ---
def greet(name, punct="!"):
    "say hello"
    return "hi " + name + punct


print(greet("a"), greet("b", "?"))
print(greet.__name__, greet.__qualname__)
print(greet.__doc__)
print(greet.__defaults__)


def nodoc():
    return 1


print(nodoc.__doc__, nodoc.__defaults__)
greet.tag = 7
print(greet.tag)


# --- Instance methods: __self__ is the instance, __func__ the function ---
class C:
    def f(self, x):
        return ("f", x)


x = C()
m = x.f
print(m(1))
print(m.__self__ is x, m.__func__ is C.f)
print(x.f(1) == C.f(x, 1))


# A plain function stored on the *instance* is not turned into a bound method.
def loose(a):
    return ("loose", a)


x.g = loose
print(x.g(5))


# --- Generator functions: calling returns an iterator ---
def gen():
    yield 1
    yield 2


g = gen()
print(next(g), next(g))
try:
    next(g)
except StopIteration:
    print("StopIteration")


# --- Classes are callable; calling creates an instance ---
print(type(C()).__name__)

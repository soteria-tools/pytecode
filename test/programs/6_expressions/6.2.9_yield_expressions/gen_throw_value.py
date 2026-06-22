# 6.2.9 Yield expressions: generator.throw() raises an exception at the suspended
# yield point (which the generator may catch), and a generator's return value is
# carried on StopIteration.value.
def catching():
    try:
        yield 1
    except ValueError as e:
        print("  caught:", e)
        yield 2
    yield 3


g = catching()
print(next(g))
print(g.throw(ValueError("injected")))
print(next(g))


# An exception the generator does not catch propagates out of throw().
def transparent():
    yield 1


t = transparent()
print(next(t))
try:
    t.throw(KeyError("k"))
except KeyError as e:
    print("propagated:", e)


# StopIteration.value carries the generator's return value.
def returns():
    yield 1
    return "done"


r = returns()
print(next(r))
try:
    next(r)
except StopIteration as e:
    print("value:", e.value)


# StopIteration with no return value -> value is None.
def empty():
    return
    yield


try:
    next(empty())
except StopIteration as e:
    print("empty value:", e.value)

# Explicitly constructed StopIteration.
print(StopIteration("x").value, StopIteration().value)

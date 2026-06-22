# 7.3 The assert statement: `assert expr` raises AssertionError when expr is
# false; `assert expr, msg` raises AssertionError(msg). __debug__ is True.
print(__debug__)
assert True
assert 1 + 1 == 2, "math broke"


def f1():
    assert False


def f2():
    assert False, "custom message"


def f3():
    assert 0, 42


for f in (f1, f2, f3):
    try:
        f()
    except AssertionError as e:
        print("AssertionError:", repr(str(e)))

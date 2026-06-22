# 7.6 The return statement: leaves the enclosing function with the given value
# (None if omitted); statements after a return are not executed.
def f():
    return 5
    print("unreachable")


def g():
    return


def h():
    pass


print(f(), g(), h())


# A finally clause runs on the way out; a return in finally overrides the try's.
def t1():
    try:
        return "try"
    finally:
        print("finally1")


def t2():
    try:
        return "try"
    finally:
        return "finally"


print(t1())
print(t2())

# 8.4 The try statement: except clauses are tried in order; the else clause runs
# only when no exception occurred; finally always runs; a return in finally
# discards a pending exception.
def classify(x):
    try:
        r = 10 / x
    except ZeroDivisionError:
        return "zero"
    except TypeError:
        return "type"
    else:
        return ("ok", r)
    finally:
        print("finally", x)


print(classify(2))
print(classify(0))
print(classify("a"))


def f():
    try:
        1 / 0
    finally:
        return 42


print(f())


# An exception raised in an except handler is not caught by a sibling except.
def g():
    try:
        try:
            raise ValueError("v")
        except ValueError:
            raise KeyError("k")
    except KeyError as e:
        return ("KeyError", str(e))


print(g())

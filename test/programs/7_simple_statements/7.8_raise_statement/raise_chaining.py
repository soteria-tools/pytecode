# 7.8 The raise statement: a class is instantiated with no args; `from` sets
# __cause__; raising while handling sets __context__ implicitly; `from None`
# suppresses the context; a bare raise with no active exception is a RuntimeError.
try:
    raise ValueError
except ValueError as e:
    print("caught", repr(str(e)), type(e).__name__)

try:
    try:
        1 / 0
    except ZeroDivisionError as zde:
        raise RuntimeError("wrapped") from zde
except RuntimeError as e:
    print(str(e), type(e.__cause__).__name__)

try:
    try:
        1 / 0
    except ZeroDivisionError:
        raise RuntimeError("new")
except RuntimeError as e:
    print(type(e.__context__).__name__, e.__cause__)

try:
    try:
        1 / 0
    except ZeroDivisionError:
        raise RuntimeError("clean") from None
except RuntimeError as e:
    print(e.__cause__, e.__suppress_context__)

try:
    raise
except RuntimeError as e:
    print("RuntimeError:", e)

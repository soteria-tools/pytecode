def inner():
    raise ValueError("from inner")
def middle():
    try:
        inner()
    except ValueError:
        print("middle saw it")
        raise
try:
    middle()
except ValueError as e:
    print("outer:", e)
try:
    try:
        raise KeyError("k1")
    except KeyError:
        raise IndexError("k2")
except IndexError as e:
    print("replaced:", e)

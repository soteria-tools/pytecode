# 3.3.1 __bool__: truth-value testing and bool(). When __bool__ is absent,
# __len__ is consulted (nonzero is true); with neither, instances are always true.
class Always:
    def __bool__(self):
        return False


print(bool(Always()), "t" if Always() else "f")


class ByLen:
    def __init__(self, n):
        self.n = n

    def __len__(self):
        return self.n


print(bool(ByLen(0)), bool(ByLen(3)))
print("t" if ByLen(0) else "f", "t" if ByLen(5) else "f")


class Default:
    pass


print(bool(Default()), "t" if Default() else "f")


# __bool__ must return a bool.
class BadBool:
    def __bool__(self):
        return 1


try:
    bool(BadBool())
except TypeError as e:
    print("TypeError:", e)

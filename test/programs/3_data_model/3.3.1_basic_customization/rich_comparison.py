# 3.3.1 Rich comparisons: x<y calls x.__lt__(y); likewise __le__/__eq__/__ne__/
# __gt__/__ge__. A method may return NotImplemented; there are no swapped
# versions (__lt__/__gt__ reflect, __le__/__ge__ reflect, __eq__/__ne__ self).
class Cmp:
    def __init__(self, v):
        self.v = v

    def __lt__(self, o):
        return self.v < o.v

    def __le__(self, o):
        return self.v <= o.v

    def __gt__(self, o):
        return self.v > o.v

    def __ge__(self, o):
        return self.v >= o.v

    def __eq__(self, o):
        return self.v == o.v

    def __ne__(self, o):
        return self.v != o.v


a, b = Cmp(1), Cmp(2)
print(a < b, a <= b, a > b, a >= b, a == b, a != b)
print(b < a, a == Cmp(1))


# A custom __ne__ is honoured (not merely "not __eq__").
class Weird:
    def __eq__(self, o):
        return True

    def __ne__(self, o):
        return True


w = Weird()
print(w == w, w != w)


# Default __eq__ compares by identity; default __ne__ inverts it.
class Plain:
    pass


p, q = Plain(), Plain()
print(p == p, p == q, p != q, p != p)


# The left method is tried, then the right operand's reflection.
class GtOnly:
    def __gt__(self, o):
        return True


class Other:
    pass


print(Other() < GtOnly())


# A comparison may return any value; bool() is applied in a boolean context.
class Truthy:
    def __eq__(self, o):
        return "yes"


print("eq" if Truthy() == 1 else "ne")

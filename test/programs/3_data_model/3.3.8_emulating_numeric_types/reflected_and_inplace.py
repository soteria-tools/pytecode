# 3.3.8 Reflected (__r*__) operations are used when the left operand does not
# support the operation; in-place (__i*__) operations fall back to the normal
# methods when absent.
class R:
    def __init__(self, v):
        self.v = v

    def __radd__(self, o):
        return ("radd", o, self.v)

    def __rsub__(self, o):
        return ("rsub", o, self.v)

    def __rmul__(self, o):
        return ("rmul", o, self.v)

    def __rfloordiv__(self, o):
        return ("rfloordiv", o, self.v)


print(10 + R(5), 10 - R(5), 10 * R(5), 17 // R(5))


# __iadd__ is used if present and modifies in place.
class Acc:
    def __init__(self, v):
        self.v = v

    def __iadd__(self, o):
        self.v += o
        return self

    def __repr__(self):
        return f"Acc({self.v})"


a = Acc(1)
b = a
a += 4
print(a, b, a is b)


# Without __iadd__, augmented assignment falls back to __add__.
class NoI:
    def __init__(self, v):
        self.v = v

    def __add__(self, o):
        return NoI(self.v + o)

    def __repr__(self):
        return f"NoI({self.v})"


c = NoI(1)
d = c
c += 4
print(c, d, c is d)

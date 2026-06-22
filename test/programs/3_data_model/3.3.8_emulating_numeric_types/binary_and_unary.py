# 3.3.8 Emulating numeric types: binary arithmetic operators map to dunders, and
# the unary operators -, abs(), ~ map to __neg__/__abs__/__invert__.
class N:
    def __init__(self, v):
        self.v = v

    def __add__(self, o):
        return N(self.v + o)

    def __sub__(self, o):
        return N(self.v - o)

    def __mul__(self, o):
        return N(self.v * o)

    def __truediv__(self, o):
        return N(self.v / o)

    def __floordiv__(self, o):
        return N(self.v // o)

    def __mod__(self, o):
        return N(self.v % o)

    def __pow__(self, o):
        return N(self.v**o)

    def __lshift__(self, o):
        return N(self.v << o)

    def __rshift__(self, o):
        return N(self.v >> o)

    def __and__(self, o):
        return N(self.v & o)

    def __or__(self, o):
        return N(self.v | o)

    def __xor__(self, o):
        return N(self.v ^ o)

    def __neg__(self):
        return N(-self.v)

    def __abs__(self):
        return N(abs(self.v))

    def __invert__(self):
        return N(~self.v)

    def __repr__(self):
        return f"N({self.v})"


n = N(12)
print(n + 3, n - 3, n * 2, n / 5, n // 5, n % 5, n**2)
print(n << 2, n >> 1, n & 6, n | 1, n ^ 5)
print(-n, abs(N(-7)), ~n)

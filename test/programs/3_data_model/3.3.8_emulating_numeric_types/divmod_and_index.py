# 3.3.8 Emulating numeric types: divmod() uses __divmod__ / __rdivmod__, and the
# hex()/bin()/oct() built-ins render an integer (obtained via __index__) with a
# base prefix; the sign sits outside the prefix.
print(hex(255), hex(-255), hex(0), hex(4096))
print(bin(5), bin(-5), bin(0))
print(oct(8), oct(-8), oct(0))

# divmod on ints and floats.
print(divmod(17, 5))
print(divmod(-17, 5))
print(divmod(7.5, 2))
print(divmod(-7.5, 2))

# round() — banker's rounding to ndigits (floats) or powers of ten (ints).
print(round(3.14159, 2), round(2.675, 2), round(2.5), round(3.5))
print(round(17, -1), round(25, -1), round(35, -1), round(12345, -2))
print(round(2.0, 0), round(17, 2), round(True), round(True, 3))

# two- and three-argument pow().
print(pow(2, 10), pow(2, 10, 1000), pow(3, 4, 5), pow(10, -1, 7))


class V:
    def __init__(self, x):
        self.x = x

    def __index__(self):
        return self.x

    def __divmod__(self, o):
        return ("divmod", self.x, o)

    def __rdivmod__(self, o):
        return ("rdivmod", o, self.x)


# __index__ feeds hex/bin/oct and sequence indexing.
print(hex(V(255)), bin(V(6)), oct(V(64)))
print(["a", "b", "c"][V(2)])

# divmod dispatches to the left __divmod__, then the right __rdivmod__.
print(divmod(V(7), 3))
print(divmod(20, V(3)))


def show(thunk):
    try:
        thunk()
    except (TypeError, ValueError, ZeroDivisionError) as e:
        print(type(e).__name__ + ":", e)


show(lambda: divmod("a", 1))
show(lambda: hex("x"))
show(lambda: divmod(1, 0))
show(lambda: pow(2, 3, 0))

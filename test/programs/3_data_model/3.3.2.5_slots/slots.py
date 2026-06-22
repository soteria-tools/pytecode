# 3.3.2.5 __slots__: declaring __slots__ restricts instances to those attribute
# names and removes the per-instance __dict__.
class P:
    __slots__ = ("x", "y")

    def __init__(self, x, y):
        self.x = x
        self.y = y


p = P(1, 2)
print(p.x, p.y)
p.x = 10
print(p.x)


def show(thunk):
    try:
        thunk()
    except AttributeError as e:
        print("AttributeError:", e)


def set_z():
    p.z = 3


show(set_z)
show(lambda: p.__dict__)
show(lambda: p.z)


# A subclass that does not define __slots__ regains a __dict__.
class R(P):
    pass


r = R(1, 2)
r.extra = 99
print(r.x, r.y, r.extra, "extra" in r.__dict__)


# A single-string __slots__ declares one slot.
class S:
    __slots__ = "only"


s = S()
s.only = 5
print(s.only)

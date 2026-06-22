# 3.3.10 Customizing positional arguments in class pattern matching: a class's
# __match_args__ maps positional sub-patterns to attribute names.
class Point:
    __match_args__ = ("x", "y")

    def __init__(self, x, y):
        self.x = x
        self.y = y


def describe(p):
    match p:
        case Point(0, 0):
            return "origin"
        case Point(x, 0):
            return f"x-axis:{x}"
        case Point(x, y):
            return f"point:{x},{y}"


print(describe(Point(0, 0)))
print(describe(Point(5, 0)))
print(describe(Point(2, 3)))


# More positional sub-patterns than __match_args__ allows is a TypeError.
class One:
    __match_args__ = ("a",)

    def __init__(self, a):
        self.a = a


try:
    match One(1):
        case One(p, q):
            print("matched", p, q)
except TypeError as e:
    print("TypeError:", e)

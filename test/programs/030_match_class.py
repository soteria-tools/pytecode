class Point:
    __match_args__ = ("x", "y")
    def __init__(self, x, y):
        self.x = x
        self.y = y

def m(p):
    match p:
        case Point(0, 0):
            return "origin"
        case Point(x, 0) if x > 0:
            return f"pos-x:{x}"
        case Point(x=x, y=y):
            return f"point:{x},{y}"
        case _:
            return "not a point"
print(m(Point(0, 0)))
print(m(Point(5, 0)))
print(m(Point(1, 2)))
print(m("hello"))

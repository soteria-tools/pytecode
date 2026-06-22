class Shape:
    def area(self):
        raise NotImplementedError
class Rect(Shape):
    def __init__(self, w, h):
        self.w, self.h = w, h
    def area(self):
        return self.w * self.h
class Circle(Shape):
    def __init__(self, r):
        self.r = r
    def area(self):
        return 3 * self.r * self.r
shapes = [Rect(2, 3), Circle(2), Rect(1, 1)]
print([s.area() for s in shapes])
print(sum(s.area() for s in shapes))
try:
    Shape().area()
except NotImplementedError:
    print("NotImplementedError")

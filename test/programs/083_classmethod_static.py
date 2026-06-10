class Shape:
    count = 0
    def __init__(self, name):
        self.name = name
        Shape.bump()
    @classmethod
    def bump(cls):
        cls.count += 1
    @classmethod
    def make_square(cls):
        return cls("square")
    @staticmethod
    def describe():
        return "shapes are shapely"
s = Shape.make_square()
print(s.name, Shape.count)
print(Shape.describe(), s.describe())
Shape("circle")
print(Shape.count)

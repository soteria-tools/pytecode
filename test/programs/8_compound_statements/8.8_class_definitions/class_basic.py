class Point:
    def __init__(self, x, y):
        self.x = x
        self.y = y
    def norm1(self):
        return abs(self.x) + abs(self.y)
p = Point(3, -4)
print(p.x, p.y)
print(p.norm1())
p.x = 10
print(p.norm1())
print(type(p).__name__)

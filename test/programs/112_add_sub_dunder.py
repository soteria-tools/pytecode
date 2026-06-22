class Vec:
    def __init__(self, x, y):
        self.x = x
        self.y = y

    def __add__(self, other):
        return Vec(self.x + other.x, self.y + other.y)

    def __sub__(self, other):
        return Vec(self.x - other.x, self.y - other.y)

    def __repr__(self):
        return f"Vec({self.x}, {self.y})"


a = Vec(1, 2)
b = Vec(10, 20)
print(a + b)
print(b - a)
print(a + b - a)
print(a - a)

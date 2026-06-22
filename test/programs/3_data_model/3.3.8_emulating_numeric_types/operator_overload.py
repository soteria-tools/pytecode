class Vec:
    def __init__(self, x, y):
        self.x = x
        self.y = y
    def __add__(self, other):
        return Vec(self.x + other.x, self.y + other.y)
    def __mul__(self, k):
        return Vec(self.x * k, self.y * k)
    def __rmul__(self, k):
        return self * k
    def __neg__(self):
        return Vec(-self.x, -self.y)
    def __repr__(self):
        return f"Vec({self.x}, {self.y})"
a, b = Vec(1, 2), Vec(10, 20)
print(a + b)
print(a * 3)
print(3 * a)
print(-a)
print(a + b * 2)

class Base:
    def __init__(self, x):
        self.x = x
    def describe(self):
        return f"Base({self.x})"
class Child(Base):
    def __init__(self, x, y):
        super().__init__(x)
        self.y = y
    def describe(self):
        return super().describe() + f"+Child({self.y})"
class GrandChild(Child):
    def describe(self):
        return super(GrandChild, self).describe() + "+GC"
g = GrandChild(1, 2)
print(g.x, g.y)
print(g.describe())

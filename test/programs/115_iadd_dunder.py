class Counter:
    def __init__(self, n):
        self.n = n

    def __iadd__(self, other):
        self.n += other
        return self

    def __sub__(self, other):
        return Counter(self.n - other)

    def __repr__(self):
        return f"Counter({self.n})"


c = Counter(10)
c += 5
print(c)
d = c
c += 1
print(c, d)
print(c - 6)
print(c, d)

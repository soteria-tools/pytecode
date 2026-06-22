class Scalar:
    def __init__(self, v):
        self.v = v

    def __radd__(self, other):
        return other + self.v

    def __rsub__(self, other):
        return other - self.v

    def __repr__(self):
        return f"Scalar({self.v})"


s = Scalar(5)
print(10 + s)
print(10 - s)
print(0 + s)
print(sum([Scalar(1), Scalar(2), Scalar(3)]))

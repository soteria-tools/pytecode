class Frac:
    def __init__(self, num, den):
        self.num = num
        self.den = den
    def __repr__(self):
        return f"Frac({self.num}, {self.den})"
    def __str__(self):
        return f"{self.num}/{self.den}"
f = Frac(1, 2)
print(f)
print(repr(f))
print([f, f])
print(f"{f}")
class OnlyRepr:
    def __repr__(self):
        return "<OnlyRepr>"
print(OnlyRepr(), str(OnlyRepr()))

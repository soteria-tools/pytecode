# 3.2.2 NotImplemented: a type with a single value, accessed through the
# built-in name NotImplemented. Numeric and rich-comparison methods return it
# when they don't implement an operation; the interpreter then tries the
# reflected operation or another fallback.
print(NotImplemented)
print(repr(NotImplemented))
print(type(NotImplemented).__name__)
print(NotImplemented is NotImplemented)


class Money:
    def __init__(self, cents):
        self.cents = cents

    def __add__(self, other):
        if isinstance(other, Money):
            return Money(self.cents + other.cents)
        return NotImplemented

    def __radd__(self, other):
        if other == 0:
            return Money(self.cents)
        return NotImplemented

    def __eq__(self, other):
        if isinstance(other, Money):
            return self.cents == other.cents
        return NotImplemented

    def __repr__(self):
        return f"Money({self.cents})"


print(Money(150) + Money(50))
print(sum([Money(10), Money(20)]))
print(Money(100) == Money(100))
print(Money(100) == Money(25))
# __eq__ returns NotImplemented for non-Money, so == falls back to identity.
print(Money(100) == 100)
print(Money(100) != 100)

class A:
    def __add__(self, other):
        if isinstance(other, B):
            return NotImplemented
        return "A.__add__"


class B:
    def __radd__(self, other):
        if isinstance(other, A):
            return "B.__radd__"
        return NotImplemented


print(A() + 1)
print(A() + B())

try:
    B() + B()
except TypeError as e:
    print("TypeError:", e)

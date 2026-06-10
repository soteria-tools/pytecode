class A:
    def who(self):
        return "A"
class B(A):
    pass
class C(A):
    def who(self):
        return "C"
class D(B, C):
    pass
print([cls.__name__ for cls in D.__mro__])
print(D().who())
print(issubclass(D, A), issubclass(B, C))

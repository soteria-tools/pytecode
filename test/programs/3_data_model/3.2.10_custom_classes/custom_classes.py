# 3.2.10 Custom classes: attribute references resolve via the class __dict__ and
# then along the MRO of base classes; class objects are callable.
class A:
    "an A"
    kind = "base"

    def who(self):
        return "A"


class B(A):
    flavor = "b"

    def who(self):
        return "B"


# C.x is C.__dict__["x"]; names not found there are looked up along the MRO.
print(B.flavor, B.__dict__["flavor"])
print(B.kind)
print(B.__name__, B.__qualname__)
print(B.__bases__ == (A,))
print([c.__name__ for c in B.__mro__])
print(A.__doc__, B.__doc__)
print("flavor" in B.__dict__, "kind" in B.__dict__)

# Class attribute assignment updates the class's own dict, not a base's.
B.extra = 1
print(B.extra, "extra" in B.__dict__, "extra" in A.__dict__)

# A class object is callable, yielding an instance.
b = B()
print(type(b).__name__, b.who())


# A classmethod is bound to the class; a staticmethod is returned unwrapped.
class K:
    @classmethod
    def cm(cls):
        return cls.__name__

    @staticmethod
    def sm(x):
        return x + 1


print(K.cm(), K.cm.__self__ is K)
print(K.sm(41))

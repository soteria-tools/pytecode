# 3.3.1 / 6.10.1 / 3.3.8: reflected-operand priority. If the right operand's type
# is a proper subclass of the left's and overrides the reflected method, that
# reflected method is tried before the left operand's method.
class Base:
    def __add__(self, o):
        return "Base.__add__"


class Sub(Base):
    def __radd__(self, o):
        return "Sub.__radd__"


print(Base() + Sub())


class Sub2(Base):
    pass


print(Base() + Sub2())


# For comparisons the priority is observed via which method runs (the methods
# return bools so the result is well-defined regardless of coercion).
class C:
    def __gt__(self, o):
        print("C.__gt__")
        return True


class D(C):
    def __lt__(self, o):
        print("D.__lt__")
        return False


print(C() > D())


class E:
    def __eq__(self, o):
        print("E.__eq__")
        return False


class F(E):
    def __eq__(self, o):
        print("F.__eq__")
        return True


print(E() == F())

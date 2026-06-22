# 3.3.4 Customizing instance and subclass checks: a metaclass __instancecheck__
# overrides isinstance(x, C) and __subclasscheck__ overrides issubclass(x, C).
# These hooks are looked up on the metaclass (the type of the class), not on the
# class itself.
class Meta(type):
    def __instancecheck__(cls, inst):
        print("  __instancecheck__", cls.__name__, inst)
        return isinstance(inst, int) and inst > 0

    def __subclasscheck__(cls, sub):
        print("  __subclasscheck__", cls.__name__, sub.__name__)
        return sub in (int, bool)


class Positive(metaclass=Meta):
    pass


print(isinstance(5, Positive))
print(isinstance(-1, Positive))
print(isinstance("x", Positive))
print(issubclass(int, Positive))
print(issubclass(bool, Positive))
print(issubclass(str, Positive))


# Defining these as ordinary methods on the class itself has no effect: they are
# only consulted on the metaclass.
class Plain:
    def __instancecheck__(self, inst):
        return True


print(isinstance(42, Plain))  # uses the default check -> False


# A tuple second argument still works (default behaviour, no hook).
print(isinstance(3, (str, int)))
print(issubclass(bool, (str, int)))

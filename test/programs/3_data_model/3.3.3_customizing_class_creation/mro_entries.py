# 3.3.3.1 Resolving MRO entries: when a base listed in a class definition is not
# a class, its __mro_entries__(bases) is called and the returned classes are used
# instead; the original bases are remembered in __orig_bases__.
class Base:
    pass


class Proxy:
    def __init__(self, *bases):
        self.bases = bases

    def __mro_entries__(self, bases):
        print("  __mro_entries__ given", len(bases), "base(s)")
        return self.bases


p = Proxy(Base)


class C(p):
    val = 7


print(C.__bases__)
print(C.__mro__)
print(C.__orig_bases__ is p)
print(issubclass(C, Base), C().val)


# An empty __mro_entries__ drops the base entirely (so C falls back to object).
class Drop:
    def __mro_entries__(self, bases):
        return ()


class D(Drop()):
    pass


print(D.__bases__)


# A class without __orig_bases__ does not gain the attribute.
class Plain(Base):
    pass


print(hasattr(Plain, "__orig_bases__"))

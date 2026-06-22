# 6.2.1 Identifiers (Names): a bound name evaluates to its object; an unbound
# name raises NameError.
x = 42
print(x)
try:
    print(undefined_name)
except NameError as e:
    print("NameError:", e)


# Private name mangling: an identifier __name textually inside class Foo is
# transformed to _Foo__name.
class Foo:
    def __init__(self):
        self.__secret = 1

    def get(self):
        return self.__secret


f = Foo()
print(f.get())
print("_Foo__secret" in f.__dict__)
print(f._Foo__secret)
print(hasattr(f, "__secret"))


# A name with a trailing double underscore is not a private name (not mangled).
class Bar:
    def __init__(self):
        self.__dunder__ = 7

    def get(self):
        return self.__dunder__


b = Bar()
print(b.get(), "__dunder__" in b.__dict__)

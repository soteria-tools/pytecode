# 3.3.3 Metaclasses: the class statement's metaclass keyword selects the type
# used to build the class. type(C) is the metaclass; the metaclass __new__ and
# __init__ run during class creation, and a class is an instance of its
# metaclass.
class Meta(type):
    def __new__(mcs, name, bases, ns, **kw):
        print("Meta.__new__", name, sorted(kw.items()))
        ns["tag"] = "made-by-meta"
        return super().__new__(mcs, name, bases, ns)

    def __init__(cls, name, bases, ns, **kw):
        print("Meta.__init__", name)
        super().__init__(name, bases, ns)


class C(metaclass=Meta):
    pass


print(type(C) is Meta)
print(type(C).__name__)
print(isinstance(C, Meta), isinstance(C, type))
print(C.tag)


# Keyword arguments in the class statement reach __new__/__init__.
class D(metaclass=Meta, color="red", size=3):
    pass


# A subclass without its own metaclass inherits the metaclass of its bases.
class E(C):
    pass


print(type(E) is Meta, isinstance(E, Meta))


# A user metaclass can override instance creation through __call__.
class Counting(type):
    def __call__(cls, *args, **kw):
        print("Counting.__call__", cls.__name__)
        return super().__call__(*args, **kw)


class Widget(metaclass=Counting):
    def __init__(self, label):
        self.label = label


w = Widget("ok")
print(w.label)


# type itself is the metaclass of ordinary classes, and type(name, bases, ns)
# builds one whose metaclass is type.
class Plain:
    pass


print(type(Plain) is type)
X = type("X", (), {"a": 1})
print(type(X) is type, X.a)


# A metaclass __instancecheck__ / __subclasscheck__ overrides isinstance and
# issubclass.
class CheckMeta(type):
    def __instancecheck__(cls, inst):
        return inst == 42

    def __subclasscheck__(cls, sub):
        return sub is int


class Magic(metaclass=CheckMeta):
    pass


print(isinstance(42, Magic), isinstance(7, Magic))
print(issubclass(int, Magic), issubclass(str, Magic))


# 3.3.3.3 __prepare__: the metaclass may supply the namespace the class body
# populates; entries it injects are visible during creation and on the class.
class PrepMeta(type):
    @classmethod
    def __prepare__(mcs, name, bases, **kw):
        return {"injected": "from-prepare"}

    def __new__(mcs, name, bases, ns, **kw):
        print("seen in ns:", "injected" in ns, "body" in ns)
        return super().__new__(mcs, name, bases, ns)


class Prepared(metaclass=PrepMeta):
    body = 1


print(Prepared.injected, Prepared.body)


# Incompatible metaclasses among bases is a TypeError.
class M1(type):
    pass


class M2(type):
    pass


class A1(metaclass=M1):
    pass


class A2(metaclass=M2):
    pass


try:

    class Bad(A1, A2):
        pass

except TypeError as e:
    print("TypeError:", e)

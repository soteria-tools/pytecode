# 3.3.3 Customizing class creation: __init_subclass__ and __set_name__.

# __init_subclass__ is called on the parent whenever it is subclassed; cls is
# the new subclass and it is implicitly a classmethod. Keyword arguments from
# the class definition are passed to it.
class Base:
    def __init_subclass__(cls, /, label="none", **kwargs):
        super().__init_subclass__(**kwargs)
        print("init_subclass", cls.__name__, label)
        cls.label = label


class A(Base):
    pass


class B(Base, label="bee"):
    pass


print(A.label, B.label)


# __set_name__ is called for each class variable that defines it, with the
# owner class and the attribute name, in definition order.
class Field:
    def __set_name__(self, owner, name):
        print("set_name", owner.__name__, name)
        self.name = name

    def __repr__(self):
        return f"Field({self.name!r})"


class Model:
    x = Field()
    y = Field()


print(Model.x, Model.y)


# object.__init_subclass__ takes no arguments.
try:

    class NoHook:
        pass

    class Bad(NoHook, oops=1):
        pass

except TypeError as e:
    print("TypeError:", e)

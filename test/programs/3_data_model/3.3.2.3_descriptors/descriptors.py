# 3.3.2.3/3.3.2.4 Descriptors: an object stored in a class that defines
# __get__/__set__/__delete__. A data descriptor (defines __set__/__delete__)
# overrides the instance dict; a non-data descriptor (only __get__) does not.
class Data:
    def __get__(self, inst, owner):
        return "data.get"

    def __set__(self, inst, value):
        inst.__dict__["_d"] = value
        print("data.set", value)


class NonData:
    def __get__(self, inst, owner):
        return "nondata.get"


class C:
    d = Data()
    n = NonData()


c = C()
print(c.d)
c.d = 5
print(c.__dict__.get("_d"))
print(c.n)
c.__dict__["n"] = "shadow"
print(c.n)


# __get__ receives (instance, owner); accessed via the class, instance is None.
class Logger:
    def __get__(self, inst, owner):
        return f"inst_is_none={inst is None} owner={owner.__name__}"


class Host:
    x = Logger()


print(Host().x)
print(Host.x)


# __delete__ on a data descriptor handles attribute deletion.
class Deletable:
    def __get__(self, inst, owner):
        return "v"

    def __delete__(self, inst):
        print("deleted")


class D:
    z = Deletable()


del D().z

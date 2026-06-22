# 3.2.11 Class instances: attribute lookup checks the instance dict first, then
# the class; assignments and deletions update the instance dict, never the class.
class C:
    shared = "class"

    def method(self):
        return "m"


c = C()
print(c.__class__ is C, type(c) is C)
print(c.shared)
c.shared = "instance"
print(c.shared, C.shared)
print(c.__dict__)
print("shared" in C.__dict__, "shared" in c.__dict__)
del c.shared
print(c.shared, c.__dict__)

# A user-defined function on the class becomes a bound method via the instance.
print(c.method(), c.method.__self__ is c)


# __getattr__ is consulted only when normal attribute lookup fails.
class D:
    def __getattr__(self, name):
        return "default:" + name


d = D()
d.real = 1
print(d.real, d.missing)

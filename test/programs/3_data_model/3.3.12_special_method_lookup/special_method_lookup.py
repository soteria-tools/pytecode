# 3.3.12 Special method lookup: implicit invocations of special methods look the
# method up on the object's type, not in the instance dictionary.
class C:
    pass


c = C()
c.__len__ = lambda: 5
try:
    len(c)
except TypeError as e:
    print("TypeError:", e)
# Explicit lookup via the instance does see the instance attribute.
print(c.__len__())


# A special method defined on the class is used implicitly.
class D:
    def __len__(self):
        return 7


print(len(D()))


# Assigning the special method on the class (after creation) also works, since
# the lookup is on the type.
class E:
    pass


E.__len__ = lambda self: 3
print(len(E()))

# 3.3.5 Emulating generic types: __class_getitem__ implements Class[key] on the
# class object and is implicitly a classmethod.
class Stack:
    def __class_getitem__(cls, item):
        return f"{cls.__name__}[{item.__name__}]"


print(Stack[int])
print(Stack[str])


# Subscripting an instance uses __getitem__; subscripting the class uses
# __class_getitem__.
class Container:
    def __class_getitem__(cls, item):
        return "class_getitem"

    def __getitem__(self, key):
        return f"getitem:{key}"


print(Container[int])
print(Container()[5])


# A class without __class_getitem__ is not subscriptable.
class Plain:
    pass


try:
    Plain[int]
except TypeError as e:
    print("TypeError:", e)

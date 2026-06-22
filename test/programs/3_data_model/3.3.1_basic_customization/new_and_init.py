# 3.3.1 Basic customization: __new__ creates the instance, __init__ initialises
# it. __init__ runs only if __new__ returned an instance of cls, and must
# return None.
class Point:
    def __new__(cls, *args):
        print("new", args)
        return object.__new__(cls)

    def __init__(self, x, y):
        print("init", x, y)
        self.x = x
        self.y = y

    def __repr__(self):
        return f"Point({self.x}, {self.y})"


p = Point(1, 2)
print(p)


# If __new__ returns something that is not an instance of cls, __init__ is
# not invoked.
class Weird:
    def __new__(cls):
        return 42

    def __init__(self):
        print("init should not run")


print(Weird())


# __init__ must return None.
class BadInit:
    def __init__(self):
        return 7


try:
    BadInit()
except TypeError as e:
    print("TypeError:", e)

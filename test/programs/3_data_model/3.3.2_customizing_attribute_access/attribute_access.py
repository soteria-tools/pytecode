# 3.3.2 Customizing attribute access.

# __getattr__ is consulted only when normal attribute lookup fails.
class Lazy:
    def __init__(self):
        self.real = 1

    def __getattr__(self, name):
        return f"computed:{name}"


x = Lazy()
print(x.real, x.missing)


# __setattr__ intercepts every assignment; delegate with object.__setattr__.
class Logged:
    def __init__(self):
        object.__setattr__(self, "log", [])

    def __setattr__(self, name, value):
        self.log.append((name, value))
        object.__setattr__(self, name, value)


y = Logged()
y.a = 1
y.b = 2
print(y.a, y.b, y.log)


# __delattr__ intercepts deletion.
class Guard:
    def __init__(self):
        self.x = 1

    def __delattr__(self, name):
        print("del", name)
        object.__delattr__(self, name)


g = Guard()
del g.x
print(hasattr(g, "x"))


# __getattribute__ runs unconditionally; raising AttributeError triggers __getattr__.
class Watch:
    def __getattribute__(self, name):
        if name == "secret":
            raise AttributeError(name)
        return object.__getattribute__(self, name)

    def __getattr__(self, name):
        return "fallback:" + name

    def m(self):
        return "method"


wt = Watch()
print(wt.m(), wt.secret)


# __dir__ customizes dir(); the result is converted to a sorted list.
class WithDir:
    def __dir__(self):
        return ["b", "a", "c"]


print(dir(WithDir()))

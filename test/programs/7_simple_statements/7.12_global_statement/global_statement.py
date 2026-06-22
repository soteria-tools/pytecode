# 7.12 The global statement: lets a function rebind (or create) a module-level
# global; a free variable can read a global without declaring it.
counter = 0


def bump():
    global counter
    counter += 1


bump()
bump()
bump()
print(counter)

x = "module"


def set_global():
    global x
    x = "changed"


def read_free():
    return x


set_global()
print(x, read_free())


def make():
    global created
    created = 42


make()
print(created)

# 8.5 The with statement: multiple items behave like nested with statements, so
# __exit__ runs in reverse order; an exception is passed to __exit__ and is
# suppressed when __exit__ returns true.
class CM:
    def __init__(self, name, suppress=False):
        self.name = name
        self.suppress = suppress

    def __enter__(self):
        print("enter", self.name)
        return self.name

    def __exit__(self, *args):
        print("exit", self.name, args[0].__name__ if args[0] else None)
        return self.suppress


with CM("a") as a, CM("b") as b:
    print("body", a, b)

print("---")
with CM("outer") as o:
    with CM("inner", suppress=True) as i:
        raise ValueError("boom")
print("after")

print("---")
with (CM("x"), CM("y") as y):
    print("body2", y)

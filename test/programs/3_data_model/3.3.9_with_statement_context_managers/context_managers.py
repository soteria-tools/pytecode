# 3.3.9 With Statement Context Managers: __enter__'s return value is bound to the
# `as` target; __exit__ receives (exc_type, exc_value, traceback) or (None, None,
# None) on a clean exit, and returning a true value suppresses the exception.
class CM:
    def __init__(self, label, suppress=False):
        self.label = label
        self.suppress = suppress

    def __enter__(self):
        print("enter", self.label)
        return self.label.upper()

    def __exit__(self, et, ev, tb):
        print("exit", self.label, et.__name__ if et else None, str(ev) if ev else None)
        return self.suppress


with CM("a") as x:
    print("body", x)

print("---")
with CM("b", suppress=True):
    raise ValueError("boom")
print("after suppressed")

print("---")
try:
    with CM("c"):
        raise KeyError("k")
except KeyError as e:
    print("propagated", e)

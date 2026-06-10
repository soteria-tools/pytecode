class Thing:
    kind = "generic"
    def __init__(self):
        self.color = "red"
t = Thing()
print(getattr(t, "color"), getattr(t, "kind"))
print(getattr(t, "missing", "fallback"))
setattr(t, "size", 42)
print(t.size, hasattr(t, "size"), hasattr(t, "nope"))
delattr(t, "size")
print(hasattr(t, "size"))
print(type(t) is Thing, type(Thing).__name__)
print(isinstance(t, object), isinstance(Thing, type))

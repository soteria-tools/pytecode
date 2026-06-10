class Lazy:
    def __init__(self):
        self.real = 1
    def __getattr__(self, name):
        if name.startswith("magic_"):
            return name.upper()
        raise AttributeError(name)
l = Lazy()
print(l.real)
print(l.magic_word)
try:
    l.other
except AttributeError as e:
    print("AttributeError", e)
print(getattr(l, "magic_x", "?"), getattr(l, "boring", "?"))

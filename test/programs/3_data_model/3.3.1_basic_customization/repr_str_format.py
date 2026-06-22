# 3.3.1 __repr__ / __str__ / __format__.
class Both:
    def __repr__(self):
        return "Both.repr"

    def __str__(self):
        return "Both.str"


class ReprOnly:
    def __repr__(self):
        return "ReprOnly.repr"


b = Both()
print(repr(b), str(b))
print(b)
r = ReprOnly()
# When a class defines __repr__ but not __str__, __repr__ is used for str too.
print(repr(r), str(r), r)


# format(), f-strings and str.format delegate to __format__.
class WithFormat:
    def __format__(self, spec):
        return f"fmt[{spec}]"

    def __str__(self):
        return "str!"


w = WithFormat()
print(format(w), format(w, ">10"))
print(f"{w}", f"{w:x<5}", "{}".format(w))


# object.__format__ with an empty spec equals str(x); a non-empty spec is an error.
class Plain:
    def __str__(self):
        return "plain"


p = Plain()
print(format(p), format(p, ""))
try:
    format(p, "x")
except TypeError as e:
    print("TypeError:", e)


# __repr__ must return a string.
class BadRepr:
    def __repr__(self):
        return 123


try:
    repr(BadRepr())
except TypeError as e:
    print("TypeError:", e)

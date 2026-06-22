# printf-style string formatting: the % operator on a string. The right operand
# is a tuple of positional values, a single value, or a mapping for %(key) specs.
print("%d %s %05.2f" % (42, "hi", 3.14159))
print("%x %X %o %e %g" % (255, 255, 8, 1234.5, 0.0001))
print("%-10s|%+d|%r|%a" % ("left", 5, "x", "café"))
print("%5.2f|%-8.3f|%+08.2f" % (3.14159, 2.5, 1.5))
print("%i %d" % (3.9, -3.9))  # %d/%i truncate floats toward zero
print("%#x %#o %#b" % (255, 8, 5) if False else "%#x %#o" % (255, 8))
print("%c%c%c" % (72, 105, 33))  # %c from codepoints
print("%c" % "Z")  # %c from a one-char string

# A single (non-tuple) value, including a list or dict, is one argument.
print("%s" % [1, 2, 3])
print("%r" % {1: 2})
print("%s" % 42)

# A mapping supplies %(key) specifiers.
print("%(name)s is %(age)d" % {"name": "Al", "age": 30})
print("%(x)05.1f" % {"x": 3.14159})

# Literal percent and no-specifier strings.
print("100%% sure" % ())
print("no specifiers")


def show(thunk):
    try:
        thunk()
    except (TypeError, ValueError, KeyError) as e:
        print(type(e).__name__ + ":", e)


show(lambda: "%d %d" % (1,))
show(lambda: "%d" % (1, 2))
show(lambda: "%(missing)s" % {"a": 1})

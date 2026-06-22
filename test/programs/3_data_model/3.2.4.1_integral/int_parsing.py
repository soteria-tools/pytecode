# 3.2.4.1 Integral: the int(str, base) constructor accepts bases 2..36 or 0
# (auto-detect from prefix), the matching 0x/0o/0b prefix, an optional sign,
# surrounding whitespace, and single underscores between digits.
print(int("0xff", 16), int("ff", 16), int("0b101", 2), int("777", 8), int("0o777", 8))
print(int("  42  "), int("-0xFF", 16), int("+10", 2), int("z", 36), int("Zz", 36))
print(int("1_000"), int("0xFF_FF", 16), int("DEAD", 16))
print(int("10", 0), int("0x10", 0), int("0o17", 0), int("0b1010", 0))
print(int("1010", 2), int("-101", 2), int("0"), int("-0"))


def show(thunk):
    try:
        thunk()
    except ValueError as e:
        print("ValueError:", e)


show(lambda: int("0xff", 10))
show(lambda: int("", 16))
show(lambda: int("1__0"))
show(lambda: int("_10"))
show(lambda: int("10_"))
show(lambda: int("12", 1))
show(lambda: int("0x", 16))
show(lambda: int("g", 16))

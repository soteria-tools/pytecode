# 6.5 The power operator: binds tighter than unary minus on its left, looser on
# its right; ** is right-associative; int**int is int unless the exponent is
# negative (then float). Raising zero to a negative power is a ZeroDivisionError.
print(-1**2)
print(2**3**2)
print(10**2, 10**-2)
print(type(10**2).__name__, type(10**-2).__name__)
print(2**0.5)
try:
    0.0**-1
except ZeroDivisionError as e:
    print("ZeroDivisionError:", e)
try:
    0**-2
except ZeroDivisionError as e:
    print("ZeroDivisionError:", e)


# 6.6 Unary arithmetic and bitwise operations: -, +, ~ (with ~x == -(x+1)).
print(-5, +5, ~5)
print(~0, ~-1)
print(+3.5, -3.5)
print(~7 == -(7 + 1))
print(- -5, +-3)

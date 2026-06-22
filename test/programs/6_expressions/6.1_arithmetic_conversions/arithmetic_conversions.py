# 6.1 Arithmetic conversions: when an operator converts its numeric operands to
# a common type, complex wins over floating-point wins over integer (and bool is
# an integer).
print(type(1 + 2).__name__)
print(type(1 + 2.0).__name__)
print(type(2.0 + 1).__name__)
print(type(1 + 2j).__name__)
print(type(1.5 + 2j).__name__)
print(type(True + 1).__name__)
print(type(True + 1.0).__name__)
print(type(True + 2j).__name__)

print(1 + 2.0, 2.0 + 1, 3 / 2)
print(1 + 2j, 1.5 + 2j, True + 2j)

# The power operator promotes to float for a negative integer exponent.
print(2**2, type(2**2).__name__, type(2**-1).__name__)

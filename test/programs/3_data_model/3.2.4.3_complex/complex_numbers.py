# 3.2.4.3 Complex: complex numbers held as a pair of double-precision floats;
# the real and imaginary parts are read via z.real and z.imag.
print(0j, 3j, -3j)
print(1 + 2j, 1 - 2j, -1 + 2j, -1 - 2j)
print(repr(0j), repr(2 + 0j), repr(complex(-0.0, 0.0)))
print(complex(), complex(5), complex(1.5, -2.5))
print(type(2j).__name__, isinstance(2j, complex))

z = 3 + 4j
print(z.real, z.imag, z.conjugate())
print(abs(3 + 4j))

# Arithmetic; int/float operands are converted to complex.
print((1 + 2j) + (3 + 4j), (1 + 2j) - (3 + 4j), (1 + 2j) * (3 + 4j))
print((1 + 2j) / (3 + 4j))
print(1 + 2j, 2j + 1, 1.5 + 2j, 2j * 3)
print(2j ** 2, (1 + 1j) ** 2)

# Equality across numeric types; complex supports no ordering.
print(1 + 0j == 1, 1j == 1, complex(1, 2) == complex(1, 2))
try:
    1j < 2j
except TypeError as e:
    print("TypeError:", e)

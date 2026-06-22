# 3.2.4.2 Real (float): machine-level double-precision floating point.
print(repr(0.1), repr(1.5), repr(-0.0))
print(1.0 + 2.0, 10.0 / 3.0)
print(2.0 ** 0.5)

# Infinities and NaN.
print(float("inf"), float("-inf"))
print(1e308 * 10)
nan = float("nan")
print(nan != nan)
print(nan == nan)
print(nan < 1.0, nan > 1.0, nan == nan)

# Rounding artefacts of binary floating point.
print(0.1 + 0.2)

# Scientific-notation thresholds in repr.
print(repr(1e16), repr(1e17), repr(1e-5), repr(1.23e-10))
print(3.0.is_integer(), 3.5.is_integer())

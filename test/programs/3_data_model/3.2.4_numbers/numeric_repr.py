# 3.2.4 Numbers: the string representations (repr/str) are valid base-10
# numeric literals, with no superfluous leading/trailing zeros and a sign shown
# only when the number is negative.
print(repr(0), repr(7), repr(-7))
print(repr(0.0), repr(-0.0), repr(1.0), repr(-2.5))
print(str(10), str(10.0), str(-10.0))
print(repr(1000000), repr(0.5))

# repr produces a valid literal of the same value (round-trips).
print(int(repr(42)) == 42, float(repr(3.14)) == 3.14)

# No trailing zeros except a single one after the decimal point.
print(repr(100.0), repr(1.50), repr(2.000))

# Numeric objects are immutable; arithmetic yields new values.
n = 3
m = n + 1
print(n, m)

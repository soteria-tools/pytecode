# 3.2.4.1 Integral: int has unlimited range; bool is a subtype of int whose two
# values behave like 0 and 1 except that str() gives "False"/"True".
print(issubclass(bool, int))
print(isinstance(True, int))
print(True == 1, False == 0)
print(True + 1, False * 5, True * True, True + True + True)
print(int(True), int(False))
print(str(True), str(False))

# Unlimited integer range.
print(2 ** 200)
print(10 ** 30 + 1)

# Shift and mask on negatives: 2's-complement illusion of infinite sign bits.
print(-7 >> 1, -1 & 0xFF, -1 >> 100)
print(~0, ~5)

# bool used as an index behaves like 0/1.
xs = ["zero", "one"]
print(xs[False], xs[True])

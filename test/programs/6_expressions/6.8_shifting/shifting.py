# 6.8 Shifting operations: integer operands; x << n == x * 2**n and
# x >> n == x // 2**n (floor division, so right shift rounds toward -inf).
print(1 << 4, 100 >> 2)
print(5 << 3 == 5 * 2**3, -20 >> 2 == -20 // 2**2)
print((-1) >> 1, (-8) >> 1, (-7) >> 1)
print(1 << 100)
try:
    1 << -1
except ValueError as e:
    print("ValueError:", e)

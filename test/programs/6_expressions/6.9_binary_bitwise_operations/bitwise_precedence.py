# 6.9 Binary bitwise operations: & yields AND, ^ XOR, | OR; & binds tighter than
# ^, which binds tighter than |.
print(5 & 3, 5 ^ 3, 5 | 3)
print(1 | 2 & 3, 5 ^ 3 & 1, 1 | 4 ^ 2 & 3)
print(0xF0 & 0x0F, 0xF0 | 0x0F, 0xFF ^ 0x0F)
print(-1 & 0xFF, ~0 & 0xFF)

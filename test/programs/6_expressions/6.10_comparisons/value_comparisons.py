# 6.10.1 Value comparisons: ==/!=/</<=/>/>= compare values; chaining a<b<c means
# (a<b and b<c) with each operand evaluated at most once and short-circuiting.
print(1 < 2 < 3, 3 < 2 < 1, 1 < 2 > 0)


def t(label, v):
    print("eval", label)
    return v


print("result:", t("a", 5) < t("b", 1) < t("c", 99))

# Cross-type numeric equality, and the IEEE NaN rules (every ordered comparison
# with NaN is false; NaN is not equal to itself).
print(1 == 1.0, 1 == True, 1.0 == True)
nan = float("nan")
print(3 < nan, nan < 3, nan <= 3, nan >= 3, 3 <= nan, 3 >= nan)
print(nan == nan, nan != nan)


# Default equality is identity; default ordering raises TypeError.
class C:
    pass


a, b = C(), C()
print(a == a, a == b, a != b)
try:
    a < b
except TypeError as e:
    print("TypeError:", e)


# Sequences compare lexicographically; equality requires the same type.
print([1, 2] == (1, 2), [1, 2, 3] < [1, 2, 4], [1, 2] < [1, 2, 3])
print((1, 2, 3) <= (1, 2, 3), "abc" < "abd", "Z" < "a")
try:
    [1, 2] < (1, 2)
except TypeError as e:
    print("TypeError:", e)

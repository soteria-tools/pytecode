# 3.2.5 Sequences: finite ordered collections indexed by 0..n-1; len() gives
# the count; a[i] selects item i; negative indices add the length; slicing
# a[i:j] (and extended a[i:j:k]) yields a sequence of the same type.
for a in ["abcde", (10, 20, 30, 40, 50), [1, 2, 3, 4, 5]]:
    n = len(a)
    print(n, a[0], a[n - 1], a[-1], a[-2])
    print(a[1:3], a[:2], a[3:], a[:])
    print(a[::2], a[1:4:2], a[::-1])
    print(type(a[1:3]) is type(a))

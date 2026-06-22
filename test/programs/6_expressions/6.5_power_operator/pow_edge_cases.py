# 6.5 The power operator — overflow, complex results, and zero-division.
def show(thunk):
    try:
        print("=>", thunk())
    except (OverflowError, ZeroDivisionError, ValueError) as e:
        print(type(e).__name__ + ":", e)


# A finite float ** finite that overflows raises OverflowError.
show(lambda: 2.0 ** 10000)
show(lambda: 10.0 ** 400)

# A negative base raised to a non-integer power yields a complex number.
show(lambda: (-1.0) ** 0.5)
show(lambda: (-8.0) ** (1.0 / 3.0))
print((-1) ** 0.5)
print(complex(-1, 0) ** 0.5)
print((2 + 0j) ** 0.5)

# Zero to a negative power.
show(lambda: 0.0 ** -1)
show(lambda: 0 ** -1)

# Ordinary cases.
print(2 ** 10, 2 ** 0, 2 ** -1, 2.0 ** 3, (-2) ** 3, (-2) ** 2)
print(2 ** 100, pow(3, 4), pow(2, 10, 1000))
print(0 ** 0, 0.0 ** 0, 1 ** 1000000)

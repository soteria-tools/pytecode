# 3.2.4 Numbers: int/float methods and numeric-tower attributes.
print((258).to_bytes(2, "big"), (258).to_bytes(2, "little"))
print((255).to_bytes(1, "big"), (1).to_bytes(4, "big"))
print((255).to_bytes(2), (255).to_bytes())
print(int.from_bytes(b"\x01\x02", "big"), int.from_bytes(b"\x01\x02", "little"))
print(int.from_bytes(b"\xff", "big"))
print((10).bit_length(), (255).bit_length(), (0).bit_length())
print((255).bit_count(), (7).bit_count())

# numeric-tower attributes.
print((5).numerator, (5).denominator, (5).real, (5).imag, (5).conjugate())
print(True.numerator, True.real, False.imag)
print((3.14).real, (3.14).imag, (2.5).conjugate())
print((3.0).is_integer(), (3.5).is_integer())
print((3.0).as_integer_ratio(), (0.5).as_integer_ratio(), (0.1).as_integer_ratio())


def show(thunk):
    try:
        thunk()
    except (OverflowError, ValueError) as e:
        print(type(e).__name__ + ":", e)


show(lambda: (-1).to_bytes(2, "big"))
show(lambda: (256).to_bytes(1, "big"))

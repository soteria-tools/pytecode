# 3.2.5.2 bytearray: a mutable byte array; same interface as bytes but with
# in-place item/slice assignment, deletion, and append/extend.
ba = bytearray(b"abc")
print(ba, type(ba).__name__, len(ba))
print(ba[0], ba[1:])
ba[0] = 65
print(ba)
ba.append(33)
print(ba)
del ba[0]
print(ba)
print(bytearray(3), bytearray([72, 73]), bytearray("hé", "utf-8"), bytearray())
print(bytearray(b"ab") + b"cd", bytearray(b"xy") * 2)
print(bytearray(b"abc") == b"abc", bytearray(b"a") < bytearray(b"b"))
print(list(bytearray(b"AB")), 65 in bytearray(b"AB"))
print(bytearray(b"h\xc3\xa9").decode())
print(isinstance(ba, bytearray), isinstance(b"x", bytearray), isinstance(ba, bytes))

ba2 = bytearray(b"12345")
ba2[1:3] = b"XY"
print(ba2)
ba2.extend(b"!!")
print(ba2)


def show(thunk):
    try:
        thunk()
    except (TypeError, ValueError) as e:
        print(type(e).__name__ + ":", e)


def bad_set():
    bytearray(b"x")[0] = 300


show(lambda: hash(bytearray(b"a")))
show(bad_set)

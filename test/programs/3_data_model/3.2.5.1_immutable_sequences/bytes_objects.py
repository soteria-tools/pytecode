# 3.2.5.1 Bytes: an immutable array of 8-bit integers (0..255). Indexing yields
# an int; slicing yields bytes; bytes support concatenation, repetition,
# lexicographic comparison, iteration (over ints), membership, and decode().
b = b"abc"
print(b, type(b).__name__, len(b))
print(b[0], b[1], b[-1])
print(b[1:], b[:2], b[::-1])
print(b + b"de", b * 2)
print(b"abc" == b"abc", b"abc" < b"abd", b"a" < b"ab")
print(list(b), list(b"\x00\xff"))
print(bytes(3), bytes([65, 66, 67]), bytes("héllo", "utf-8"))
print(repr(b"a\tb\nc"), repr(b"quote'\""), repr(b"\x00\x1f\x7f\xff"))
print(b"abc".decode(), b"h\xc3\xa9".decode("utf-8"))
print(97 in b"abc", 200 in b"abc", b"bc" in b"abc")
d = {b"k": 1}
print(d[b"k"])
for x in b"AB":
    print(x)


def show(thunk):
    try:
        thunk()
    except (TypeError, ValueError) as e:
        print(type(e).__name__ + ":", e)


show(lambda: b"abc"["x"])
show(lambda: b"abc" + "x")
show(lambda: bytes("hi"))
show(lambda: bytes([300]))

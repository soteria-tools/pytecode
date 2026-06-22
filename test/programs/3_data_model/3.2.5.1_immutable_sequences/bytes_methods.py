# 3.2.5.1 bytes — the byte-oriented methods mirror str's; results stay bytes.
b = b"Hello World"
print(b.upper(), b.lower())
print(b.replace(b"l", b"L"), b.replace(b"o", b"0", 1))
print(b.split(b" "), b"a,b,,c".split(b","), b"  x y  ".split())
print(b.startswith(b"Hello"), b.endswith(b"World"), b.startswith(b"x"))
print(b.find(b"o"), b.find(b"z"), b.rfind(b"o"), b.count(b"l"), b.index(b"W"))
print(b"  trim  ".strip(), b"xxabcxx".strip(b"x") if False else b"  a  ".lstrip())
print(b"\x00\x01\xff".hex(), b"abc".hex())
print(b":".join([b"a", b"b", b"c"]), b"".join([b"x", b"y"]))
print(b"abc".decode(), b"h\xc3\xa9".decode())

# operators and indexing.
print(b"ab" + b"cd", b"xy" * 3, b"a" in b, 72 in b)
print(b[0], b[:5], b[::-1], len(b), list(b"AB"))
print(b == b"Hello World", b"abc" < b"abd")

# constructors.
print(bytes([72, 73]), bytes(3), bytes(b"copy"), bytes("hé", "utf-8"))


def show(thunk):
    try:
        thunk()
    except (ValueError, TypeError) as e:
        print(type(e).__name__ + ":", e)


show(lambda: b"abc".index(b"z"))

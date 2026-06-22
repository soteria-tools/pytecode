# 3.2.5.1 Immutable sequences (str, tuple): cannot be changed once created.
s = "abc"
t = (1, 2, 3)
try:
    s[0] = "x"
except TypeError as e:
    print("TypeError:", e)
try:
    t[0] = 9
except TypeError as e:
    print("TypeError:", e)
try:
    del t[0]
except TypeError as e:
    print("TypeError:", e)

# A string is a sequence of Unicode code points; each item is a length-1 string.
w = "héY"
print(len(w), [w[i] for i in range(len(w))])
print(len(w[0]), ord(w[1]), chr(0x68))

# Tuple construction: a singleton needs a trailing comma; () is empty.
print((), (5,), type((5,)) is tuple, type((5)) is int)
print(len(()), len((5,)), len((1, 2)))

# An immutable tuple may hold mutable objects whose value can still change.
box = ([],)
box[0].append(7)
print(box)

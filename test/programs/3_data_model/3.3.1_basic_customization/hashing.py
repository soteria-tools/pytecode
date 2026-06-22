# 3.3.1 __hash__: hash() returns an int; equal objects hash equally. Overriding
# __eq__ without __hash__ (or setting __hash__ = None) makes instances
# unhashable; a custom __hash__ result is reduced like an int hash.
print(hash(0), hash(1), hash(-1), hash(2), hash(-2), hash(100), hash(-100))
print(hash(True), hash(False), hash(1.0), hash(2.0), hash(-1.0), hash(0.0))
print(hash(10) == hash(10.0), hash(2**61 - 1), hash(2**61))


class A:
    def __eq__(self, o):
        return isinstance(o, A)


class B:
    def __eq__(self, o):
        return True

    def __hash__(self):
        return 42


class C:
    __hash__ = None


class D:
    def __hash__(self):
        return -1


def show(label, thunk):
    try:
        print(label, thunk())
    except TypeError as e:
        print(label, "TypeError:", e)


show("A", lambda: hash(A()))
show("B", lambda: hash(B()))
show("C", lambda: hash(C()))
show("D", lambda: hash(D()))
show("dict", lambda: {A(): 1})
show("set", lambda: {A()})

# A plain class (neither __eq__ nor __hash__ overridden) is hashable and can be a
# dict key / set element.
class Plain:
    pass


p = Plain()
d = {p: "v"}
print(d[p], p in {p})

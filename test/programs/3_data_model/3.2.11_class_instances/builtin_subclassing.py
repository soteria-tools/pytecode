# 3.2 The standard type hierarchy: user classes may subclass the built-in types.
# A subclass instance behaves like the underlying type for operators, methods,
# iteration, comparison and hashing, while also carrying its own attributes.


# --- list subclass ---
class L(list):
    pass


lst = L([1, 2, 3])
print(lst, len(lst), lst[0], lst[-1], lst[1:])
lst.append(4)
print(lst, sum(lst), 2 in lst, isinstance(lst, list))
lst[0] = 10
print(lst, sorted(lst, reverse=True), [x * 2 for x in lst])
print(L([3, 1, 2]) == [3, 1, 2])


class NamedList(list):
    def __init__(self, name, items):
        super().__init__(items)
        self.name = name


nl = NamedList("xs", [1, 2, 3])
print(nl, nl.name, len(nl))


# --- dict subclass with __missing__ ---
class D(dict):
    def __missing__(self, key):
        return f"default:{key}"


d = D(a=1, b=2)
print(d["a"], d["z"], len(d), sorted(d.keys()))
d["c"] = 3
print(sorted(d.items()), "a" in d, d.get("b"), d.get("zz", "none"))
del d["a"]
print(sorted(d), isinstance(d, dict))


# --- int / str / float subclasses ---
class MyInt(int):
    pass


class MyStr(str):
    pass


class MyFloat(float):
    pass


print(MyInt(5) + 1, MyInt(5) * 2, MyInt(5) < 10, int(MyInt(7)), MyInt(5) == 5)
print(MyStr("hi").upper(), len(MyStr("hi")), MyStr("ab") + "c", "b" in MyStr("abc"))
print(MyFloat(3.5) + 1.0, MyFloat(2.0) == 2.0)
print(f"{MyInt(42):05d}", f"{MyStr('hi'):>5}")


# --- hashing: subclass instances hash like their payload ---
keyed = {MyInt(5): "i", MyStr("k"): "s"}
print(keyed[5], keyed["k"])
print(hash(MyInt(5)) == hash(5), hash(MyStr("x")) == hash("x"))


# --- set / tuple subclasses ---
class MySet(set):
    pass


class MyTuple(tuple):
    pass


print(sorted(MySet([1, 2, 3, 2])), 2 in MySet([1, 2]))
print(MyTuple([1, 2, 3]), MyTuple([1, 2]) == (1, 2), len(MyTuple([1, 2, 3])))

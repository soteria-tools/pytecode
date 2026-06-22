# 3.2.7.1 Mapping types — dict: methods and the PEP 584 union operators.
d = {"a": 1, "b": 2, "c": 3}
print(d.get("a"), d.get("z"), d.get("z", 99))
print(list(d.keys()), list(d.values()), list(d.items()))
print(d.pop("a"), d.pop("z", -1), d)
d.setdefault("x", 10)
d.setdefault("b", 99)
print(sorted(d.items()))
d.update({"y": 5}, z=6)
print(sorted(d.items()))
d2 = d.copy()
d2["only2"] = 1
print("only2" in d, "only2" in d2)

# PEP 584 union operators.
print({"a": 1} | {"b": 2}, {"a": 1, "b": 1} | {"b": 2})
e = {"p": 1}
e |= {"q": 2}
print(e)

# popitem (LIFO), clear.
print(d.popitem())
d.clear()
print(d, len(d))


def show(thunk):
    try:
        thunk()
    except KeyError as ex:
        print("KeyError:", ex)


show(lambda: {}.popitem())

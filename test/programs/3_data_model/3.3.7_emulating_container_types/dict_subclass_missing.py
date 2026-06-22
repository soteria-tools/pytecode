# 3.3.7 __missing__: dict subclasses may define __missing__(self, key), which
# d[key] calls when the key is absent (it is NOT consulted by .get() or `in`).
class DefaultZero(dict):
    def __missing__(self, key):
        return 0


d = DefaultZero(a=5)
print(d["a"], d["missing"], d["other"])
print(d.get("a"), d.get("missing"), d.get("missing", -1))
print("a" in d, "missing" in d, len(d))


# __missing__ that records the access mutates the dict.
class Memo(dict):
    def __missing__(self, key):
        self[key] = key * 2
        return self[key]


m = Memo()
print(m[3], m[5], sorted(m.items()))


# A subclass without __missing__ raises KeyError as usual.
class Plain(dict):
    pass


def show(thunk):
    try:
        thunk()
    except KeyError as e:
        print("KeyError:", e)


show(lambda: Plain()["x"])

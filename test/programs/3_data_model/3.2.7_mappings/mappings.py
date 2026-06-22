# 3.2.7 Mappings: collections indexed by arbitrary keys; a[k] reads, assigns and
# (with del) removes the entry indexed by k; len() returns the number of items.
d = {"a": 1, "b": 2}
print(len(d), d["a"])
d["c"] = 3
print(d["c"], len(d))
d["a"] = 10
print(d["a"], len(d))
del d["a"]
print(sorted(d.items()), len(d))
try:
    d["missing"]
except KeyError as e:
    print("KeyError:", e)

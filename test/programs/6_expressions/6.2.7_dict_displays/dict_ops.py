d = {"a": 1, "b": 2}
d["c"] = 3
print(d)
print(d["a"], d.get("z"), d.get("z", 99))
print("a" in d, "z" in d)
print(sorted(d.keys()), sorted(d.values()))
print(d.pop("b"), d)
d.setdefault("x", []).append(1)
print(d)
d.update({"a": 10, "y": 20})
print(d)
del d["a"]
print(sorted(d.items()))
print(len(d))

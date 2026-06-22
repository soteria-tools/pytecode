# 6.2.7 Dictionary displays: duplicate keys keep the last value; ** unpacks a
# mapping (later values replace earlier ones); a dict comprehension inserts in
# production order.
print({"a": 1, "b": 2, "a": 3})
base = {"x": 1, "y": 2}
print({**base, "y": 20, "z": 3})
print({**{"a": 1}, **{"a": 2, "b": 3}})
print({k: k * k for k in range(4)})
print({v: k for k, v in {"p": 1, "q": 2}.items()})

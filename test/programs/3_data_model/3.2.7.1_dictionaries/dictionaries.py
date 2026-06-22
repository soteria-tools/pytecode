# 3.2.7.1 Dictionaries: keys are nearly arbitrary; numeric keys that compare
# equal index the same entry; insertion order is preserved; mutable (unhashable)
# keys are rejected.

# Numeric keys: 1, 1.0 and True all index the same entry.
d = {}
d[1] = "int"
print(d[1.0], d[True])
d[1.0] = "float"
print(d[1], len(d))

# Insertion order is preserved; replacing a key keeps its position, while
# removing then re-inserting moves it to the end.
order = {}
for k in ["z", "a", "m", "a"]:
    order[k] = len(order)
print(list(order))
order["z"] = 99
print(list(order))
del order["a"]
order["a"] = 1
print(list(order))

# Mutable types (list/dict/set) are unhashable and cannot be used as keys.
try:
    {}[[1, 2]] = 0
except TypeError as e:
    print("TypeError:", e)
try:
    {[1]: 0}
except TypeError as e:
    print("TypeError:", e)

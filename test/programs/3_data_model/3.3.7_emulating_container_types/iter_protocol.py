it = iter([1, 2, 3])
print(next(it))
print(next(it))
print(next(it))
print(next(it, "default"))
it2 = iter("ab")
print(next(it2), next(it2))
try:
    next(it2)
except StopIteration:
    print("StopIteration")

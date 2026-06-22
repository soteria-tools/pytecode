# 6.2.6 Set displays: {} builds an empty dict (not a set); a set drops duplicate
# elements; an empty set is built with set().
print(type({}).__name__)
print(type({1, 2}).__name__)
print(sorted({1, 2, 2, 3, 3, 3}))
print(sorted({x % 3 for x in range(10)}))
print(type(set()).__name__, len(set()))

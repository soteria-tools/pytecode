# 3.2.6 Set types: the named set-algebra methods (accepting any iterable) and
# mutators, alongside the operators.
s = {1, 2, 3}
print(sorted(s.union([3, 4])), sorted(s.intersection({2, 3, 5})))
print(sorted(s.difference([1])), sorted(s.symmetric_difference({2, 4})))
print(s.issubset({1, 2, 3, 4}), s.issuperset({1, 2}), s.isdisjoint({4, 5}))
print({1, 2}.issubset([1, 2, 3]), {1, 2}.isdisjoint([3, 4]))

m = {1, 2, 3}
m.add(10)
m.discard(1)
m.remove(2)
m.update([20, 3, 30])
print(sorted(m))
m.clear()
print(m, len(m))

popped = {42}.pop()
print(popped)

# operators still work alongside.
print(sorted({1, 2, 3} | {3, 4}), sorted({1, 2, 3} & {2, 3, 4}))
print({1, 2} <= {1, 2, 3}, {1, 2, 3} < {1, 2, 3})


def show(thunk):
    try:
        thunk()
    except KeyError as e:
        print("KeyError:", e)


show(lambda: set().pop())

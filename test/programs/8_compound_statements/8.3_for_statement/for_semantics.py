# 8.3 The for statement: the target is reassigned each iteration (overwriting any
# assignment in the body), persists after the loop, and is left untouched if the
# iterable is empty.
out = []
for i in range(4):
    out.append(i)
    i = 99
print(out, i)

for a, b in [(1, 2), (3, 4)]:
    print(a, b)
for first, *rest in [[1, 2, 3], [4, 5]]:
    print(first, rest)

for x in range(3):
    pass
else:
    print("completed")
for x in range(3):
    if x == 1:
        break
else:
    print("not printed")
print("after")

z = "before"
for z in []:
    pass
print(z)

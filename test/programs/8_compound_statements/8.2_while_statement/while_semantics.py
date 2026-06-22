# 8.2 The while statement: repeats while the test is true; the else clause runs
# when the test becomes false, but not after a break; continue returns to the test.
n = 0
while n < 3:
    print("loop", n)
    n += 1
else:
    print("else", n)

m = 0
while True:
    if m == 2:
        break
    m += 1
else:
    print("not printed")
print("m", m)

total = 0
i = 0
while i < 5:
    i += 1
    if i % 2 == 0:
        continue
    total += i
print(total)

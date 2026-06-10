n = 3
while n > 0:
    n -= 1
else:
    print("exhausted", n)
n = 5
while n > 0:
    n -= 1
    if n == 2:
        break
else:
    print("not printed")
print("end", n)

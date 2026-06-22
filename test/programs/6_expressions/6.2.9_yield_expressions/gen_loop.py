def squares(n):
    for i in range(n):
        yield i * i
print(list(squares(5)))
for v in squares(3):
    print(v)
total = 0
for v in squares(10):
    if v > 25:
        break
    total += v
print(total)

x = 5
print("big" if x > 3 else "small")
print("big" if x > 7 else "small")
y = (x if x else 99) + 1
print(y)

i = 0
while True:
    i += 1
    if i % 2 == 0:
        continue
    if i > 7:
        break
    print(i)
print("done", i)

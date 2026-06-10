a = [1, 2, 3]
b = a
b.append(4)
print(a)
c = a[:]
c.append(5)
print(a, c)
d = {"k": [1]}
e = dict(d)
e["k"].append(2)
print(d)
m = [[0] * 2] * 3
m[0][0] = 9
print(m)

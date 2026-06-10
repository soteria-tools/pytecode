def counter(start):
    count = start
    def bump(step=1):
        nonlocal count
        count += step
        return count
    return bump
c1 = counter(0)
c2 = counter(100)
print(c1(), c1(), c1(5))
print(c2())
print(c1())

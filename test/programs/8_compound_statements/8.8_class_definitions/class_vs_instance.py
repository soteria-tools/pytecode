class Counter:
    total = 0
    def __init__(self):
        Counter.total += 1
        self.mine = Counter.total
c1 = Counter()
c2 = Counter()
print(Counter.total, c1.mine, c2.mine)
print(c1.total, c2.total)
c1.total = 99
print(c1.total, c2.total, Counter.total)

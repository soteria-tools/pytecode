class Ring:
    def __init__(self, items):
        self.items = items
    def __len__(self):
        return len(self.items)
    def __getitem__(self, i):
        return self.items[i % len(self.items)]
    def __setitem__(self, i, v):
        self.items[i % len(self.items)] = v
r = Ring([10, 20, 30])
print(len(r))
print(r[0], r[4], r[-1])
r[7] = 99
print(r.items)
print(bool(Ring([])), bool(r))

# 3.3.7 Emulating container types: __len__, __getitem__, __setitem__,
# __delitem__, __iter__, __reversed__, __contains__.
class Seq:
    def __init__(self, data):
        self.data = list(data)

    def __len__(self):
        return len(self.data)

    def __getitem__(self, key):
        return self.data[key]

    def __setitem__(self, key, value):
        self.data[key] = value

    def __delitem__(self, key):
        del self.data[key]

    def __iter__(self):
        return iter(self.data)

    def __reversed__(self):
        return iter(self.data[::-1])

    def __contains__(self, item):
        return item in self.data

    def __repr__(self):
        return f"Seq({self.data})"


s = Seq([1, 2, 3, 4])
print(len(s), s[0], s[3])
s[1] = 20
del s[0]
print(s)
print(list(s))
print(list(reversed(s)))
print(3 in s, 99 in s)
print(bool(Seq([])), bool(Seq([5])))


# reversed() falls back to __len__ + __getitem__ when __reversed__ is absent.
class Range3:
    def __len__(self):
        return 3

    def __getitem__(self, i):
        return i * 10


print(list(reversed(Range3())))


# Membership without __contains__ falls back to iteration.
class IterOnly:
    def __iter__(self):
        return iter([10, 20, 30])


print(20 in IterOnly(), 99 in IterOnly())

# reversed() on builtin sequences.
print(list(reversed([1, 2, 3])), list(reversed("abc")), list(reversed(range(4))))

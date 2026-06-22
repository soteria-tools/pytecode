# 3.3.8 Numeric conversion methods: __int__/__float__ back int()/float();
# __index__ provides lossless integer conversion (used for indexing) and is the
# fallback for int() when __int__ is absent; __round__ backs round().
class F:
    def __int__(self):
        return 9

    def __float__(self):
        return 2.5


print(int(F()), float(F()))


class Idx:
    def __index__(self):
        return 3


print(int(Idx()))
print([0, 10, 20, 30, 40][Idx()])
print("abcdef"[Idx()])


class Rnd:
    def __round__(self, n=None):
        return ("round", n)


print(round(Rnd()), round(Rnd(), 2))

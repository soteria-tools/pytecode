# 3.2.13 Internal types — slice objects: represent extended-slice syntax and can
# also be constructed directly; they expose read-only start/stop/step.
s = slice(1, 10, 2)
print(s.start, s.stop, s.step)
print(slice(5).start, slice(5).stop, slice(5).step)
print(repr(slice(1, 10, 2)), repr(slice(5)))
print(slice(1, 5) == slice(1, 5), slice(1, 5) == slice(1, 6))


# A custom container receives a slice object for slice subscripts.
class Seq:
    def __getitem__(self, key):
        return key


print(Seq()[1:10:2])
print(Seq()[::-1])
print(Seq()[7])

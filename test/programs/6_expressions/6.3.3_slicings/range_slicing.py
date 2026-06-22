# 6.3.3 Slicings on range objects: indexing yields the element start + i*step
# (with negative-index support); slicing yields a new range.
r = range(10)
print(r[3], r[-1], r[0])
print(r[2:8:2], r[::-1], r[::2], r[-3:])
print(range(2, 20, 3)[2], range(2, 20, 3)[-1])
print(range(10)[slice(None, None, 3)])
print(range(0, 100, 5)[2:6])
print(list(range(10)[1:9:3]))
print(range(5)[10:], range(5)[2:1])  # empty results


def show(thunk):
    try:
        thunk()
    except (ValueError, IndexError, TypeError) as e:
        print(type(e).__name__ + ":", e)


show(lambda: range(5)[100])
show(lambda: range(5)[::0])
show(lambda: range(5)["x"])

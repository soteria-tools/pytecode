# 6.3.2 Subscriptions: sequences take int/slice keys (negative indices add the
# length); mappings take key objects; a comma-separated subscript is a tuple key.
xs = [10, 20, 30, 40]
print(xs[0], xs[-1], xs[2])
print("hello"[1], "hello"[-1])
print((1, 2, 3)[1])

grid = {(0, 0): "origin", (1, 2): "a"}
print(grid[0, 0], grid[1, 2])


def show(thunk):
    try:
        thunk()
    except (IndexError, KeyError, TypeError) as e:
        print(type(e).__name__ + ":", e)


show(lambda: xs[10])
show(lambda: {"a": 1}["b"])
show(lambda: xs["x"])
show(lambda: (1, 2)["y"])
show(lambda: "abc"[1.0])

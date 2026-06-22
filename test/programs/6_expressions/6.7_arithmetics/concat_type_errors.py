def show(f):
    try:
        f()
    except TypeError as e:
        print("TypeError:", e)


show(lambda: 1 + "x")
show(lambda: "x" + 1)
show(lambda: [1] + (2,))
show(lambda: (1,) + [2])
show(lambda: None + 1)
show(lambda: "a" - "b")
show(lambda: [1] - [2])
show(lambda: 1 - "x")

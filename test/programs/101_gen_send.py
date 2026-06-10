def echo():
    received = yield "ready"
    while received != "stop":
        received = yield f"got:{received}"
    yield "bye"
g = echo()
print(next(g))
print(g.send("a"))
print(g.send("b"))
print(g.send("stop"))

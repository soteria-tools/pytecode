def inner():
    yield 1
    yield 2
    return "inner done"
def outer():
    result = yield from inner()
    print("result:", result)
    yield from [10, 20]
    yield from "ab"
print(list(outer()))

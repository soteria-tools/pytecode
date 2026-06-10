def m(x):
    match x:
        case {"op": "add", "a": a, "b": b}:
            return a + b
        case {"op": op, **rest}:
            return f"{op}:{sorted(rest)}"
        case _:
            return "no match"
print(m({"op": "add", "a": 2, "b": 3}))
print(m({"op": "neg", "x": 1, "y": 2}))
print(m([1, 2]))

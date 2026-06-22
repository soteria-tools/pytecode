# 8.6 The match statement — OR / guard / AS / capture / wildcard / value /
# mapping-with-**rest / starred-sequence patterns.
def describe(x):
    match x:
        case 0 | 1 | 2:
            return "small"
        case [a, b] if a == b:
            return f"pair-equal:{a}"
        case [a, b]:
            return f"pair:{a},{b}"
        case str() as s:
            return f"str:{s}"
        case {"type": t, **rest}:
            return f"map:{t}:{sorted(rest)}"
        case (first, *others):
            return f"seq:{first}:{others}"
        case _:
            return "other"


print(describe(1))
print(describe([3, 3]))
print(describe([3, 4]))
print(describe("hi"))
print(describe({"type": "a", "x": 1, "y": 2}))
print(describe((9, 8, 7)))
print(describe(3.14))


# A value pattern matches against a dotted (attribute) name.
class Color:
    RED = "red"
    GREEN = "green"


def name(c):
    match c:
        case Color.RED:
            return "is-red"
        case Color.GREEN:
            return "is-green"
        case _:
            return "unknown"


print(name("red"), name("green"), name("blue"))

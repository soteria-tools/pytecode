def m(x):
    match x:
        case []:
            return "empty"
        case [a]:
            return f"one:{a}"
        case [a, b]:
            return f"two:{a},{b}"
        case [first, *rest]:
            return f"many:{first}+{len(rest)}"
for v in [[], [1], [1, 2], [1, 2, 3, 4]]:
    print(m(v))

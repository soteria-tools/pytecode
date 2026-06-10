def m(x):
    match x:
        case 0:
            return "zero"
        case 1 | 2:
            return "one-or-two"
        case "hi":
            return "greeting"
        case None:
            return "none"
        case _:
            return "other"
for v in [0, 1, 2, "hi", None, 9.5]:
    print(m(v))

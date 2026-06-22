# 6.16 Evaluation order: expressions evaluate left to right; in an assignment the
# right-hand side is evaluated before the (left-hand) target.
order = []


def t(label):
    order.append(label)
    return label


def n(label, v):
    order.append(label)
    return v


order.clear()
_ = (t("a"), t("b"), t("c"))
print(order)

order.clear()
_ = n("a", 1) + n("b", 2) * (n("c", 3) - n("d", 4))
print(order)

order.clear()


def f(*args, **kwargs):
    return None


f(t("arg1"), t("arg2"), kw=t("kw3"))
print(order)

order.clear()
box = {}
box[t("target")] = t("value")
print(order)

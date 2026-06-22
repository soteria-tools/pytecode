def t(x):
    print("eval", x)
    return x
print(t(0) and t(1))
print(t(2) or t(3))
print(t("") or t("fallback"))
print(None or 5, 0 and 7)

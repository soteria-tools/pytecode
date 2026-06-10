def classify(n):
    if n < 0:
        return "neg"
    elif n == 0:
        return "zero"
    elif n < 10:
        return "small"
    else:
        return "big"
for v in [-5, 0, 3, 100]:
    print(classify(v))

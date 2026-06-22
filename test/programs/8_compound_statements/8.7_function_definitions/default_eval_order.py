def make_default():
    print("default computed")
    return 42
def f(x=make_default()):
    return x
print("defined")
print(f())
print(f(1))

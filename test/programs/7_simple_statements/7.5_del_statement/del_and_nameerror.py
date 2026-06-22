x = 1
del x
try:
    print(x)
except NameError as e:
    print("NameError")

def f():
    y = 2
    del y
    try:
        return y
    except UnboundLocalError:
        return "UnboundLocalError"
print(f())

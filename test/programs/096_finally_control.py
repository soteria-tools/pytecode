def f():
    try:
        return "try"
    finally:
        print("finally f")
print(f())
def g():
    try:
        return "try"
    finally:
        return "finally"
print(g())
def h():
    for i in range(3):
        try:
            if i == 1:
                continue
            if i == 2:
                break
        finally:
            print("cleanup", i)
    return "done"
print(h())

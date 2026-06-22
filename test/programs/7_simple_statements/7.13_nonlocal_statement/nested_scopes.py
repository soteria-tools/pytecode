x = "global"
def outer():
    x = "outer"
    def middle():
        x = "middle"
        def inner():
            nonlocal x
            x = "set-by-inner"
            return x
        inner()
        return x
    return middle(), x
print(outer())
print(x)
def reads_global():
    return x
print(reads_global())

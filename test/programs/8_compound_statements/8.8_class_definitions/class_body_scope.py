x = "module"
class C:
    x = "class"
    y = x + "!"
    def get(self):
        return x
print(C.x, C.y)
print(C().get())
class D:
    vals = [1, 2, 3]
    doubled = [v * 2 for v in vals]
print(D.doubled)

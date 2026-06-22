# 7.4 The pass statement: a null operation, used as a syntactic placeholder.
def f(arg):
    pass


class C:
    pass


for i in range(3):
    pass

print(f(1), C.__name__, "done")

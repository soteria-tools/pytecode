# 3.2.1 None: a type with a single value, accessed through the built-in name
# None; its truth value is false; returned by functions that don't explicitly
# return a value.
print(None)
print(repr(None))
print(type(None).__name__)
print(bool(None))
print(None is None)


def no_return():
    pass


def bare_return():
    return


print(no_return(), bare_return())
print(no_return() is None, bare_return() is None)
print("yes" if None else "no")
print(None == None, None == 0, None == False)

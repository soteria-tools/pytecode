# 6.10.3 Identity comparisons: `is`/`is not` test whether two references are the
# same object.
print(None is None, None is not None)
a = []
b = a
c = []
print(a is b, a is c, a is not c)
print([] is [], {} is {})
x = object()
print(x is x)
print(... is ..., NotImplemented is NotImplemented)

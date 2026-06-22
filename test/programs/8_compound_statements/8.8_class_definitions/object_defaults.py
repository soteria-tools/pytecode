class Empty:
    pass
e = Empty()
e.a = 1
print(e.a)
print(Empty.__name__, type(e).__name__)
print(isinstance(e, object), issubclass(Empty, object))
print(object() is not None)
class WithEq:
    pass
w1, w2 = WithEq(), WithEq()
print(w1 == w1, w1 == w2, w1 != w2)

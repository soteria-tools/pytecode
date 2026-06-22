# 8.7 Function definitions: decorators are evaluated at definition time and
# applied in nested (bottom-up) fashion; positional-only (/) and keyword-only (*)
# parameters constrain how arguments may be passed.
trace = []


def deco(tag):
    trace.append(("make", tag))

    def wrap(f):
        trace.append(("apply", tag))

        def inner(*a, **k):
            return f(*a, **k) + f"<{tag}>"

        return inner

    return wrap


@deco("outer")
@deco("inner")
def greet():
    return "hi"


print(trace)
print(greet())


def f(a, b, /, c, *, d):
    return (a, b, c, d)


print(f(1, 2, 3, d=4))
print(f(1, 2, c=3, d=4))


def show(thunk):
    try:
        thunk()
    except TypeError as e:
        print("TypeError:", e)


show(lambda: f(1, 2, 3, 4))
show(lambda: f(a=1, b=2, c=3, d=4))

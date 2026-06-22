# 8.8 Class definitions: classes can be decorated (Foo = deco(Foo)); a class with
# no bases inherits object; the body executes as a suite in a new namespace whose
# definition order is preserved in __dict__.
def add_attr(cls):
    cls.decorated = True
    return cls


@add_attr
class Foo:
    pass


print(Foo.decorated, Foo.__bases__ == (object,))

log = []


def tag(name):
    def deco(cls):
        log.append(name)
        cls.tags = getattr(cls, "tags", ()) + (name,)
        return cls

    return deco


@tag("a")
@tag("b")
class Bar:
    pass


print(log, Bar.tags)


class Body:
    print("executing body")
    x = 1
    y = x + 1

    def method(self):
        return self.x


print(Body.x, Body.y, Body().method())
print([k for k in Body.__dict__ if not k.startswith("__")])


class Shared:
    val = "class"


s = Shared()
print(s.val)
s.val = "instance"
print(s.val, Shared.val)

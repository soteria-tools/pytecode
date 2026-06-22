def make_method(self):
    return f"I am {self.name}"
Dynamic = type("Dynamic", (), {"greet": make_method, "kind": "made-by-type"})
d = Dynamic()
d.name = "dyn"
print(Dynamic.__name__, d.kind)
print(d.greet())
Sub = type("Sub", (Dynamic,), {})
s = Sub()
s.name = "sub"
print(s.greet(), isinstance(s, Dynamic))
print([c.__name__ for c in Sub.__mro__])

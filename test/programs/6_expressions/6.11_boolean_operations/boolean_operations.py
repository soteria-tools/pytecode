# 6.11 Boolean operations: `and`/`or` return the last evaluated operand (not a
# coerced bool) and short-circuit; `not` always yields a bool.
def crash():
    raise Exception("should not be called")


print(1 and 2, 0 and 2, 1 or 2, 0 or 2)
print("" or "foo", "bar" or "foo")
print([] or [1], [2] and [3])
print(None or 0 or "" or "last")
print(3 and 0 and crash())
print(not 0, not 1, not "", not "x", not [], not None)
print(not "foo")
print(type(1 and 2).__name__, type(not 0).__name__)

# Falsy values: False, None, numeric zero, empty str/containers.
print(bool(0), bool(0.0), bool(""), bool([]), bool({}), bool(set()), bool(()))
print(bool(1), bool(-1), bool("x"), bool([0]), bool(0j), bool(1j))

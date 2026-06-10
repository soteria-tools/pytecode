ops = {
    "add": lambda a, b: a + b,
    "mul": lambda a, b: a * b,
}
for name in sorted(ops):
    print(name, ops[name](3, 4))
def apply_all(fns, x):
    return [f(x) for f in fns]
print(apply_all([abs, str, float], -3))

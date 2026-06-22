# 6.10.1 A rich comparison may return any value; that value is produced as-is in
# a value context, and bool() is only applied in a boolean context.
class Box:
    def __init__(self, v):
        self.v = v

    def __eq__(self, o):
        return f"eq({self.v})"

    def __lt__(self, o):
        return f"lt({self.v})"

    def __ne__(self, o):
        return [self.v]


a, b = Box(1), Box(2)
print(a == b)
print(a < b)
print(a != b)

# In a boolean context the result is coerced via bool().
print("yes" if a == b else "no")
print(bool(a != b))

# Builtin comparisons still yield real bools.
print(1 < 2, type(1 == 1).__name__)

# The raw value can be captured.
r = a == b
print(r, type(r).__name__)

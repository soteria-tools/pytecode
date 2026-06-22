# 6.10.2 Membership test operations: for containers, x in y is
# any(x is e or x == e for e in y); for strings it is a substring test, with the
# empty string a substring of everything.
print(2 in [1, 2, 3], 9 in [1, 2, 3], 2 not in [1, 2, 3])
print("b" in {"a": 1, "b": 2}, "z" in {"a": 1})
print(3 in (1, 2, 3), 3 in {1, 2, 3})
print("ell" in "hello", "" in "hello", "x" in "hello", "H" in "hello")


# The identity check (x is e) short-circuits the equality check, so an object is
# a member of a container that holds it even if its __eq__ says otherwise.
class Never:
    def __eq__(self, other):
        return False


n = Never()
print(n in [n], n in [Never()])

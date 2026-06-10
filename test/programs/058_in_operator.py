print(2 in [1, 2, 3], 9 in [1, 2, 3])
print("ell" in "hello", "z" not in "hello")
print(2 in (1, 2), "a" in {"a": 1})
print(1 in {1, 2})
class Box:
    def __contains__(self, item):
        return item == "magic"
print("magic" in Box(), "other" in Box())

class Adder:
    def __init__(self, n):
        self.n = n
    def __call__(self, x):
        return x + self.n
add5 = Adder(5)
print(add5(10))
print(list(map(add5, [1, 2, 3])))
print(callable(add5), callable(42), callable(len))

class CM:
    def __init__(self, name, swallow=False):
        self.name = name
        self.swallow = swallow
    def __enter__(self):
        print("enter", self.name)
        return self.name
    def __exit__(self, exc_type, exc, tb):
        print("exit", self.name, exc_type.__name__ if exc_type else None)
        return self.swallow
with CM("a") as x:
    print("body", x)
with CM("b", swallow=True):
    raise ValueError("boom")
print("survived")
with CM("c"), CM("d"):
    print("nested body")

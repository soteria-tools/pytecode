def repeat(n):
    def deco(f):
        def wrapped(*args):
            results = []
            for _ in range(n):
                results.append(f(*args))
            return results
        return wrapped
    return deco

@repeat(3)
def hello(name):
    return "hi " + name
print(hello("bob"))

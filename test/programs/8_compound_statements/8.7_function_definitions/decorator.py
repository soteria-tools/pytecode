def trace(f):
    def wrapped(*args):
        print("calling", f.__name__)
        result = f(*args)
        print("got", result)
        return result
    return wrapped

@trace
def double(x):
    return x * 2
print(double(21))

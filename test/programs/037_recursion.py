def fib(n):
    return n if n < 2 else fib(n - 1) + fib(n - 2)
print([fib(i) for i in range(10)])
def fact(n):
    if n == 0:
        return 1
    return n * fact(n - 1)
print(fact(20))

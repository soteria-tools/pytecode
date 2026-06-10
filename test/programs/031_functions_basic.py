def add(a, b):
    return a + b
def greet(name, greeting="hello"):
    return greeting + ", " + name
print(add(2, 3))
print(greet("world"))
print(greet("there", "hi"))
print(greet(greeting="yo", name="x"))

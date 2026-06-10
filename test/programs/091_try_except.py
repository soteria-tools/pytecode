def safe_div(a, b):
    try:
        return a / b
    except ZeroDivisionError:
        return "div by zero"
print(safe_div(10, 2))
print(safe_div(1, 0))
try:
    xs = [1]
    print(xs[5])
except IndexError as e:
    print("IndexError:", e)
try:
    {}["k"]
except KeyError as e:
    print("KeyError:", e)

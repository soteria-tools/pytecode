for exc in [ValueError("v"), KeyError("k"), ZeroDivisionError("z")]:
    try:
        raise exc
    except LookupError as e:
        print("lookup", type(e).__name__)
    except ArithmeticError as e:
        print("arith", type(e).__name__)
    except Exception as e:
        print("generic", type(e).__name__)
try:
    raise TypeError("a", "b")
except TypeError as e:
    print(e.args, str(e))

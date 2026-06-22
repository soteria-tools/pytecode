def run(n):
    try:
        x = 10 // n
    except ZeroDivisionError:
        print("handler")
    else:
        print("else", x)
    finally:
        print("finally")
run(2)
run(0)

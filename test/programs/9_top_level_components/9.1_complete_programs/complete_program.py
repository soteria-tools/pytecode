# 9.1 Complete Python programs: a script is executed in the __main__ module's
# namespace, so __name__ == "__main__"; a program is a sequence of statements run
# top to bottom, with blank lines permitted between them (9.2 file input).
print(__name__)

x = 1

y = 2


print(x + y)

if __name__ == "__main__":
    print("running as main")

order = []
order.append("first")
order.append("second")
print(order)

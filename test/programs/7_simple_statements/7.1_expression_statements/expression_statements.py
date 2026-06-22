# 7.1 Expression statements: in (non-interactive) script mode an expression
# statement is evaluated for its side effects but its value is not printed.
42
"a string"
[x for x in range(3)]
1 + 1
print("explicit")


def f():
    print("called")
    return 99


f()

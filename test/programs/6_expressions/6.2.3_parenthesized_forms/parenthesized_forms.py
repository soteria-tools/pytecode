# 6.2.3 Parenthesized forms: a tuple is formed by commas, not parentheses; ()
# is the empty tuple; (expr) without a comma yields just the expression.
print(type((5)).__name__, type((5,)).__name__)
print((), type(()).__name__)
print((1, 2, 3))
x = 5,
print(x, type(x).__name__)
print((((7))))
print((1, (2, 3), 4))
a = 1, 2, 3
print(a, type(a).__name__)

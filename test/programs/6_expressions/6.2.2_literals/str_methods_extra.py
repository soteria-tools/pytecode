# Additional str methods: split with maxsplit, rfind/rindex, removeprefix/suffix,
# the is* predicates, translate, expandtabs, casefold, encode.
print("a-b-c-d".split("-", 2))
print("  x  y  z  ".split(None, 1))
print("a,b,,c".split(","), "x y z".split())
print("hello".rfind("l"), "hello".rindex("l"), "hello".rfind("z"))
print("abcabc".removeprefix("abc"), "abcabc".removesuffix("abc"))
print("abcabc".removeprefix("xy"), "test".removesuffix("xy"))
print("Hello World".istitle(), "hello".istitle(), "HELLO".istitle())
print("   ".isspace(), "a b".isspace(), "abc123".isalnum(), "abc!".isalnum())
print("123".isnumeric(), "12a".isnumeric(), "var_1".isidentifier(), "1var".isidentifier())
print("ABC".casefold())
print("a\tb\tc".expandtabs(4))
print("a\tbc\td".expandtabs())
print("hello".translate({ord("l"): "L", ord("o"): None}))
print("café".encode(), "café".encode("utf-8"), "abc".encode("ascii"))


def show(thunk):
    try:
        thunk()
    except ValueError as e:
        print("ValueError:", e)


show(lambda: "abc".rindex("z"))

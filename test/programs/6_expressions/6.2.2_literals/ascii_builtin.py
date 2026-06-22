# ascii(obj) returns repr(obj) with every non-ASCII codepoint escaped as
# \xXX (<=0xff), \uXXXX (<=0xffff) or \UXXXXXXXX, leaving ASCII text unchanged.
print(ascii("café"))
print(ascii("a\tb"))
print(ascii("héllo™ 𝕏"))
print(ascii([1, "ü", "ok"]))
print(ascii({"k": "naïve"}))
print(ascii("plain ascii"))
print(ascii(123), ascii(None))

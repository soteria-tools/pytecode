try:
    try:
        int("nope")
    except ValueError as e:
        raise RuntimeError("conversion failed") from e
except RuntimeError as e:
    print(e)
    print(type(e.__cause__).__name__)
try:
    assert 1 + 1 == 3, "math is broken"
except AssertionError as e:
    print("AssertionError:", e)
assert True
print("end")

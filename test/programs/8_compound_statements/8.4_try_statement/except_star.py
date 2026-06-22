# 8.4 The try statement — except* (PEP 654) and exception groups. An
# ExceptionGroup bundles several exceptions; `except*` splits the group, runs
# each clause on the matching subgroup, and re-raises whatever is left over.
eg = ExceptionGroup("top", [ValueError(1), TypeError(2), ValueError(3)])
print(eg)
print(repr(eg))
print(eg.message, eg.exceptions, eg.args)
print(isinstance(eg, Exception), isinstance(eg, BaseException))
print(type(eg).__name__, isinstance(eg, BaseExceptionGroup))

# split / subgroup partition by an exception type, preserving the message.
m, r = eg.split(ValueError)
print(repr(m), repr(r))
print(eg.subgroup(KeyError))
print(repr(eg.subgroup(ValueError)))

# nested groups keep their structure.
nested = ExceptionGroup("outer", [ValueError(1), ExceptionGroup("inner", [TypeError(9)])])
nm, nr = nested.split(TypeError)
print(repr(nm))
print(repr(nr))

# Plain exceptions now repr like CPython.
print(repr(ValueError(1)), repr(ValueError("x", "y")), repr(ValueError()))


def run(label, f):
    try:
        f()
        print(label, "completed normally")
    except BaseException as e:
        kids = getattr(e, "exceptions", None)
        print(label, type(e).__name__, "|", e, "|", kids and [repr(x) for x in kids])


# Two clauses catch the whole group.
def two_clauses():
    try:
        raise ExceptionGroup("eg", [ValueError(1), TypeError(2)])
    except* ValueError as e:
        print("  caught V:", e.exceptions)
    except* TypeError as e:
        print("  caught T:", e.exceptions)


run("two_clauses", two_clauses)


# Unmatched sub-exceptions are re-raised as a group.
def leftover():
    try:
        raise ExceptionGroup("g", [ValueError(1), TypeError(2), KeyError(3)])
    except* ValueError:
        pass


run("leftover", leftover)


# A handler that raises combines with the leftover into a new group.
def handler_raises():
    try:
        raise ExceptionGroup("g", [ValueError(1), TypeError(2)])
    except* ValueError:
        raise RuntimeError("boom")


run("handler_raises", handler_raises)


# If nothing is left over, a handler's exception propagates on its own.
def only_raise():
    try:
        raise ExceptionGroup("g", [ValueError(1)])
    except* ValueError:
        raise RuntimeError("x")


run("only_raise", only_raise)


# A bare (non-group) exception caught by except* is wrapped in a group.
def wrap_bare():
    try:
        raise ValueError("solo")
    except* ValueError as e:
        print("  wrapped:", type(e).__name__, repr(e.message), e.exceptions)


run("wrap_bare", wrap_bare)


# Construction validation.
def show(thunk):
    try:
        thunk()
    except (TypeError, ValueError) as e:
        print(type(e).__name__ + ":", e)


show(lambda: ExceptionGroup("m", []))
show(lambda: ExceptionGroup("m", [1]))
show(lambda: ExceptionGroup(5, [ValueError()]))

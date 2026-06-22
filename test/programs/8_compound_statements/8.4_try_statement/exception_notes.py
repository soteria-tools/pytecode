# Exception introspection: .args / str / repr, add_note()/__notes__, and
# with_traceback() returning self.
e = ValueError("msg")
print(e.args, str(e), repr(e))
print(ValueError("a", "b").args, ValueError().args)

# add_note appends to __notes__ (created on first call), in order.
e = ValueError("base")
e.add_note("first note")
e.add_note("second note")
print(e.__notes__)


def show(thunk):
    try:
        thunk()
    except TypeError as ex:
        print("TypeError:", ex)


show(lambda: ValueError("x").add_note(123))

# with_traceback(tb) sets __traceback__ and returns the exception itself.
e = KeyError("k")
print(e.with_traceback(None) is e)
print(e.__traceback__)


# __notes__ does not exist until add_note is called.
def check_notes():
    try:
        RuntimeError("r").__notes__
    except AttributeError:
        print("no __notes__ yet")


check_notes()

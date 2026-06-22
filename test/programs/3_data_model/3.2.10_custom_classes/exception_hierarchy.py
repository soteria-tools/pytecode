# Built-in exception hierarchy (3.13): the standard exception classes exist with
# the documented inheritance relationships, so `except Base` catches subclasses.
print(issubclass(IndexError, LookupError), issubclass(KeyError, LookupError))
print(issubclass(ZeroDivisionError, ArithmeticError))
print(issubclass(OverflowError, ArithmeticError), issubclass(FloatingPointError, ArithmeticError))
print(issubclass(ModuleNotFoundError, ImportError))
print(issubclass(FileNotFoundError, OSError), issubclass(PermissionError, OSError))
print(issubclass(BrokenPipeError, ConnectionError), issubclass(ConnectionError, OSError))
print(issubclass(NotImplementedError, RuntimeError), issubclass(RecursionError, RuntimeError))
print(issubclass(UnboundLocalError, NameError))
print(issubclass(UnicodeDecodeError, UnicodeError), issubclass(UnicodeError, ValueError))
print(issubclass(TabError, IndentationError), issubclass(IndentationError, SyntaxError))
print(issubclass(DeprecationWarning, Warning), issubclass(Warning, Exception))
print(issubclass(KeyboardInterrupt, BaseException), issubclass(KeyboardInterrupt, Exception))
print(issubclass(SystemExit, BaseException), issubclass(GeneratorExit, BaseException))

# Every Exception subclass is also a BaseException.
for cls in (ValueError, OSError, RecursionError, UnicodeError, MemoryError):
    print(cls.__name__, issubclass(cls, Exception), issubclass(cls, BaseException))


# Catching via a base class.
def show(thunk, base):
    try:
        thunk()
    except base as e:
        print(type(e).__name__, "caught as", base.__name__)


show(lambda: [][9], LookupError)
show(lambda: {}["x"], LookupError)
show(lambda: 1 / 0, ArithmeticError)
show(lambda: undefined_name, Exception)  # noqa: F821

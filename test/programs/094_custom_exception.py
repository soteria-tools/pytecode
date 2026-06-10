class AppError(Exception):
    pass
class NotFound(AppError):
    def __init__(self, what):
        super().__init__(f"missing: {what}")
        self.what = what
try:
    raise NotFound("config")
except AppError as e:
    print(type(e).__name__, e, e.what)
try:
    raise AppError("plain")
except NotFound:
    print("not reached")
except AppError as e:
    print("caught", e)

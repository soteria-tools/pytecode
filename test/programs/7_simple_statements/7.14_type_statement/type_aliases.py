# 7.14 The type statement (PEP 695): `type X = ...` creates a
# typing.TypeAliasType. Its __name__ is the alias name, its __value__ is the
# (lazily-evaluated) right-hand side, and repr shows the bare name.
type Alias = list[int]
print(Alias)
print(type(Alias).__name__)
print(Alias.__name__)
print(Alias.__value__)
print(Alias.__type_params__)

type Number = int | float
print(Number, Number.__value__)

type Plain = int
print(Plain.__value__)

type Nested = dict[str, list[int]]
print(Nested.__value__)


# The value is evaluated lazily: defining an alias over an undefined name does
# not raise; only accessing __value__ does.
type Late = does_not_exist
print("defined Late")
try:
    Late.__value__
except NameError as e:
    print("NameError:", e)

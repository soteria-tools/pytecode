# Interpreter test programs

Each `.py` here is a differential test: `dune build @interp` runs it under both
`python3.13` and the pytecode interpreter and asserts the stdout matches byte
for byte. The runner (`test/interp_runner.exe`) walks this tree recursively, so
new tests are picked up automatically wherever you drop them.

## Organisation

Programs are filed under the section of the [Python Language Reference][ref]
they exercise. The folder name is `<section-number>_<short-name>`, nested only
one level deep under `programs/` (e.g. `6_expressions/6.7_arithmetics/`).

[ref]: https://docs.python.org/3/reference/index.html

- **`3_data_model/`** — special methods on user-defined classes (the dunder
  protocols): `__repr__`/`__eq__` (3.3.1), descriptors & `__getattr__` (3.3.2),
  `__call__` (3.3.6), `__len__`/`__getitem__`/`__iter__` (3.3.7),
  `__add__`/`__radd__` as a class feature (3.3.8).
- **`6_expressions/`** — operators and expression forms: arithmetic (6.7),
  power (6.5), bitwise/shift (6.9), comparisons & membership (6.10), boolean
  ops (6.11), conditional (6.13), lambdas (6.14), literals & f-strings (6.2.2),
  the various displays/comprehensions/genexps/yield (6.2.x), primaries —
  attribute refs, slicings, calls (6.3.x).
- **`7_simple_statements/`** — assignment & unpacking (7.2), augmented
  assignment (7.2.1), `del` (7.5), `raise` (7.8), `nonlocal`/scoping (7.13).
- **`8_compound_statements/`** — `if` (8.1), `while` (8.2), `for` (8.3), `try`
  (8.4), `with` (8.5), `match` (8.6), function definitions incl. decorators
  (8.7), class definitions incl. inheritance/MRO/super (8.8).
- **`9_top_level_components/`** — minimal complete programs (9.1).

## Conventions

- **No numeric prefixes.** File names are descriptive only
  (`add_sub_int.py`, not `105_add_sub_int.py`); ordering within a folder is
  alphabetical and carries no meaning.
- Names are unique across the whole tree, so a single test can be run with
  `dune exec test/interp_runner.exe -- test/programs <substring>` — the
  substring matches anywhere in the path, so it also selects a whole section
  (e.g. `6.7` or `arithmetic`).
- Keep each program's output deterministic (sort sets/dicts before printing,
  avoid addresses/timestamps) so the byte-for-byte comparison is stable.

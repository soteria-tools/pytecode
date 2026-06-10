# pytecode

CPython bytecode as a pure OCaml data structure — the foundation for a Python
interpreter written in OCaml.

pytecode does **not** reimplement Python compilation. It runs the host
CPython (pinned to **3.13**) to parse and compile, then extracts the
resulting code-object tree through a small dump script into an OCaml AST:
an array of instructions with opcodes as a variant type, plus full
code-object metadata (constants, names, localsplus, exception table,
line/column positions). Once loaded, the AST has zero Python dependency.

```ocaml
match Pytecode.Loader.load_file ~cache:true "foo.py" with
| Ok code -> Format.printf "%a" Pytecode.Ast.pp_code code
| Error e -> prerr_endline (Pytecode.Error.to_string e)
```

```console
$ pytecode dump foo.py    # dis-like pretty-print of the AST
$ pytecode json foo.py    # raw JSON envelope, for debugging
```

## Architecture

```
source.py ──► tools/dump_bytecode.py (pinned 3.13) ──► versioned JSON
                                                          │
            Subprocess backend (default) ─────────────────┤
            pyml in-process backend (future) ─────────────┤
                                                          ▼
                                  Decode (yojson) ──► Ast.code (pure OCaml)
                                                          ▲
            native .pyc reader (possible future) ─────────┘
```

- **One normalization**, specified in [doc/normalization.md](doc/normalization.md):
  `CACHE`/`EXTENDED_ARG` stripped; jumps and exception-table boundaries are
  absolute instruction indices. Everything else is raw CPython semantics.
- **Backends are swappable** (`Backend_intf.S`) and all produce `Ast.code`,
  so the golden tests validate every backend. A pyml backend can reuse the
  embedded dump script verbatim (run it in-process, hand the JSON string to
  the same decoder).
- **`Cache.wrap`** gives `.pyc`-style caching: BLAKE-256 of (backend
  identity, path, source) → marshalled AST. Warm loads never touch Python
  (measured: full stdlib, 1072 files, 0.2 s warm vs 8.5 s cold).

## Requirements

- OCaml ≥ 5.2, dune, yojson, zarith (ppx_expect to run tests)
- `python3.13` on `PATH` at load time (or `$PYTECODE_PYTHON`, or
  `Subprocess.make ~python`)

## Maintenance: bumping the pinned CPython

The dump script uses only `dis` APIs that are public and stable since 3.13
(plus a vendored 15-line exception-table parser, format stable 3.11 → 3.14).
To move to a new CPython:

1. Update `EXPECTED` in `tools/dump_bytecode.py` and
   `tools/gen_opcodes.py`, and `Decode.expected_python_prefix`.
2. Regenerate the opcode tables: `dune build @gen && dune promote`
   (run under the new `python3.13`/`python3.14`). The variant is a union
   over supported versions; opcode *names* are the wire format, so numeric
   renumbering between versions is irrelevant. The generated file is just
   the variant with `[@has_arg]`-style flag attributes — the in-tree
   `[@@deriving opcode]` ppx (`ppx/`) derives `to_string`/`of_string`/
   `all`/`has_*` from them.
3. `dune runtest` (golden outputs may shift with compiler changes — review
   and promote) and `dune build @stdlib` (acceptance gate: every stdlib
   module must round-trip with zero unknown opcodes).

## Testing

```console
$ dune runtest          # golden expect-tests + invariants
$ dune build @stdlib    # sweep the entire pinned stdlib (~10 s)
$ dune build @gen       # check generated opcode tables for drift
```

The dump script also has a `--check` mode that cross-validates the vendored
exception-table parser and localsplus reconstruction against CPython's
private helpers.

# The pytecode normalization contract

`Ast.code` mirrors CPython code objects with **exactly one** normalization,
applied at extraction time. Any backend (the subprocess dump script today, a
pyml backend, or a future native `.pyc` reader) must implement it
identically — the golden tests and the stdlib sweep are the executable
specification.

## Instruction stream

Relative to CPython's raw `co_code`:

1. **`CACHE` entries are removed.** They are inline-cache padding for the
   specializing interpreter and carry no semantics.
2. **`EXTENDED_ARG` entries are removed**, and every instruction's `arg` is
   the fully folded oparg (`dis` already reports folded args on the real
   instruction).
3. Specialized (adaptive) opcodes cannot appear: freshly compiled code is
   unspecialized by construction, and `dis` deoptimizes by default. The dump
   script asserts `opname == baseopname` under `--check`.

Because of (1) and (2), byte offsets into `co_code` are meaningless in the
AST. Therefore:

4. **Jump args are absolute instruction indices** into `Ast.code.instrs`
   (for every opcode with `Opcode.is_jump`), not byte offsets and not
   relative deltas. When mapping byte offsets to indices, note that a raw
   jump target may point at the `EXTENDED_ARG` prefix of an instruction —
   i.e. its `start_offset`, not its `offset`; both must resolve to the same
   index.
5. **Exception-table boundaries are instruction indices**: `start_idx`
   inclusive, `end_idx` exclusive (an `end` equal to the code's byte length
   maps to `Array.length instrs`), `target_idx` the handler entry point.
6. `push_lasti` semantics follow from (4): an interpreter pushes the
   *instruction index* of the raising instruction where CPython would push a
   byte offset. This is self-consistent — only the matching `RERAISE`
   consumes it.

Everything else is raw CPython: args index `consts`/`names`/`localsplus`
exactly as in `ceval.c`, including flag-bit encodings (LOAD_GLOBAL's low
"push NULL" bit, LOAD_ATTR's low method bit, LOAD_SUPER_ATTR's two low bits,
COMPARE_OP's `arg lsr 5` operation + bit 4 coerce-to-bool,
`*_FAST_*_FAST` packed 4-bit index pairs).

## localsplus

The merged fast-locals layout of 3.11+: `co_varnames`, then `co_cellvars`
entries not already in varnames, then `co_freevars`. A captured parameter
appears once, in its argument slot, with kind `Local_and_cell` (`MAKE_CELL`
converts it in place). All `*_FAST` / `*_DEREF` / `MAKE_CELL` args index
this single array. `nlocals` counts the `Local` + `Local_and_cell` slots.

## Constants

| Python | AST | wire encoding |
|---|---|---|
| `None` | `None_` | `null` |
| `bool` | `Bool` | `true`/`false` |
| `int` | `Int of Z.t` | bare JSON integer (arbitrary precision) |
| `float` | `Float` | `{"t":"f","v":float.hex()}` — exact, incl. `inf`/`nan`/`-0.0` |
| `complex` | `Complex` | `{"t":"z","re":hex,"im":hex}` |
| `str` | `Str` | JSON string; lone surrogates: `{"t":"sw","v":hex(WTF-8)}` |
| `bytes` | `Bytes` | `{"t":"b","v":hex}` |
| `tuple` | `Tuple` | JSON array |
| `frozenset` | `Frozenset` | `{"t":"fs","v":[...]}`, sorted by encoded form (deterministic; it is semantically a set) |
| code object | `Code` | `{"t":"co","v":{...}}`, recursive |
| `Ellipsis` | `Ellipsis` | `{"t":"el"}` |

This set was validated against every constant in the CPython 3.13 stdlib;
the dump script raises on anything else.

## Debug info

`lines` and `positions` are per-instruction parallel arrays (`-1` = absent
component); `positions` is empty when dumped with `--no-positions`.

## Wire format versioning

Every envelope carries `format` (this wire format's version — bump on any
schema change), the full `python` version (the decoder rejects anything
outside the pinned `3.13.` prefix), and the dump script's SHA-256.

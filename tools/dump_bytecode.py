#!/usr/bin/env python3
"""pytecode bytecode dumper — pinned to CPython 3.13.

Compiles Python source with the host CPython and emits a versioned JSON
representation of the code-object tree, normalized for pytecode (see
doc/normalization.md):

- CACHE/EXTENDED_ARG entries are stripped; args are EXTENDED_ARG-folded.
- Jump args and exception-table boundaries are absolute instruction indices.

Only documented, stable APIs of the `dis` module are used (everything needed
became public in 3.13: Instruction.jump_target, .start_offset, .line_number,
.positions), except that --check cross-validates against CPython private
helpers when they exist. The exception-table varint parser is vendored below:
the format is simple and has been stable 3.11 -> 3.14.

Modes:
    dump_bytecode.py FILE                     one JSON envelope on stdout
    dump_bytecode.py --stdin-source [--filename NAME]
                                              compile source read from stdin
    dump_bytecode.py --batch                  newline-delimited paths on stdin,
                                              one NDJSON envelope per line
Flags:
    --no-positions    omit column-level position info (smaller output)
    --check           cross-validate against CPython internals (slower)
    --allow-version   skip the pinned-version assertion (experiments only)

Envelope:    {"format": 1, "python": "3.13.5", "script": "<sha256>",
              "ok": true, "code": {...}}
          or {..., "ok": false, "error": {"kind": ..., "msg": ..., ...}}
Batch mode adds "file": <path> to every envelope.
"""

import argparse
import base64  # noqa: F401  (kept for forward-compat experiments)
import dis
import hashlib
import json
import sys

FORMAT = 1
EXPECTED = (3, 13)

CHECK = False
POSITIONS = True

try:
    with open(__file__, "rb") as _f:
        SCRIPT_SHA = hashlib.sha256(_f.read()).hexdigest()
except OSError:
    SCRIPT_SHA = "unknown"


def envelope(**kw):
    doc = {
        "format": FORMAT,
        "python": "%d.%d.%d" % sys.version_info[:3],
        "script": SCRIPT_SHA,
    }
    doc.update(kw)
    return doc


# ----------------------------------------------------------------------
# Exception table parsing (vendored from CPython Lib/dis.py; the byte
# format is documented in InternalDocs/exception_handling.md and stable
# since 3.11). Entries are (start, end, target, depth, lasti) with byte
# offsets; end = start + length, exclusive.
# ----------------------------------------------------------------------

def _parse_varint(iterator):
    b = next(iterator)
    val = b & 63
    while b & 64:
        val <<= 6
        b = next(iterator)
        val |= b & 63
    return val


def parse_exception_table(co):
    iterator = iter(co.co_exceptiontable)
    entries = []
    try:
        while True:
            start = _parse_varint(iterator) * 2
            length = _parse_varint(iterator) * 2
            end = start + length
            target = _parse_varint(iterator) * 2
            dl = _parse_varint(iterator)
            entries.append((start, end, target, dl >> 1, bool(dl & 1)))
    except StopIteration:
        return entries


# ----------------------------------------------------------------------
# String / constant encoding
# ----------------------------------------------------------------------

def enc_str(s):
    """A Python str as JSON: plain string normally; lone surrogates (legal in
    Python str, PEP 383) cannot transit JSON portably, so fall back to the
    hex of the surrogatepass (WTF-8) encoding."""
    try:
        s.encode("utf-8")
        return s
    except UnicodeEncodeError:
        return {"t": "sw", "v": s.encode("utf-8", "surrogatepass").hex()}


def enc_const(v):
    t = type(v)
    if v is None:
        return None
    if t is bool:
        return v
    if t is int:
        return v  # arbitrary precision; decoded with zarith
    if t is float:
        # float.hex() round-trips exactly, incl. inf/nan/-0.0, and OCaml's
        # float_of_string parses all its outputs natively.
        return {"t": "f", "v": v.hex()}
    if t is complex:
        return {"t": "z", "re": v.real.hex(), "im": v.imag.hex()}
    if t is str:
        return enc_str(v)
    if t is bytes:
        return {"t": "b", "v": v.hex()}
    if t is tuple:
        return [enc_const(x) for x in v]
    if t is frozenset:
        items = [enc_const(x) for x in v]
        # Deterministic order (it is semantically a set).
        items.sort(key=lambda e: json.dumps(e, sort_keys=True, ensure_ascii=False))
        return {"t": "fs", "v": items}
    if v is Ellipsis:
        return {"t": "el"}
    if t is type((lambda: 0).__code__):
        return {"t": "co", "v": enc_code(v)}
    raise TypeError("unsupported constant type: %s (%r)" % (t.__name__, v))


# ----------------------------------------------------------------------
# Code objects
# ----------------------------------------------------------------------

def localsplus(co):
    """Reconstruct the merged fast-locals layout of 3.11+: varnames, then
    cellvars not in varnames (captured parameters occupy their argument slot
    and are converted in place by MAKE_CELL), then freevars."""
    lp = []
    varnames = co.co_varnames
    cellvars = co.co_cellvars
    for v in varnames:
        lp.append([enc_str(v), "lc" if v in cellvars else "l"])
    for c in cellvars:
        if c not in varnames:
            lp.append([enc_str(c), "c"])
    for f in co.co_freevars:
        lp.append([enc_str(f), "f"])
    if CHECK and hasattr(co, "_varname_from_oparg"):
        for i, (name, _kind) in enumerate(lp):
            expect = co._varname_from_oparg(i)
            got = name if isinstance(name, str) else None
            assert got == expect, (co.co_qualname, i, got, expect)
    return lp


def enc_code(co):
    instrs = [
        i for i in dis.get_instructions(co) if i.opname != "EXTENDED_ARG"
    ]
    # Map byte offsets to instruction indices. Jump targets (and exception
    # table boundaries) may point at an instruction's EXTENDED_ARG prefix,
    # i.e. its start_offset rather than its offset: register both.
    idx_of = {}
    for k, i in enumerate(instrs):
        idx_of[i.offset] = k
        idx_of[i.start_offset] = k

    rows = []
    for i in instrs:
        if CHECK:
            assert i.opname == i.baseopname, (co.co_qualname, i)
        if i.jump_target is not None:
            arg = idx_of[i.jump_target]
        elif i.arg is not None:
            arg = i.arg
        else:
            arg = 0
        line = i.line_number if i.line_number is not None else -1
        if POSITIONS:
            p = i.positions
            pos = [
                -1 if p.lineno is None else p.lineno,
                -1 if p.end_lineno is None else p.end_lineno,
                -1 if p.col_offset is None else p.col_offset,
                -1 if p.end_col_offset is None else p.end_col_offset,
            ]
            rows.append([i.opname, arg, line, pos])
        else:
            rows.append([i.opname, arg, line])

    code_end = len(co.co_code)
    exn = []
    for start, end, target, depth, lasti in parse_exception_table(co):
        start_idx = idx_of[start]
        target_idx = idx_of[target]
        if end in idx_of:
            end_idx = idx_of[end]
        else:
            assert end == code_end, (co.co_qualname, start, end, code_end)
            end_idx = len(instrs)
        exn.append([start_idx, end_idx, target_idx, depth, 1 if lasti else 0])
    if CHECK and hasattr(dis, "_parse_exception_table"):
        official = [
            (e.start, e.end, e.target, e.depth, e.lasti)
            for e in dis._parse_exception_table(co)
        ]
        ours = parse_exception_table(co)
        assert ours == official, (co.co_qualname, ours, official)

    return {
        "filename": enc_str(co.co_filename),
        "name": enc_str(co.co_name),
        "qualname": enc_str(co.co_qualname),
        "firstlineno": co.co_firstlineno,
        "argcount": co.co_argcount,
        "posonlyargcount": co.co_posonlyargcount,
        "kwonlyargcount": co.co_kwonlyargcount,
        "nlocals": co.co_nlocals,
        "stacksize": co.co_stacksize,
        "flags": co.co_flags,
        "consts": [enc_const(c) for c in co.co_consts],
        "names": [enc_str(n) for n in co.co_names],
        "localsplus": localsplus(co),
        "instrs": rows,
        "exn": exn,
    }


# ----------------------------------------------------------------------
# Driver
# ----------------------------------------------------------------------

def compile_source(source_bytes, filename):
    # bytes input lets CPython honor PEP 263 coding cookies and BOMs.
    # dont_inherit: do not leak this script's own __future__ flags.
    return compile(source_bytes, filename, "exec", dont_inherit=True, optimize=0)


def syntax_error_doc(e):
    return {
        "kind": "syntax",
        "msg": e.msg or str(e),
        "filename": e.filename or "<unknown>",
        "line": e.lineno if e.lineno is not None else -1,
        "col": e.offset if e.offset is not None else -1,
        "text": (e.text or "").rstrip("\n"),
    }


def process(source_bytes, filename):
    try:
        co = compile_source(source_bytes, filename)
    except SyntaxError as e:
        return envelope(ok=False, error=syntax_error_doc(e))
    except ValueError as e:
        # e.g. null bytes in source
        return envelope(
            ok=False,
            error={
                "kind": "compile",
                "msg": str(e),
                "filename": filename,
                "line": -1,
                "col": -1,
                "text": "",
            },
        )
    return envelope(ok=True, code=enc_code(co))


def emit(doc, out):
    json.dump(doc, out, ensure_ascii=False, separators=(",", ":"))
    out.write("\n")
    out.flush()


def main():
    global CHECK, POSITIONS
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("file", nargs="?")
    ap.add_argument("--stdin-source", action="store_true")
    ap.add_argument("--filename", default="<string>")
    ap.add_argument("--batch", action="store_true")
    ap.add_argument("--no-positions", action="store_true")
    ap.add_argument("--check", action="store_true")
    ap.add_argument("--allow-version", action="store_true")
    args = ap.parse_args()

    CHECK = args.check
    POSITIONS = not args.no_positions

    sys.stdout.reconfigure(encoding="utf-8")
    out = sys.stdout

    if not args.allow_version and sys.version_info[:2] != EXPECTED:
        emit(
            envelope(
                ok=False,
                error={
                    "kind": "version",
                    "msg": "expected CPython %d.%d, running %s"
                    % (EXPECTED + (sys.version.split()[0],)),
                },
            ),
            out,
        )
        return

    if args.batch:
        paths = (
            sys.stdin.buffer.read()
            .decode("utf-8", "surrogateescape")
            .splitlines()
        )
        for p in paths:
            if not p:
                continue
            try:
                with open(p, "rb") as f:
                    src = f.read()
            except OSError as e:
                emit(
                    envelope(
                        file=p, ok=False, error={"kind": "io", "msg": str(e)}
                    ),
                    out,
                )
                continue
            doc = process(src, p)
            doc["file"] = p
            emit(doc, out)
        return

    if args.stdin_source:
        src = sys.stdin.buffer.read()
        emit(process(src, args.filename), out)
        return

    if args.file is None:
        ap.error("expected a FILE argument, --stdin-source, or --batch")
    try:
        with open(args.file, "rb") as f:
            src = f.read()
    except OSError as e:
        emit(envelope(ok=False, error={"kind": "io", "msg": str(e)}), out)
        return
    emit(process(src, args.file), out)


if __name__ == "__main__":
    main()

open Pytecode

(* Golden tests: compile a snippet with the pinned CPython and lock the
   pretty-printed AST. Review against `python3.13 -m dis` when promoting. *)

let dump src =
  match Loader.load_string ~filename:"<test>" src with
  | Ok code -> Format.printf "%a" Ast.pp_code code
  | Error e -> print_string ("ERROR: " ^ Error.to_string e)

let%expect_test "closure with captured parameter" =
  dump {|
def outer(x):
    def inner():
        nonlocal x
        x += 1
        return x
    return inner
|};
  [%expect {|
    <module> (<test>:1)
      argcount 0 (posonly 0, kwonly 0), nlocals 0, stacksize 1, flags 0x0
      names: outer
         0    0  RESUME                        0
         2    1  LOAD_CONST                    0  (<code outer>)
              2  MAKE_FUNCTION
              3  STORE_NAME                    0  (outer)
              4  RETURN_CONST                  1  (None)

    Disassembly of outer:
    outer (<test>:2)
      argcount 1 (posonly 0, kwonly 0), nlocals 2, stacksize 2, flags 0x3 [OPTIMIZED NEWLOCALS]
      localsplus: x:local+cell, inner:local
              0  MAKE_CELL                     0  (x)
         2    1  RESUME                        0
         3    2  LOAD_FAST                     0  (x)
              3  BUILD_TUPLE                   1
              4  LOAD_CONST                    1  (<code outer.<locals>.inner>)
              5  MAKE_FUNCTION
              6  SET_FUNCTION_ATTRIBUTE        8
              7  STORE_FAST                    1  (inner)
         7    8  LOAD_FAST                     1  (inner)
              9  RETURN_VALUE

    Disassembly of outer.<locals>.inner:
    outer.<locals>.inner (<test>:3)
      argcount 0 (posonly 0, kwonly 0), nlocals 0, stacksize 2, flags 0x13 [OPTIMIZED NEWLOCALS NESTED]
      localsplus: x:free
              0  COPY_FREE_VARS                1
         3    1  RESUME                        0
         5    2  LOAD_DEREF                    0  (x)
              3  LOAD_CONST                    1  (1)
              4  BINARY_OP                    13  (+=)
              5  STORE_DEREF                   0  (x)
         6    6  LOAD_DEREF                    0  (x)
              7  RETURN_VALUE
    |}]

let%expect_test "try/except/finally exception table" =
  dump {|
def f(a):
    try:
        return 1 / a
    except ZeroDivisionError as e:
        return e
    finally:
        print("done")
|};
  [%expect {|
    <module> (<test>:1)
      argcount 0 (posonly 0, kwonly 0), nlocals 0, stacksize 1, flags 0x0
      names: f
         0    0  RESUME                        0
         2    1  LOAD_CONST                    0  (<code f>)
              2  MAKE_FUNCTION
              3  STORE_NAME                    0  (f)
              4  RETURN_CONST                  1  (None)

    Disassembly of f:
    f (<test>:2)
      argcount 1 (posonly 0, kwonly 0), nlocals 2, stacksize 5, flags 0x3 [OPTIMIZED NEWLOCALS]
      names: print, ZeroDivisionError
      localsplus: a:local, e:local
         2    0  RESUME                        0
         3    1  NOP
         4    2  LOAD_CONST                    1  (1)
              3  LOAD_FAST                     0  (a)
              4  BINARY_OP                    11  (/)
         8    5  LOAD_GLOBAL                   1  (print (+NULL))
              6  LOAD_CONST                    2  ('done')
              7  CALL                          1
              8  POP_TOP
              9  RETURN_VALUE
             10  PUSH_EXC_INFO
         5   11  LOAD_GLOBAL                   2  (ZeroDivisionError)
             12  CHECK_EXC_MATCH
             13  POP_JUMP_IF_FALSE            30  (to 30)
             14  STORE_FAST                    1  (e)
         6   15  LOAD_FAST                     1  (e)
             16  SWAP                          2
             17  POP_EXCEPT
             18  LOAD_CONST                    0  (None)
             19  STORE_FAST                    1  (e)
             20  DELETE_FAST                   1  (e)
         8   21  LOAD_GLOBAL                   1  (print (+NULL))
             22  LOAD_CONST                    2  ('done')
             23  CALL                          1
             24  POP_TOP
             25  RETURN_VALUE
             26  LOAD_CONST                    0  (None)
             27  STORE_FAST                    1  (e)
             28  DELETE_FAST                   1  (e)
             29  RERAISE                       1
         5   30  RERAISE                       0
             31  COPY                          3
             32  POP_EXCEPT
             33  RERAISE                       1
             34  PUSH_EXC_INFO
         8   35  LOAD_GLOBAL                   1  (print (+NULL))
             36  LOAD_CONST                    2  ('done')
             37  CALL                          1
             38  POP_TOP
             39  RERAISE                       0
             40  COPY                          3
             41  POP_EXCEPT
             42  RERAISE                       1
      exception table:
        [2, 5) -> 10 depth=0
        [10, 15) -> 31 depth=1 lasti
        [15, 16) -> 26 depth=1 lasti
        [16, 17) -> 31 depth=1 lasti
        [17, 21) -> 34 depth=0
        [26, 31) -> 31 depth=1 lasti
        [31, 34) -> 34 depth=0
        [34, 40) -> 40 depth=1 lasti
    |}]

let%expect_test "loop with break and continue" =
  dump {|
def f(xs):
    total = 0
    for x in xs:
        if x < 0:
            continue
        if x > 100:
            break
        total += x
    return total
|};
  [%expect {|
    <module> (<test>:1)
      argcount 0 (posonly 0, kwonly 0), nlocals 0, stacksize 1, flags 0x0
      names: f
         0    0  RESUME                        0
         2    1  LOAD_CONST                    0  (<code f>)
              2  MAKE_FUNCTION
              3  STORE_NAME                    0  (f)
              4  RETURN_CONST                  1  (None)

    Disassembly of f:
    f (<test>:2)
      argcount 1 (posonly 0, kwonly 0), nlocals 3, stacksize 3, flags 0x3 [OPTIMIZED NEWLOCALS]
      localsplus: xs:local, total:local, x:local
         2    0  RESUME                        0
         3    1  LOAD_CONST                    1  (0)
              2  STORE_FAST                    1  (total)
         4    3  LOAD_FAST                     0  (xs)
              4  GET_ITER
              5  FOR_ITER                     23  (to 23)
              6  STORE_FAST                    2  (x)
         5    7  LOAD_FAST                     2  (x)
              8  LOAD_CONST                    1  (0)
              9  COMPARE_OP                   18  (< (bool))
             10  POP_JUMP_IF_FALSE            12  (to 12)
         6   11  JUMP_BACKWARD                 5  (to 5)
         7   12  LOAD_FAST                     2  (x)
             13  LOAD_CONST                    2  (100)
             14  COMPARE_OP                  148  (> (bool))
             15  POP_JUMP_IF_FALSE            19  (to 19)
         8   16  POP_TOP
        10   17  LOAD_FAST                     1  (total)
             18  RETURN_VALUE
         9   19  LOAD_FAST_LOAD_FAST          18  (total, x)
             20  BINARY_OP                    13  (+=)
             21  STORE_FAST                    1  (total)
             22  JUMP_BACKWARD                 5  (to 5)
         4   23  END_FOR
             24  POP_TOP
        10   25  LOAD_FAST                     1  (total)
             26  RETURN_VALUE
    |}]

let%expect_test "match statement" =
  dump {|
def f(p):
    match p:
        case (0, y):
            return y
        case {"k": v}:
            return v
        case [1, *rest]:
            return rest
        case _:
            return None
|};
  [%expect {|
    <module> (<test>:1)
      argcount 0 (posonly 0, kwonly 0), nlocals 0, stacksize 1, flags 0x0
      names: f
         0    0  RESUME                        0
         2    1  LOAD_CONST                    0  (<code f>)
              2  MAKE_FUNCTION
              3  STORE_NAME                    0  (f)
              4  RETURN_CONST                  1  (None)

    Disassembly of f:
    f (<test>:2)
      argcount 1 (posonly 0, kwonly 0), nlocals 4, stacksize 5, flags 0x3 [OPTIMIZED NEWLOCALS]
      localsplus: p:local, y:local, v:local, rest:local
         2    0  RESUME                        0
         3    1  LOAD_FAST                     0  (p)
         4    2  COPY                          1
              3  MATCH_SEQUENCE
              4  POP_JUMP_IF_FALSE            17  (to 17)
              5  GET_LEN
              6  LOAD_CONST                    1  (2)
              7  COMPARE_OP                   72  (==)
              8  POP_JUMP_IF_FALSE            17  (to 17)
              9  UNPACK_SEQUENCE               2
             10  LOAD_CONST                    2  (0)
             11  COMPARE_OP                   88  (== (bool))
             12  POP_JUMP_IF_FALSE            17  (to 17)
             13  STORE_FAST                    1  (y)
             14  POP_TOP
         5   15  LOAD_FAST                     1  (y)
             16  RETURN_VALUE
         4   17  POP_TOP
         6   18  COPY                          1
             19  MATCH_MAPPING
             20  POP_JUMP_IF_FALSE            38  (to 38)
             21  GET_LEN
             22  LOAD_CONST                    3  (1)
             23  COMPARE_OP                  172  (>=)
             24  POP_JUMP_IF_FALSE            38  (to 38)
             25  LOAD_CONST                    4  (('k',))
             26  MATCH_KEYS
             27  COPY                          1
             28  POP_JUMP_IF_NONE             36  (to 36)
             29  UNPACK_SEQUENCE               1
             30  STORE_FAST                    2  (v)
             31  POP_TOP
             32  POP_TOP
             33  POP_TOP
         7   34  LOAD_FAST                     2  (v)
             35  RETURN_VALUE
         6   36  POP_TOP
             37  POP_TOP
             38  POP_TOP
         8   39  MATCH_SEQUENCE
             40  POP_JUMP_IF_FALSE            52  (to 52)
             41  GET_LEN
             42  LOAD_CONST                    3  (1)
             43  COMPARE_OP                  172  (>=)
             44  POP_JUMP_IF_FALSE            52  (to 52)
             45  UNPACK_EX                     1
             46  LOAD_CONST                    3  (1)
             47  COMPARE_OP                   88  (== (bool))
             48  POP_JUMP_IF_FALSE            52  (to 52)
             49  STORE_FAST                    3  (rest)
         9   50  LOAD_FAST                     3  (rest)
             51  RETURN_VALUE
         8   52  POP_TOP
        10   53  NOP
        11   54  RETURN_CONST                  0  (None)
    |}]

let%expect_test "generator, coroutine, async generator flags" =
  dump {|
def gen():
    yield 1

async def coro():
    return 1

async def agen():
    yield 1
|};
  [%expect {|
    <module> (<test>:1)
      argcount 0 (posonly 0, kwonly 0), nlocals 0, stacksize 1, flags 0x0
      names: gen, coro, agen
         0    0  RESUME                        0
         2    1  LOAD_CONST                    0  (<code gen>)
              2  MAKE_FUNCTION
              3  STORE_NAME                    0  (gen)
         5    4  LOAD_CONST                    1  (<code coro>)
              5  MAKE_FUNCTION
              6  STORE_NAME                    1  (coro)
         8    7  LOAD_CONST                    2  (<code agen>)
              8  MAKE_FUNCTION
              9  STORE_NAME                    2  (agen)
             10  RETURN_CONST                  3  (None)

    Disassembly of gen:
    gen (<test>:2)
      argcount 0 (posonly 0, kwonly 0), nlocals 0, stacksize 2, flags 0x23 [OPTIMIZED NEWLOCALS GENERATOR]
         2    0  RETURN_GENERATOR
              1  POP_TOP
              2  RESUME                        0
         3    3  LOAD_CONST                    1  (1)
              4  YIELD_VALUE                   0
              5  RESUME                        5
              6  POP_TOP
              7  RETURN_CONST                  0  (None)
              8  CALL_INTRINSIC_1              3
              9  RERAISE                       1
      exception table:
        [2, 8) -> 8 depth=0 lasti

    Disassembly of coro:
    coro (<test>:5)
      argcount 0 (posonly 0, kwonly 0), nlocals 0, stacksize 2, flags 0x83 [OPTIMIZED NEWLOCALS COROUTINE]
         5    0  RETURN_GENERATOR
              1  POP_TOP
              2  RESUME                        0
         6    3  RETURN_CONST                  1  (1)
              4  CALL_INTRINSIC_1              3
              5  RERAISE                       1
      exception table:
        [2, 4) -> 4 depth=0 lasti

    Disassembly of agen:
    agen (<test>:8)
      argcount 0 (posonly 0, kwonly 0), nlocals 0, stacksize 2, flags 0x203 [OPTIMIZED NEWLOCALS ASYNC_GENERATOR]
         8    0  RETURN_GENERATOR
              1  POP_TOP
              2  RESUME                        0
         9    3  LOAD_CONST                    1  (1)
              4  CALL_INTRINSIC_1              4
              5  YIELD_VALUE                   0
              6  RESUME                        5
              7  POP_TOP
              8  RETURN_CONST                  0  (None)
              9  CALL_INTRINSIC_1              3
             10  RERAISE                       1
      exception table:
        [2, 9) -> 9 depth=0 lasti
    |}]

let%expect_test "inlined comprehension" =
  dump {|
def f(xs):
    return [x * 2 for x in xs if x]
|};
  [%expect {|
    <module> (<test>:1)
      argcount 0 (posonly 0, kwonly 0), nlocals 0, stacksize 1, flags 0x0
      names: f
         0    0  RESUME                        0
         2    1  LOAD_CONST                    0  (<code f>)
              2  MAKE_FUNCTION
              3  STORE_NAME                    0  (f)
              4  RETURN_CONST                  1  (None)

    Disassembly of f:
    f (<test>:2)
      argcount 1 (posonly 0, kwonly 0), nlocals 2, stacksize 5, flags 0x3 [OPTIMIZED NEWLOCALS]
      localsplus: xs:local, x:local
         2    0  RESUME                        0
         3    1  LOAD_FAST                     0  (xs)
              2  GET_ITER
              3  LOAD_FAST_AND_CLEAR           1  (x)
              4  SWAP                          2
              5  BUILD_LIST                    0
              6  SWAP                          2
              7  GET_ITER
              8  FOR_ITER                     18  (to 18)
              9  STORE_FAST_LOAD_FAST         17  (x, x)
             10  TO_BOOL
             11  POP_JUMP_IF_TRUE             13  (to 13)
             12  JUMP_BACKWARD                 8  (to 8)
             13  LOAD_FAST                     1  (x)
             14  LOAD_CONST                    1  (2)
             15  BINARY_OP                     5  (*)
             16  LIST_APPEND                   2
             17  JUMP_BACKWARD                 8  (to 8)
             18  END_FOR
             19  POP_TOP
             20  SWAP                          2
             21  STORE_FAST                    1  (x)
             22  RETURN_VALUE
             23  SWAP                          2
             24  POP_TOP
             25  SWAP                          2
             26  STORE_FAST                    1  (x)
             27  RERAISE                       0
      exception table:
        [5, 11) -> 23 depth=2
        [13, 20) -> 23 depth=2
    |}]

let%expect_test "class body with decorator and method" =
  dump {|
@register
class C(Base):
    tag = "c"

    def method(self):
        return self.tag
|};
  [%expect {|
    <module> (<test>:1)
      argcount 0 (posonly 0, kwonly 0), nlocals 0, stacksize 6, flags 0x0
      names: register, Base, C
         0    0  RESUME                        0
         2    1  LOAD_NAME                     0  (register)
         3    2  LOAD_BUILD_CLASS
              3  PUSH_NULL
              4  LOAD_CONST                    0  (<code C>)
              5  MAKE_FUNCTION
              6  LOAD_CONST                    1  ('C')
              7  LOAD_NAME                     1  (Base)
              8  CALL                          3
         2    9  CALL                          0
         3   10  STORE_NAME                    2  (C)
             11  RETURN_CONST                  2  (None)

    Disassembly of C:
    C (<test>:2)
      argcount 0 (posonly 0, kwonly 0), nlocals 0, stacksize 1, flags 0x0
      names: __name__, __module__, __qualname__, __firstlineno__, tag, method, __static_attributes__
         2    0  RESUME                        0
              1  LOAD_NAME                     0  (__name__)
              2  STORE_NAME                    1  (__module__)
              3  LOAD_CONST                    0  ('C')
              4  STORE_NAME                    2  (__qualname__)
              5  LOAD_CONST                    1  (2)
              6  STORE_NAME                    3  (__firstlineno__)
         4    7  LOAD_CONST                    2  ('c')
              8  STORE_NAME                    4  (tag)
         6    9  LOAD_CONST                    3  (<code C.method>)
             10  MAKE_FUNCTION
             11  STORE_NAME                    5  (method)
             12  LOAD_CONST                    4  (())
             13  STORE_NAME                    6  (__static_attributes__)
             14  RETURN_CONST                  5  (None)

    Disassembly of C.method:
    C.method (<test>:6)
      argcount 1 (posonly 0, kwonly 0), nlocals 1, stacksize 1, flags 0x3 [OPTIMIZED NEWLOCALS]
      names: tag
      localsplus: self:local
         6    0  RESUME                        0
         7    1  LOAD_FAST                     0  (self)
              2  LOAD_ATTR                     0  (tag)
              3  RETURN_VALUE
    |}]

let%expect_test "with block" =
  dump {|
def f(path):
    with open(path) as fh:
        return fh.read()
|};
  [%expect {|
    <module> (<test>:1)
      argcount 0 (posonly 0, kwonly 0), nlocals 0, stacksize 1, flags 0x0
      names: f
         0    0  RESUME                        0
         2    1  LOAD_CONST                    0  (<code f>)
              2  MAKE_FUNCTION
              3  STORE_NAME                    0  (f)
              4  RETURN_CONST                  1  (None)

    Disassembly of f:
    f (<test>:2)
      argcount 1 (posonly 0, kwonly 0), nlocals 2, stacksize 6, flags 0x3 [OPTIMIZED NEWLOCALS]
      names: open, read
      localsplus: path:local, fh:local
         2    0  RESUME                        0
         3    1  LOAD_GLOBAL                   1  (open (+NULL))
              2  LOAD_FAST                     0  (path)
              3  CALL                          1
              4  BEFORE_WITH
              5  STORE_FAST                    1  (fh)
         4    6  LOAD_FAST                     1  (fh)
              7  LOAD_ATTR                     3  (method read)
              8  CALL                          0
         3    9  SWAP                          2
             10  LOAD_CONST                    0  (None)
             11  LOAD_CONST                    0  (None)
             12  LOAD_CONST                    0  (None)
             13  CALL                          2
             14  POP_TOP
             15  RETURN_VALUE
             16  PUSH_EXC_INFO
             17  WITH_EXCEPT_START
             18  TO_BOOL
             19  POP_JUMP_IF_TRUE             21  (to 21)
             20  RERAISE                       2
             21  POP_TOP
             22  POP_EXCEPT
             23  POP_TOP
             24  POP_TOP
             25  RETURN_CONST                  0  (None)
             26  COPY                          3
             27  POP_EXCEPT
             28  RERAISE                       1
      exception table:
        [5, 9) -> 16 depth=1 lasti
        [16, 22) -> 26 depth=3 lasti
    |}]

let%expect_test "constant zoo" =
  dump
    {|
BIG = 123456789012345678901234567890
FLOATS = (0.5, -0.0, float("inf"))
C = 1 + 2j
FS = frozenset({1, "a", (2, 3)})
B = b"\x00ascii\xff"
S = "héllo \N{GRINNING FACE}"
E = ...
NESTED = ((1, (2, 3)), None, True)
|};
  [%expect {|
    <module> (<test>:1)
      argcount 0 (posonly 0, kwonly 0), nlocals 0, stacksize 5, flags 0x0
      names: BIG, float, FLOATS, C, frozenset, FS, B, S, E, NESTED
         0    0  RESUME                        0
         2    1  LOAD_CONST                    0  (123456789012345678901234567890)
              2  STORE_NAME                    0  (BIG)
         3    3  LOAD_CONST                    1  (0.5)
              4  LOAD_CONST                    2  (-0.0)
              5  LOAD_NAME                     1  (float)
              6  PUSH_NULL
              7  LOAD_CONST                    3  ('inf')
              8  CALL                          1
              9  BUILD_TUPLE                   3
             10  STORE_NAME                    2  (FLOATS)
         4   11  LOAD_CONST                    4  (complex(1.0, 2.0))
             12  STORE_NAME                    3  (C)
         5   13  LOAD_NAME                     4  (frozenset)
             14  PUSH_NULL
             15  BUILD_SET                     0
             16  LOAD_CONST                    5  (frozenset({'a', 1, (2, 3)}))
             17  SET_UPDATE                    1
             18  CALL                          1
             19  STORE_NAME                    5  (FS)
         6   20  LOAD_CONST                    6  (b'\x00ascii\xff')
             21  STORE_NAME                    6  (B)
         7   22  LOAD_CONST                    7  ('héllo 😀')
             23  STORE_NAME                    7  (S)
         8   24  LOAD_CONST                    8  (Ellipsis)
             25  STORE_NAME                    8  (E)
         9   26  LOAD_CONST                    9  (((1, (2, 3)), None, True))
             27  STORE_NAME                    9  (NESTED)
             28  RETURN_CONST                 10  (None)
    |}]

let%expect_test "syntax error" =
  dump "def f(:\n";
  [%expect {|
    ERROR: <test>:1:7: syntax error: invalid syntax
      def f(:
    |}]

(* ------------------------------------------------------------------ *)
(* Invariant tests (summarized output, not full dumps)                 *)
(* ------------------------------------------------------------------ *)

let load_exn src =
  match Loader.load_string ~filename:"<test>" src with
  | Ok code -> code
  | Error e -> failwith (Error.to_string e)

let rec iter_codes f (c : Ast.code) =
  f c;
  Array.iter (function Ast.Code n -> iter_codes f n | _ -> ()) c.consts

let%expect_test "EXTENDED_ARG folding and far jumps" =
  (* >256 constants force LOAD_CONST args > 255 (EXTENDED_ARG-folded), and a
     fat loop body forces jump offsets beyond one byte. *)
  let buf = Buffer.create 4096 in
  Buffer.add_string buf "def f(x):\n    acc = 0\n";
  Buffer.add_string buf "    while x:\n";
  for i = 0 to 299 do
    Buffer.add_string buf (Printf.sprintf "        acc += %d.5\n" i)
  done;
  Buffer.add_string buf "    return acc\n";
  let code = load_exn (Buffer.contents buf) in
  let big_arg = ref 0 and bad_jump = ref 0 and stripped_ok = ref true in
  iter_codes
    (fun c ->
      let n = Array.length c.instrs in
      Array.iter
        (fun { Ast.op; arg } ->
          if Opcode.has_arg op && arg > 255 then incr big_arg;
          if Opcode.is_jump op && not (arg >= 0 && arg < n) then incr bad_jump;
          match op with
          | CACHE | EXTENDED_ARG -> stripped_ok := false
          | _ -> ())
        c.instrs)
    code;
  Printf.printf "args >255: %s, jumps in range: %b, no CACHE/EXTENDED_ARG: %b\n"
    (if !big_arg > 0 then "yes" else "NO")
    (!bad_jump = 0) !stripped_ok;
  [%expect {| args >255: yes, jumps in range: true, no CACHE/EXTENDED_ARG: true |}]

let%expect_test "positions cover every instruction" =
  let code = load_exn "def f(x):\n    return x + 1\n" in
  let ok = ref true in
  iter_codes
    (fun c ->
      if Array.length c.positions <> Array.length c.instrs then ok := false;
      if Array.length c.lines <> Array.length c.instrs then ok := false)
    code;
  Printf.printf "positions/lines aligned: %b\n" !ok;
  [%expect {| positions/lines aligned: true |}]

let%expect_test "lone surrogate string constant survives as WTF-8" =
  let code = load_exn "S = \"\\ud800abc\"\n" in
  let found = ref None in
  iter_codes
    (fun c ->
      Array.iter
        (function Ast.Str s -> found := Some s | _ -> ())
        c.consts)
    code;
  (match !found with
  | Some s ->
      String.iter (fun ch -> Printf.printf "%02x " (Char.code ch)) s;
      print_newline ()
  | None -> print_endline "no string constant found");
  [%expect {| ed a0 80 61 62 63 |}]

let%expect_test "batch mode preserves order and isolates failures" =
  let write name contents =
    let path = Filename.concat (Filename.get_temp_dir_name ()) name in
    Out_channel.with_open_bin path (fun oc ->
        Out_channel.output_string oc contents);
    path
  in
  let good1 = write "pytecode_b1.py" "x = 1\n" in
  let bad = write "pytecode_b2.py" "def f(:\n" in
  let good2 = write "pytecode_b3.py" "y = 2\n" in
  let (module B : Backend_intf.S) = Loader.default_backend () in
  B.compile_batch [ good1; bad; good2 ]
  |> List.iter (fun (path, result) ->
         Printf.printf "%s -> %s\n"
           (Filename.basename path)
           (match result with
           | Ok code -> "ok (" ^ code.Ast.qualname ^ ")"
           | Error (Error.Python_syntax_error _) -> "syntax error"
           | Error e -> "ERROR: " ^ Error.to_string e));
  [%expect {|
    pytecode_b1.py -> ok (<module>)
    pytecode_b2.py -> syntax error
    pytecode_b3.py -> ok (<module>)
    |}]

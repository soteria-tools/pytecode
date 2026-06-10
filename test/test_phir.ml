open Pytecode

(* Golden tests for the Phir transformation. *)

let phir src =
  match Loader.load_string ~filename:"<test>" src with
  | Ok code -> Format.printf "%a" Phir.pp_code (Phir.of_code code)
  | Error e -> print_string ("ERROR: " ^ Error.to_string e)

let%expect_test "assignments fold constants and variable reads" =
  phir {|
x = 5
y = x
|};
  [%expect
    {|
    <module> (<test>:1)
      argcount 0 (posonly 0, kwonly 0), nlocals 0, stacksize 1, flags 0x0
         2    0  Assign(name:x, 5)
         3    1  Assign(name:y, name:x)
              2  Return(None)
    |}]

let%expect_test "operators receive their arguments directly" =
  phir {|
def f(a, b):
    c = a + b
    c += 1
    return c * 2
|};
  [%expect
    {|
    <module> (<test>:1)
      argcount 0 (posonly 0, kwonly 0), nlocals 0, stacksize 1, flags 0x0
         2    0  Make_function(<code f>)
              1  Assign(name:f, stack)
              2  Return(None)

    Disassembly of f:
    f (<test>:2)
      argcount 2 (posonly 0, kwonly 0), nlocals 3, stacksize 2, flags 0x3 [OPTIMIZED NEWLOCALS]
      localsplus: a:local, b:local, c:local
         3    0  Binary_op(+, a, b)
              1  Assign(c, stack)
         4    2  Binary_op(+=, c, 1)
              3  Assign(c, stack)
         5    4  Binary_op(*, c, 2)
              5  Return(stack)
    |}]

let%expect_test "calls carry the callee and arguments" =
  phir {|
print("hi", 1)

def g(o, x):
    return o.m(x, 2)
|};
  [%expect
    {|
    <module> (<test>:1)
      argcount 0 (posonly 0, kwonly 0), nlocals 0, stacksize 4, flags 0x0
         2    0  Call(name:print, null, ['hi', 1])
              1  Pop_top(stack)
         4    2  Make_function(<code g>)
              3  Assign(name:g, stack)
              4  Return(None)

    Disassembly of g:
    g (<test>:4)
      argcount 2 (posonly 0, kwonly 0), nlocals 2, stacksize 4, flags 0x3 [OPTIMIZED NEWLOCALS]
      localsplus: o:local, x:local
         5    0  Load_method(o, m)
              1  Call(stack, stack, [x, 2])
              2  Return(stack)
    |}]

let%expect_test "nested calls: inner folds, outer takes stack" =
  phir "r = f(g(1))\n";
  [%expect
    {|
    <module> (<test>:1)
      argcount 0 (posonly 0, kwonly 0), nlocals 0, stacksize 5, flags 0x0
         1    0  Push(name:f)
              1  Push(null)
              2  Call(name:g, null, [1])
              3  Call(stack, stack, [stack])
              4  Assign(name:r, stack)
              5  Return(None)
    |}]

let%expect_test "loop: jumps are instruction indices, condition folded" =
  phir
    {|
def f(xs):
    total = 0
    for x in xs:
        if x > 100:
            break
        total += x
    return total
|};
  [%expect
    {|
    <module> (<test>:1)
      argcount 0 (posonly 0, kwonly 0), nlocals 0, stacksize 1, flags 0x0
         2    0  Make_function(<code f>)
              1  Assign(name:f, stack)
              2  Return(None)

    Disassembly of f:
    f (<test>:2)
      argcount 1 (posonly 0, kwonly 0), nlocals 3, stacksize 3, flags 0x3 [OPTIMIZED NEWLOCALS]
      localsplus: xs:local, total:local, x:local
         3    0  Assign(total, 0)
         4    1  Get_iter(xs)
              2  For_iter(11)
              3  Assign(x, stack)
         5    4  Compare(> as bool, x, 100)
              5  Cond_jump(if_false, stack, 8)
         6    6  Pop_top(stack)
         8    7  Return(total)
         7    8  Binary_op(+=, total, x)
              9  Assign(total, stack)
             10  Jump(2)
         4   11  End_for
             12  Pop_top(stack)
         8   13  Return(total)
    |}]

let%expect_test "try/except: folding stops at region boundaries" =
  phir
    {|
def f(a):
    try:
        return 1 / a
    except ZeroDivisionError:
        return 0
|};
  [%expect
    {|
    <module> (<test>:1)
      argcount 0 (posonly 0, kwonly 0), nlocals 0, stacksize 1, flags 0x0
         2    0  Make_function(<code f>)
              1  Assign(name:f, stack)
              2  Return(None)

    Disassembly of f:
    f (<test>:2)
      argcount 1 (posonly 0, kwonly 0), nlocals 1, stacksize 4, flags 0x3 [OPTIMIZED NEWLOCALS]
      localsplus: a:local
         4    0  Binary_op(/, 1, a)
              1  Return(stack)
              2  Push_exc_info
         5    3  Check_exc_match(global:ZeroDivisionError)
              4  Cond_jump(if_false, stack, 8)
              5  Pop_top(stack)
         6    6  Pop_except
              7  Return(0)
         5    8  Reraise(0)
              9  Copy(3)
             10  Pop_except
             11  Reraise(1)
      exception table:
        [0, 1) -> 2 depth=0
        [2, 6) -> 9 depth=1 lasti
        [8, 9) -> 9 depth=1 lasti
    |}]

let%expect_test "function definition with defaults" =
  phir {|
def f(a, b=1):
    return a + b
|};
  [%expect
    {|
    <module> (<test>:1)
      argcount 0 (posonly 0, kwonly 0), nlocals 0, stacksize 2, flags 0x0
         2    0  Push((1,))
              1  Make_function(<code f>)
              2  Set_function_attribute(defaults, stack, stack)
              3  Assign(name:f, stack)
              4  Return(None)

    Disassembly of f:
    f (<test>:2)
      argcount 2 (posonly 0, kwonly 0), nlocals 2, stacksize 2, flags 0x3 [OPTIMIZED NEWLOCALS]
      localsplus: a:local, b:local
         3    0  Binary_op(+, a, b)
              1  Return(stack)
    |}]

let%expect_test "import folds level and from-list constants" =
  phir "import os.path as p\nfrom sys import argv\n";
  [%expect
    {|
    <module> (<test>:1)
      argcount 0 (posonly 0, kwonly 0), nlocals 0, stacksize 2, flags 0x0
         1    0  Import_name(os.path, 0, None)
              1  Import_from(path)
              2  Assign(name:p, stack)
              3  Pop_top(stack)
         2    4  Import_name(sys, 0, ('argv',))
              5  Import_from(argv)
              6  Assign(name:argv, stack)
              7  Pop_top(stack)
              8  Return(None)
    |}]

let%expect_test "keyword call and f-string" =
  phir {|
def f(x):
    g(x, key=1)
    return f"v={x:>10}"
|};
  [%expect
    {|
    <module> (<test>:1)
      argcount 0 (posonly 0, kwonly 0), nlocals 0, stacksize 1, flags 0x0
         2    0  Make_function(<code f>)
              1  Assign(name:f, stack)
              2  Return(None)

    Disassembly of f:
    f (<test>:2)
      argcount 1 (posonly 0, kwonly 0), nlocals 1, stacksize 5, flags 0x3 [OPTIMIZED NEWLOCALS]
      localsplus: x:local
         3    0  Call_kw(global:g, null, [x, 1], ('key',))
              1  Pop_top(stack)
         4    2  Push('v=')
              3  Format_with_spec(x, '>10')
              4  Build_string([stack, stack])
              5  Return(stack)
    |}]

(* Invariants, checked across a tricky module. *)

let rec iter_codes f (c : Phir.code) =
  f c;
  Array.iter
    (fun ins ->
      List.iter
        (function Phir.Code n -> iter_codes f n | _ -> ())
        (Phir.values ins))
    c.instrs

let%expect_test "stack operands form a prefix of every operand list" =
  let src =
    {|
async def agen(xs):
    async for x in xs:
        yield x.f(1).g

def f(a, b, *args, **kw):
    match (a, b):
        case (0, [x, *rest]) if x > 0:
            return {**kw, "x": x}
        case {"k": v}:
            return lambda: v + a
    with open(a) as fh:
        try:
            return fh.read()[a:b]
        except (OSError, ValueError) as e:
            raise RuntimeError("bad") from e
        finally:
            del fh
|}
  in
  (match Loader.load_string ~filename:"<test>" src with
  | Error e -> print_string ("ERROR: " ^ Error.to_string e)
  | Ok code ->
      let phir = Phir.of_code code in
      let violations = ref 0 and instrs = ref 0 in
      iter_codes
        (fun c ->
          Array.iter
            (fun ins ->
              incr instrs;
              let seen_non_stack = ref false in
              List.iter
                (fun v ->
                  match v with
                  | Phir.Stack -> if !seen_non_stack then incr violations
                  | _ -> seen_non_stack := true)
                (Phir.values ins))
            c.instrs)
        phir;
      Printf.printf "instrs: %d, stack-prefix violations: %d\n" !instrs
        !violations);
  [%expect {| instrs: 125, stack-prefix violations: 0 |}]

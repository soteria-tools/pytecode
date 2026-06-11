type instr = { op : Opcode.t; arg : int }
type local_kind = Local | Cell | Local_and_cell | Free

type exn_entry = {
  start_idx : int;
  end_idx : int;
  target_idx : int;
  depth : int;
  push_lasti : bool;
}

type positions = {
  lineno : int;
  end_lineno : int;
  col_offset : int;
  end_col_offset : int;
}

type const =
  | None_
  | Bool of bool
  | Int of Z.t
  | Float of float
  | Complex of { re : float; im : float }
  | Str of string
  | Bytes of string
  | Tuple of const array
  | Frozenset of const array
  | Code of instr code
  | Ellipsis

(* Parameterized over the instruction representation so derived IRs (e.g.
   {!Phir}) can reuse the same frame, debug-info and exception-table shape and
   the shared pretty-printer below. [Ast.code] is [instr code]. *)
and 'i code = {
  filename : string;
  name : string;
  qualname : string;
  docstring : string option;
  firstlineno : int;
  argcount : int;
  posonlyargcount : int;
  kwonlyargcount : int;
  nlocals : int;
  stacksize : int;
  flags : int;
  consts : const array;
  names : string array;
  localsplus : (string * local_kind) array;
  instrs : 'i array;
  exn_table : exn_entry array;
  lines : int array;
  positions : positions array;
}

(* CO_* flag bits — stable across CPython versions. *)
let co_optimized = 0x1
let co_newlocals = 0x2
let co_varargs = 0x4
let co_varkeywords = 0x8
let co_nested = 0x10
let co_generator = 0x20
let co_nofree = 0x40
let co_coroutine = 0x80
let co_iterable_coroutine = 0x100
let co_async_generator = 0x200
let test_flag bit c = c.flags land bit <> 0
let is_optimized c = test_flag co_optimized c
let is_generator c = test_flag co_generator c
let is_coroutine c = test_flag co_coroutine c
let is_async_generator c = test_flag co_async_generator c
let has_varargs c = test_flag co_varargs c
let has_varkw c = test_flag co_varkeywords c
let is_nested c = test_flag co_nested c

let filter_localsplus pred c =
  Array.of_list
    (List.filter_map
       (fun (name, kind) -> if pred kind then Some name else None)
       (Array.to_list c.localsplus))

let varnames c =
  filter_localsplus (function Local | Local_and_cell -> true | _ -> false) c

let cellvars c =
  filter_localsplus (function Cell | Local_and_cell -> true | _ -> false) c

let freevars c = filter_localsplus (function Free -> true | _ -> false) c

(* ------------------------------------------------------------------ *)
(* Pretty-printing                                                     *)
(* ------------------------------------------------------------------ *)

let float_repr f =
  if Float.is_nan f then "nan"
  else if f = Float.infinity then "inf"
  else if f = Float.neg_infinity then "-inf"
  else if Float.is_integer f && Float.abs f < 1e16 then Printf.sprintf "%.1f" f
  else
    let s = Printf.sprintf "%.12g" f in
    if float_of_string s = f then s else Printf.sprintf "%.17g" f

(* Python-ish single-quoted repr; UTF-8 bytes >= 128 pass through. *)
let str_repr s =
  let buf = Buffer.create (String.length s + 2) in
  Buffer.add_char buf '\'';
  String.iter
    (fun ch ->
      match ch with
      | '\'' -> Buffer.add_string buf "\\'"
      | '\\' -> Buffer.add_string buf "\\\\"
      | '\n' -> Buffer.add_string buf "\\n"
      | '\r' -> Buffer.add_string buf "\\r"
      | '\t' -> Buffer.add_string buf "\\t"
      | c when Char.code c < 32 || Char.code c = 127 ->
          Buffer.add_string buf (Printf.sprintf "\\x%02x" (Char.code c))
      | c -> Buffer.add_char buf c)
    s;
  Buffer.add_char buf '\'';
  Buffer.contents buf

let bytes_repr s =
  let buf = Buffer.create (String.length s + 3) in
  Buffer.add_string buf "b'";
  String.iter
    (fun ch ->
      match ch with
      | '\'' -> Buffer.add_string buf "\\'"
      | '\\' -> Buffer.add_string buf "\\\\"
      | c when Char.code c < 32 || Char.code c >= 127 ->
          Buffer.add_string buf (Printf.sprintf "\\x%02x" (Char.code c))
      | c -> Buffer.add_char buf c)
    s;
  Buffer.add_char buf '\'';
  Buffer.contents buf

let rec pp_const ppf : const -> unit = function
  | None_ -> Fmt.string ppf "None"
  | Bool true -> Fmt.string ppf "True"
  | Bool false -> Fmt.string ppf "False"
  | Int z -> Fmt.string ppf (Z.to_string z)
  | Float f -> Fmt.string ppf (float_repr f)
  | Complex { re; im } ->
      Fmt.pf ppf "complex(%s, %s)" (float_repr re) (float_repr im)
  | Str s -> Fmt.string ppf (str_repr s)
  | Bytes s -> Fmt.string ppf (bytes_repr s)
  | Tuple [| x |] -> Fmt.pf ppf "(%a,)" pp_const x
  | Tuple xs -> Fmt.pf ppf "(%a)" pp_consts xs
  | Frozenset [||] -> Fmt.string ppf "frozenset()"
  | Frozenset xs -> Fmt.pf ppf "frozenset({%a})" pp_consts xs
  | Code c -> Fmt.pf ppf "<code %s>" c.qualname
  | Ellipsis -> Fmt.string ppf "Ellipsis"

(* A comma-separated run of constants, with literal ", " (never a Format break,
   so reprs stay on one line). *)
and pp_consts ppf xs = Fmt.(array ~sep:(any ", ") pp_const) ppf xs

let const_repr = Fmt.to_to_string pp_const

let local_kind_repr = function
  | Local -> "local"
  | Cell -> "cell"
  | Local_and_cell -> "local+cell"
  | Free -> "free"

let flags_repr flags =
  let names =
    List.filter_map
      (fun (bit, name) -> if flags land bit <> 0 then Some name else None)
      [
        (co_optimized, "OPTIMIZED");
        (co_newlocals, "NEWLOCALS");
        (co_varargs, "VARARGS");
        (co_varkeywords, "VARKEYWORDS");
        (co_nested, "NESTED");
        (co_generator, "GENERATOR");
        (co_nofree, "NOFREE");
        (co_coroutine, "COROUTINE");
        (co_iterable_coroutine, "ITERABLE_COROUTINE");
        (co_async_generator, "ASYNC_GENERATOR");
      ]
  in
  match names with [] -> "" | _ -> " [" ^ String.concat " " names ^ "]"

(* Best-effort human hint for an instruction arg, mirroring `dis`'s argrepr.
   Display only — the interpreter must decode args itself. *)
let arg_hint c { op; arg } =
  let name idx =
    if idx >= 0 && idx < Array.length c.names then c.names.(idx) else "?"
  in
  let local idx =
    if idx >= 0 && idx < Array.length c.localsplus then fst c.localsplus.(idx)
    else "?"
  in
  match op with
  | LOAD_GLOBAL ->
      Some (name (arg lsr 1) ^ if arg land 1 = 1 then " (+NULL)" else "")
  | LOAD_ATTR ->
      Some ((if arg land 1 = 1 then "method " else "") ^ name (arg lsr 1))
  | LOAD_SUPER_ATTR -> Some (name (arg lsr 2))
  | LOAD_FAST_LOAD_FAST | STORE_FAST_LOAD_FAST | STORE_FAST_STORE_FAST ->
      Some (local (arg lsr 4) ^ ", " ^ local (arg land 0xf))
  | BINARY_OP when arg < Array.length Opcode.binary_op_repr ->
      Some Opcode.binary_op_repr.(arg)
  | COMPARE_OP when arg lsr 5 < Array.length Opcode.cmp_op_repr ->
      Some
        (Opcode.cmp_op_repr.(arg lsr 5)
        ^ if arg land 16 <> 0 then " (bool)" else "")
  | op when Opcode.has_const op ->
      if arg >= 0 && arg < Array.length c.consts then
        Some (const_repr c.consts.(arg))
      else Some "?"
  | op when Opcode.is_jump op -> Some ("to " ^ string_of_int arg)
  | op when Opcode.has_name op -> Some (name arg)
  | op when Opcode.has_local op || Opcode.has_free op -> Some (local arg)
  | _ -> None

let pp_localsplus_entry ppf (n, k) = Fmt.pf ppf "%s:%s" n (local_kind_repr k)

let pp_exn_entry ppf e =
  Fmt.pf ppf "    [%d, %d) -> %d depth=%d%s@\n" e.start_idx e.end_idx
    e.target_idx e.depth
    (if e.push_lasti then " lasti" else "")

(* Pair each instruction with its index and a line-number label (shown only when
   the line changes), matching `dis`'s gutter. *)
let instr_rows (c : 'i code) =
  let _, _, rev =
    Array.fold_left
      (fun (i, prev, acc) ins ->
        let line = if i < Array.length c.lines then c.lines.(i) else -1 in
        let label, prev =
          if line <> prev && line >= 0 then (string_of_int line, line)
          else ("", prev)
        in
        (i + 1, prev, (label, i, ins) :: acc))
      (0, min_int, []) c.instrs
  in
  List.rev rev

(* Shared dis-like renderer. The instruction column ([pp_instr]) and the nested
   code objects ([children]) are supplied by the caller, so derived IRs reuse
   the frame header, localsplus/exception-table layout, line-label gutter and
   depth-first recursion. [pp_instr] must not emit trailing whitespace. *)
let rec pp_code_generic ~pp_instr ~children ppf (c : 'i code) =
  let pp_row ppf (label, i, ins) =
    Fmt.pf ppf "  %4s %4d  %a@\n" label i (pp_instr c) ins
  in
  Fmt.pf ppf "%s (%s:%d)@\n" c.qualname c.filename c.firstlineno;
  Fmt.pf ppf
    "  argcount %d (posonly %d, kwonly %d), nlocals %d, stacksize %d, flags \
     0x%x%s@\n"
    c.argcount c.posonlyargcount c.kwonlyargcount c.nlocals c.stacksize c.flags
    (flags_repr c.flags);
  if Array.length c.names > 0 then
    Fmt.pf ppf "  names: %a@\n" Fmt.(array ~sep:(any ", ") string) c.names;
  if Array.length c.localsplus > 0 then
    Fmt.pf ppf "  localsplus: %a@\n"
      Fmt.(array ~sep:(any ", ") pp_localsplus_entry)
      c.localsplus;
  Fmt.(list ~sep:nop pp_row) ppf (instr_rows c);
  if Array.length c.exn_table > 0 then begin
    Fmt.pf ppf "  exception table:@\n";
    Fmt.(array ~sep:nop pp_exn_entry) ppf c.exn_table
  end;
  (* Recurse into nested code objects, depth-first, like `dis`. *)
  List.iter
    (fun nested ->
      Fmt.pf ppf "@\nDisassembly of %s:@\n" nested.qualname;
      pp_code_generic ~pp_instr ~children ppf nested)
    (children c)

(* The instruction column for raw bytecode: opcode, decimal arg, dis-like hint.
   No padding when there is nothing after the opcode, so no trailing space. *)
let pp_instr c ppf ins =
  match
    (Opcode.has_arg ins.op, Option.map (Fmt.str "  (%s)") (arg_hint c ins))
  with
  | false, None -> Fmt.string ppf (Opcode.to_string ins.op)
  | has_arg, hint ->
      let arg_str = if has_arg then Fmt.str "%4d" ins.arg else "    " in
      Fmt.pf ppf "%-26s %s%s" (Opcode.to_string ins.op) arg_str
        (Option.value ~default:"" hint)

let children c =
  List.filter_map
    (function Code nested -> Some nested | _ -> None)
    (Array.to_list c.consts)

let pp_code ppf c = pp_code_generic ~pp_instr ~children ppf c

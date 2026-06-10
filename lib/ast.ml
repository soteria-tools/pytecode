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
  | Code of code
  | Ellipsis

and code = {
  filename : string;
  name : string;
  qualname : string;
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
  instrs : instr array;
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
let is_optimized = test_flag co_optimized
let is_generator = test_flag co_generator
let is_coroutine = test_flag co_coroutine
let is_async_generator = test_flag co_async_generator
let has_varargs = test_flag co_varargs
let has_varkw = test_flag co_varkeywords
let is_nested = test_flag co_nested

let filter_localsplus pred c =
  Array.of_list
    (List.filter_map
       (fun (name, kind) -> if pred kind then Some name else None)
       (Array.to_list c.localsplus))

let varnames = filter_localsplus (function Local | Local_and_cell -> true | _ -> false)
let cellvars = filter_localsplus (function Cell | Local_and_cell -> true | _ -> false)
let freevars = filter_localsplus (function Free -> true | _ -> false)

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

let rec const_repr = function
  | None_ -> "None"
  | Bool true -> "True"
  | Bool false -> "False"
  | Int z -> Z.to_string z
  | Float f -> float_repr f
  | Complex { re; im } ->
      Printf.sprintf "complex(%s, %s)" (float_repr re) (float_repr im)
  | Str s -> str_repr s
  | Bytes s -> bytes_repr s
  | Tuple [| x |] -> Printf.sprintf "(%s,)" (const_repr x)
  | Tuple xs ->
      Printf.sprintf "(%s)"
        (String.concat ", " (Array.to_list (Array.map const_repr xs)))
  | Frozenset [||] -> "frozenset()"
  | Frozenset xs ->
      Printf.sprintf "frozenset({%s})"
        (String.concat ", " (Array.to_list (Array.map const_repr xs)))
  | Code c -> Printf.sprintf "<code %s>" c.qualname
  | Ellipsis -> "Ellipsis"

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
    if idx >= 0 && idx < Array.length c.localsplus then
      fst c.localsplus.(idx)
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

let rec render buf c =
  let pr fmt = Printf.bprintf buf fmt in
  pr "%s (%s:%d)\n" c.qualname c.filename c.firstlineno;
  pr "  argcount %d (posonly %d, kwonly %d), nlocals %d, stacksize %d, flags 0x%x%s\n"
    c.argcount c.posonlyargcount c.kwonlyargcount c.nlocals c.stacksize
    c.flags (flags_repr c.flags);
  if Array.length c.names > 0 then
    pr "  names: %s\n" (String.concat ", " (Array.to_list c.names));
  if Array.length c.localsplus > 0 then
    pr "  localsplus: %s\n"
      (String.concat ", "
         (Array.to_list
            (Array.map
               (fun (n, k) -> n ^ ":" ^ local_kind_repr k)
               c.localsplus)));
  let prev_line = ref min_int in
  Array.iteri
    (fun i ins ->
      let line = if i < Array.length c.lines then c.lines.(i) else -1 in
      let line_label =
        if line <> !prev_line && line >= 0 then (
          prev_line := line;
          string_of_int line)
        else ""
      in
      let arg_str =
        if Opcode.has_arg ins.op then Printf.sprintf "%4d" ins.arg else "    "
      in
      let hint =
        match arg_hint c ins with
        | Some h -> Printf.sprintf "  (%s)" h
        | None -> ""
      in
      pr "  %4s %4d  %-26s %s%s\n" line_label i (Opcode.to_string ins.op)
        arg_str hint)
    c.instrs;
  if Array.length c.exn_table > 0 then begin
    pr "  exception table:\n";
    Array.iter
      (fun e ->
        pr "    [%d, %d) -> %d depth=%d%s\n" e.start_idx e.end_idx
          e.target_idx e.depth
          (if e.push_lasti then " lasti" else ""))
      c.exn_table
  end;
  (* Recurse into nested code objects, depth-first, like `dis`. *)
  Array.iter
    (function
      | Code nested ->
          pr "\nDisassembly of %s:\n" nested.qualname;
          render buf nested
      | _ -> ())
    c.consts

let pp_const fmt c = Format.pp_print_string fmt (const_repr c)

let pp_code fmt c =
  let buf = Buffer.create 4096 in
  render buf c;
  Format.pp_print_string fmt (Buffer.contents buf)

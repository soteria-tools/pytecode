(* Core types of the definitional interpreter.

   Everything is pure: the whole interpreter state is one immutable record
   threaded explicitly, mutable Python entities live in a persistent heap
   keyed by integer addresses, and "mutation" is a functional map update. *)

module Phir = Pytecode.Phir
module Ast = Pytecode.Ast
module Int_map = Map.Make (Int)

(* ------------------------------------------------------------------ *)
(* Values                                                              *)
(* ------------------------------------------------------------------ *)

(* Immutable values are immediate; every mutable Python entity (and
   everything with observable identity) is a [Ref] into the heap. *)
type value =
  | None_
  | Bool of bool
  | Int of Z.t
  | Float of float
  (* ref: Language Reference 3.2.4.3 Complex (numbers.Complex) *)
  | Complex of float * float (* real, imaginary *)
  | Str of string (* UTF-8 *)
  | Bytes of string (* an immutable byte string (raw bytes) *)
  | Tuple of value list
  | Slice of value * value * value (* start, stop, step (None_ = absent) *)
  | Range of Z.t * Z.t * Z.t (* start, stop, step *)
  | Builtin of string (* builtin function, by name *)
  | Bound of value * value (* callable, self — a bound method *)
  | Code_obj of Phir.code (* a code constant (consumed by Make_function) *)
  | Ref of int (* heap address *)
  | Null (* CPython's NULL stack sentinel *)
  (* ref: Language Reference 3.2.2 NotImplemented *)
  | Not_implemented (* the NotImplemented singleton *)
  (* ref: Language Reference 3.2.3 Ellipsis (the [...] literal) *)
  | Ellipsis

and obj =
  | List of value list
  | Dict of (value * value) list (* insertion-ordered *)
  | Set of value list (* insertion-ordered *)
  | Frozenset of value list (* immutable, hashable set (insertion-ordered) *)
  | Bytearray of string (* a mutable byte array *)
  | Cell of value option (* closure cell; None = empty *)
  | Func of func
  | Class of cls
  | Instance of {
      cls : int;
      dict : int; (* a heap Dict with Str keys *)
      native : value;
          (* for a subclass of a built-in type, the underlying payload value
             (a Ref to a Dict/List/…, or an immediate); None_ otherwise *)
    }
  | Gen of gen
  | Super of { cls : int; self : value } (* bound super object *)
  | Property of { fget : value; fset : value option }
  | Classmethod of value
  | Staticmethod of value
  | Iter of iter
  (* ref: 3.3.5 — types.GenericAlias, e.g. list[int] (origin class + args) *)
  | Generic_alias of { ga_origin : value; ga_args : value list }
  (* ref: 6.7 (PEP 604) — types.UnionType, e.g. int | str (member types) *)
  | Union_type of value list
  (* ref: 7.14 (PEP 695) — typing.TypeAliasType from a `type X = ...` statement;
     ta_value is the lazily-evaluated value-computing function *)
  | Type_alias of {
      ta_name : string;
      ta_value : value;
      ta_type_params : value; (* a tuple of TypeVars, () when non-generic *)
    }
  (* ref: 8.10 (PEP 695) — a typing.TypeVar from a `[T]` type-parameter list.
     bound/constraints are lazily-evaluated functions (None_ when absent). *)
  | Typevar of { tv_name : string; tv_bound : value; tv_constraints : value }

and func = {
  code : Phir.code;
  globals : int; (* module globals Dict address *)
  defaults : value list;
  kwdefaults : (value * value) list; (* Str name -> default *)
  closure : value list; (* Cell refs *)
  fdict : int; (* function attributes (f.x = 1), a heap Dict *)
}

and cls = {
  cname : string;
  bases : int list; (* class addresses *)
  mro : int list; (* C3 linearization, self first *)
  cdict : int; (* class namespace, a heap Dict with Str keys *)
  builtin : string option; (* Some "int" for builtin types like int/str/... *)
  meta : int option;
      (* metaclass address; None means the default [type] (ref: 3.3.3) *)
}

and gen = {
  gframe : frame option; (* None once exhausted *)
  gstarted : bool;
  gkind : [ `Gen | `Coroutine | `Async_gen ];
}

(* Builtin iterators. Each step is a functional heap update. *)
and iter =
  | It_list of int * int (* list address (read live), next index *)
  | It_seq of value list (* remaining items: tuples, dict-key snapshots, ... *)
  | It_str of string * int (* UTF-8 byte offset *)
  | It_range of Z.t * Z.t * Z.t (* next, stop, step *)
  | It_zip of value list (* component iterators *)
  | It_map of value * value list (* function, component iterators *)
  | It_filter of value * value (* predicate (or None_), iterator *)
  | It_enum of Z.t * value (* next index, iterator *)

and frame = {
  code : Phir.code;
  globals : int; (* module globals Dict address *)
  ns : int; (* namespace Dict for Name ops (= globals except class bodies) *)
  slots : value Int_map.t; (* localsplus; absent = unbound *)
  stack : value list; (* operand stack, top first *)
  idx : int; (* next instruction *)
  closure : value list; (* the function's closure cells (Copy_free_vars) *)
}

(* ------------------------------------------------------------------ *)
(* Interpreter state                                                   *)
(* ------------------------------------------------------------------ *)

type state = {
  heap : obj Int_map.t;
  next : int; (* next free address *)
  out : string list; (* program stdout, reversed chunks *)
  cur_exc : value; (* "current exception" (sys.exc_info), None_ if none *)
  builtins : int; (* address of the builtins Dict *)
}

(* The error monad: [Error] carries a raised Python exception object.
   State changes made before a raise persist (Python does not roll back). *)
type 'a r = ('a * state, value * state) result

let ( let* ) = Result.bind
let return st v : 'a r = Ok (v, st)

let alloc st o : value * state =
  ( Ref st.next,
    { st with heap = Int_map.add st.next o st.heap; next = st.next + 1 } )

let heap_get st addr = Int_map.find addr st.heap
let heap_set st addr o = { st with heap = Int_map.add addr o st.heap }
let deref st = function Ref a -> Some (heap_get st a) | _ -> None
let output st s = { st with out = s :: st.out }
let collected_output st = String.concat "" (List.rev st.out)

(* ------------------------------------------------------------------ *)
(* Small pure helpers                                                  *)
(* ------------------------------------------------------------------ *)

let type_name st (v : value) =
  match v with
  | None_ -> "NoneType"
  | Bool _ -> "bool"
  | Int _ -> "int"
  | Float _ -> "float"
  | Complex _ -> "complex"
  | Str _ -> "str"
  | Bytes _ -> "bytes"
  | Tuple _ -> "tuple"
  | Slice _ -> "slice"
  | Range _ -> "range"
  | Builtin _ -> "builtin_function_or_method"
  | Bound _ -> "method"
  | Code_obj _ -> "code"
  | Null -> "<NULL>"
  | Not_implemented -> "NotImplementedType"
  | Ellipsis -> "ellipsis"
  | Ref a -> (
      match heap_get st a with
      | List _ -> "list"
      | Dict _ -> "dict"
      | Set _ -> "set"
      | Frozenset _ -> "frozenset"
      | Bytearray _ -> "bytearray"
      | Cell _ -> "cell"
      | Func _ -> "function"
      | Class { cname; _ } -> cname (* the *metatype* name is "type" *)
      | Instance { cls; _ } -> (
          match heap_get st cls with
          | Class { cname; _ } -> cname
          | _ -> "object")
      | Gen { gkind = `Gen; _ } -> "generator"
      | Gen { gkind = `Coroutine; _ } -> "coroutine"
      | Gen { gkind = `Async_gen; _ } -> "async_generator"
      | Super _ -> "super"
      | Property _ -> "property"
      | Classmethod _ -> "classmethod"
      | Staticmethod _ -> "staticmethod"
      | Iter _ -> "iterator"
      | Generic_alias _ -> "types.GenericAlias"
      | Union_type _ -> "types.UnionType"
      | Type_alias _ -> "typing.TypeAliasType"
      | Typevar _ -> "typing.TypeVar")

(* Python float repr: the shortest decimal digits that round-trip, laid out
   in fixed notation unless the decimal exponent is < -4 or >= 16. *)
let float_repr f =
  if Float.is_nan f then "nan"
  else if f = Float.infinity then "inf"
  else if f = Float.neg_infinity then "-inf"
  else if f = 0. then if 1. /. f < 0. then "-0.0" else "0.0"
  else
    (* shortest scientific form "d.dddde±XX" that round-trips *)
    let sci =
      let rec try_prec p =
        if p >= 17 then Printf.sprintf "%.16e" f
        else
          let s = Printf.sprintf "%.*e" p f in
          if float_of_string s = f then s else try_prec (p + 1)
      in
      try_prec 0
    in
    let mant, exp =
      match String.split_on_char 'e' sci with
      | [ m; e ] -> (m, int_of_string e)
      | _ -> (sci, 0)
    in
    let neg = String.length mant > 0 && mant.[0] = '-' in
    let digits =
      String.to_seq mant
      |> Seq.filter (fun c -> c >= '0' && c <= '9')
      |> String.of_seq
    in
    (* strip trailing zeros of the mantissa (keep at least one digit) *)
    let digits =
      let n = String.length digits in
      let rec last i =
        if i > 1 && digits.[i - 1] = '0' then last (i - 1) else i
      in
      String.sub digits 0 (last n)
    in
    let sign = if neg then "-" else "" in
    let nd = String.length digits in
    if exp < -4 || exp >= 16 then
      (* scientific: d[.ddd]e±XX with at least two exponent digits *)
      let m =
        if nd = 1 then digits
        else String.sub digits 0 1 ^ "." ^ String.sub digits 1 (nd - 1)
      in
      Printf.sprintf "%s%se%s%02d" sign m
        (if exp < 0 then "-" else "+")
        (abs exp)
    else if exp >= 0 then
      let int_digits =
        if nd > exp + 1 then String.sub digits 0 (exp + 1)
        else digits ^ String.make (exp + 1 - nd) '0'
      in
      let frac =
        if nd > exp + 1 then String.sub digits (exp + 1) (nd - exp - 1) else ""
      in
      sign ^ int_digits ^ "." ^ if frac = "" then "0" else frac
    else sign ^ "0." ^ String.make (-exp - 1) '0' ^ digits

(* ref: Language Reference 3.2.4 numbers.Number (repr is a valid base-10 literal,
   no superfluous zeros) and 3.2.4.3 Complex.
   A complex part is the float repr without a forced trailing ".0" (CPython
   formats complex components in repr mode without the ADD_DOT_0 flag): 2.0->"2",
   -0.0->"-0", 1.5->"1.5", inf->"inf". *)
let complex_part f =
  let s = float_repr f in
  let n = String.length s in
  if n >= 2 && String.sub s (n - 2) 2 = ".0" then String.sub s 0 (n - 2) else s

(* Python complex repr: if the real part is positive zero, show only the
   imaginary part ("3j"); otherwise parenthesise and show both with the
   imaginary sign ("(1+2j)", "(-0-3j)"). *)
let complex_repr re im =
  if re = 0. && Float.copy_sign 1. re = 1. then complex_part im ^ "j"
  else
    let im_s = complex_part im in
    let sign = if String.length im_s > 0 && im_s.[0] = '-' then "" else "+" in
    "(" ^ complex_part re ^ sign ^ im_s ^ "j)"

(* Python str repr: single quotes, unless the string contains a single
   quote and no double quote. *)
let str_repr s =
  let has c = String.contains s c in
  let quote = if has '\'' && not (has '"') then '"' else '\'' in
  let escape c =
    match c with
    | '\\' -> "\\\\"
    | '\n' -> "\\n"
    | '\r' -> "\\r"
    | '\t' -> "\\t"
    | c when c = quote -> Printf.sprintf "\\%c" c
    | c when Char.code c < 32 || Char.code c = 127 ->
        Printf.sprintf "\\x%02x" (Char.code c)
    | c -> String.make 1 c
  in
  let qs = String.make 1 quote in
  qs ^ String.concat "" (List.map escape (List.of_seq (String.to_seq s))) ^ qs

(* Python bytes repr: a b-prefixed quoted form; printable ASCII is shown
   literally, \t \n \r are named, and every other byte (< 32, 127, or >= 128)
   is shown as \xXX. Quote selection mirrors str_repr. *)
let bytes_repr s =
  let has c = String.contains s c in
  let quote = if has '\'' && not (has '"') then '"' else '\'' in
  let escape c =
    match c with
    | '\\' -> "\\\\"
    | '\t' -> "\\t"
    | '\n' -> "\\n"
    | '\r' -> "\\r"
    | c when c = quote -> Printf.sprintf "\\%c" c
    | c when Char.code c < 32 || Char.code c >= 127 ->
        Printf.sprintf "\\x%02x" (Char.code c)
    | c -> String.make 1 c
  in
  let qs = String.make 1 quote in
  "b" ^ qs
  ^ String.concat "" (List.map escape (List.of_seq (String.to_seq s)))
  ^ qs

(* ------------------------------------------------------------------ *)
(* UTF-8 (Python strings are sequences of code points)                 *)
(* ------------------------------------------------------------------ *)

let utf8_seq_len c =
  let n = Char.code c in
  if n < 0x80 then 1 else if n < 0xE0 then 2 else if n < 0xF0 then 3 else 4

let utf8_length s =
  let rec go i acc =
    if i >= String.length s then acc else go (i + utf8_seq_len s.[i]) (acc + 1)
  in
  go 0 0

(* Byte offset of code point [n] (counting from offset [i]). *)
let rec utf8_offset s i n =
  if n = 0 then i else utf8_offset s (i + utf8_seq_len s.[i]) (n - 1)

let utf8_sub s ~pos ~len =
  let start = utf8_offset s 0 pos in
  let stop = utf8_offset s start len in
  String.sub s start (stop - start)

let utf8_decode_at s i =
  let n = utf8_seq_len s.[i] in
  let b k = Char.code s.[i + k] in
  let cp =
    match n with
    | 1 -> b 0
    | 2 -> ((b 0 land 0x1F) lsl 6) lor (b 1 land 0x3F)
    | 3 ->
        ((b 0 land 0x0F) lsl 12) lor ((b 1 land 0x3F) lsl 6) lor (b 2 land 0x3F)
    | _ ->
        ((b 0 land 0x07) lsl 18)
        lor ((b 1 land 0x3F) lsl 12)
        lor ((b 2 land 0x3F) lsl 6)
        lor (b 3 land 0x3F)
  in
  (cp, n)

let utf8_encode cp =
  let bytes =
    if cp < 0x80 then [ cp ]
    else if cp < 0x800 then [ 0xC0 lor (cp lsr 6); 0x80 lor (cp land 0x3F) ]
    else if cp < 0x10000 then
      [
        0xE0 lor (cp lsr 12);
        0x80 lor ((cp lsr 6) land 0x3F);
        0x80 lor (cp land 0x3F);
      ]
    else
      [
        0xF0 lor (cp lsr 18);
        0x80 lor ((cp lsr 12) land 0x3F);
        0x80 lor ((cp lsr 6) land 0x3F);
        0x80 lor (cp land 0x3F);
      ]
  in
  String.concat "" (List.map (fun b -> String.make 1 (Char.chr b)) bytes)

let utf8_chars s =
  let rec go i acc =
    if i >= String.length s then List.rev acc
    else
      let n = utf8_seq_len s.[i] in
      go (i + n) (String.sub s i n :: acc)
  in
  go 0 []

(* ------------------------------------------------------------------ *)
(* Integer helpers (Python floor-division semantics)                   *)
(* ------------------------------------------------------------------ *)

let z_floordiv a b = Z.fdiv a b
let z_mod a b = Z.sub a (Z.mul (Z.fdiv a b) b)

let py_float_mod a b =
  let r = Float.rem a b in
  if r <> 0. && r < 0. <> (b < 0.) then r +. b else r

let py_float_floordiv a b = Float.floor (a /. b)

(* ------------------------------------------------------------------ *)
(* Shared pure helpers (no recursion into the interpreter knot)        *)
(* ------------------------------------------------------------------ *)

let addr = function Ref a -> a | _ -> invalid_arg "addr"

let cls_of st a =
  match heap_get st a with Class c -> c | _ -> invalid_arg "cls_of"

let dict_pairs st a =
  match heap_get st a with Dict ps -> ps | _ -> invalid_arg "dict_pairs"

(* monadic list combinators (state threaded, short-circuit on Error) *)
let rec map_m st f = function
  | [] -> Ok ([], st)
  | x :: xs ->
      let* y, st = f st x in
      let* ys, st = map_m st f xs in
      Ok (y :: ys, st)

let rec fold_m st f acc = function
  | [] -> Ok (acc, st)
  | x :: xs ->
      let* acc, st = f st acc x in
      fold_m st f acc xs

let rec take n = function
  | xs when n = 0 -> ([], xs)
  | x :: xs ->
      let a, b = take (n - 1) xs in
      (x :: a, b)
  | [] -> invalid_arg "take"

let rec drop n xs =
  if n <= 0 then xs else match xs with [] -> [] | _ :: t -> drop (n - 1) t

(* ---------- numeric coercions (pure) ------------------------------- *)

let as_z = function
  | Int z -> Some z
  | Bool b -> Some (if b then Z.one else Z.zero)
  | _ -> None

let as_float = function
  | Float f -> Some f
  | Int z -> Some (Z.to_float z)
  | Bool b -> Some (if b then 1. else 0.)
  | _ -> None

let is_number v = as_float v <> None
let is_complex = function Complex _ -> true | _ -> false

(* any numeric operand, including complex (real types embed as imag 0) *)
let is_numeric v = is_number v || is_complex v

let as_complex = function
  | Complex (re, im) -> Some (re, im)
  | v -> ( match as_float v with Some f -> Some (f, 0.) | None -> None)

(* ref: 3.2.5 — the raw bytes of a bytes-like object (bytes or bytearray); used
   for cross-type comparison, concatenation and membership. *)
let as_bytes st v =
  match v with
  | Bytes s -> Some s
  | Ref a -> ( match heap_get st a with Bytearray s -> Some s | _ -> None)
  | _ -> None

(* ref: 6.10.1 Value comparisons (numbers compared mathematically) and 3.2.4.2
   Real — IEEE 754 semantics. Numeric equality and ordering; assume both are
   numbers. *)
let num_eq a b =
  match (as_z a, as_z b) with
  | Some x, Some y -> Z.equal x y
  | _ -> Option.get (as_float a) = Option.get (as_float b)

let num_lt a b =
  match (as_z a, as_z b) with
  | Some x, Some y -> Z.lt x y
  | _ -> Option.get (as_float a) < Option.get (as_float b)

let num_le a b =
  match (as_z a, as_z b) with
  | Some x, Some y -> Z.leq x y
  | _ -> Option.get (as_float a) <= Option.get (as_float b)

(* Object identity, matching the `is` operator: heap objects by address, other
   immediates structurally. *)
let val_identical a b =
  match (a, b) with
  | Ref x, Ref y -> x = y
  | Null, Null -> true
  | Ref _, _ | _, Ref _ -> false
  | x, y -> x = y

let is_instance_value st = function
  | Ref a -> ( match heap_get st a with Instance _ -> true | _ -> false)
  | _ -> false

(* the underlying payload of a built-in-subclass instance, if any *)
let native_of st v : value option =
  match deref st v with
  | Some (Instance { native; _ }) when native <> None_ -> Some native
  | _ -> None

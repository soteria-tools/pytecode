(* A definitional, purely functional interpreter for Phir.

   Optimized for readability and simplicity, not speed. The whole
   interpreter state is the immutable [Value.state] record, threaded
   explicitly; raised Python exceptions travel in the [Error] case of
   ['a Value.r]. There is no mutable state anywhere. *)

open Value
module Phir = Pytecode.Phir
module Ast = Pytecode.Ast

type frame_outcome = Returned of value | Yielded of value * frame

(* What executing one instruction does to the frame. *)
type istep = Next of frame | Goto of frame * int | Fin of frame_outcome

let addr = function Ref a -> a | _ -> invalid_arg "addr"
let push f v = { f with stack = v :: f.stack }

let pop f =
  match f.stack with
  | v :: rest -> (v, { f with stack = rest })
  | [] -> invalid_arg "pop: empty operand stack"

let advance f = { f with idx = f.idx + 1 }

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

(* ------------------------------------------------------------------ *)
(* Boot: builtin classes and the builtins namespace                    *)
(* ------------------------------------------------------------------ *)

(* (name, parent); parents must appear first. *)
(* ref: Built-in Exceptions — the standard exception hierarchy (3.13).
   ExceptionGroup/BaseExceptionGroup are created separately (multiple bases). *)
let exception_tree =
  [
    ("BaseException", None);
    ("GeneratorExit", Some "BaseException");
    ("KeyboardInterrupt", Some "BaseException");
    ("SystemExit", Some "BaseException");
    ("Exception", Some "BaseException");
    ("ArithmeticError", Some "Exception");
    ("FloatingPointError", Some "ArithmeticError");
    ("OverflowError", Some "ArithmeticError");
    ("ZeroDivisionError", Some "ArithmeticError");
    ("AssertionError", Some "Exception");
    ("AttributeError", Some "Exception");
    ("BufferError", Some "Exception");
    ("EOFError", Some "Exception");
    ("ImportError", Some "Exception");
    ("ModuleNotFoundError", Some "ImportError");
    ("LookupError", Some "Exception");
    ("IndexError", Some "LookupError");
    ("KeyError", Some "LookupError");
    ("MemoryError", Some "Exception");
    ("NameError", Some "Exception");
    ("UnboundLocalError", Some "NameError");
    ("OSError", Some "Exception");
    ("BlockingIOError", Some "OSError");
    ("ChildProcessError", Some "OSError");
    ("ConnectionError", Some "OSError");
    ("BrokenPipeError", Some "ConnectionError");
    ("ConnectionAbortedError", Some "ConnectionError");
    ("ConnectionRefusedError", Some "ConnectionError");
    ("ConnectionResetError", Some "ConnectionError");
    ("FileExistsError", Some "OSError");
    ("FileNotFoundError", Some "OSError");
    ("InterruptedError", Some "OSError");
    ("IsADirectoryError", Some "OSError");
    ("NotADirectoryError", Some "OSError");
    ("PermissionError", Some "OSError");
    ("ProcessLookupError", Some "OSError");
    ("TimeoutError", Some "OSError");
    ("ReferenceError", Some "Exception");
    ("RuntimeError", Some "Exception");
    ("NotImplementedError", Some "RuntimeError");
    ("RecursionError", Some "RuntimeError");
    ("StopAsyncIteration", Some "Exception");
    ("StopIteration", Some "Exception");
    ("SyntaxError", Some "Exception");
    ("IndentationError", Some "SyntaxError");
    ("TabError", Some "IndentationError");
    ("SystemError", Some "Exception");
    ("TypeError", Some "Exception");
    ("ValueError", Some "Exception");
    ("UnicodeError", Some "ValueError");
    ("UnicodeDecodeError", Some "UnicodeError");
    ("UnicodeEncodeError", Some "UnicodeError");
    ("UnicodeTranslateError", Some "UnicodeError");
    ("Warning", Some "Exception");
    ("BytesWarning", Some "Warning");
    ("DeprecationWarning", Some "Warning");
    ("EncodingWarning", Some "Warning");
    ("FutureWarning", Some "Warning");
    ("ImportWarning", Some "Warning");
    ("PendingDeprecationWarning", Some "Warning");
    ("ResourceWarning", Some "Warning");
    ("RuntimeWarning", Some "Warning");
    ("SyntaxWarning", Some "Warning");
    ("UnicodeWarning", Some "Warning");
    ("UserWarning", Some "Warning");
  ]

let builtin_functions =
  [
    "print";
    "len";
    "repr";
    "ascii";
    "hash";
    "abs";
    "min";
    "max";
    "sum";
    "sorted";
    "any";
    "all";
    "divmod";
    "pow";
    "round";
    "hex";
    "bin";
    "oct";
    "chr";
    "ord";
    "callable";
    "getattr";
    "setattr";
    "hasattr";
    "dir";
    "reversed";
    "delattr";
    "isinstance";
    "issubclass";
    "iter";
    "next";
    "enumerate";
    "zip";
    "map";
    "filter";
    "vars";
    "format";
    "__build_class__";
    "super";
  ]

let builtin_types =
  (* (name, base, tag) — base must appear first; object's base is itself. *)
  [
    ("object", "object", "object");
    ("type", "object", "type");
    ("int", "object", "int");
    ("bool", "int", "bool");
    ("float", "object", "float");
    ("complex", "object", "complex");
    (* ref: 3.2.4.3 Complex *)
    ("str", "object", "str");
    ("bytes", "object", "bytes");
    (* ref: 3.2.5.1 Bytes *)
    ("bytearray", "object", "bytearray");
    (* ref: 3.2.5.2 Bytearray *)
    ("list", "object", "list");
    ("dict", "object", "dict");
    ("tuple", "object", "tuple");
    ("set", "object", "set");
    ("frozenset", "object", "frozenset");
    (* ref: 3.2.6 Set types *)
    ("range", "object", "range");
    ("slice", "object", "slice");
    (* ref: 3.2.13 Internal types — slice objects *)
    ("property", "object", "property");
    ("classmethod", "object", "classmethod");
    ("staticmethod", "object", "staticmethod");
    (* ref: 3.2.1 None / 3.2.2 NotImplemented / 3.2.3 Ellipsis — the singleton
       types, so type(None)/type(...)/type(NotImplemented) resolve *)
    ("NoneType", "object", "NoneType");
    ("ellipsis", "object", "ellipsis");
    ("NotImplementedType", "object", "NotImplementedType");
    (* ref: 3.3.5 generic aliases / 6.7 union types / 7.14 type aliases — the
       types whose __name__ these objects report (type(list[int]).__name__ ==
       "GenericAlias", etc.) *)
    ("GenericAlias", "object", "GenericAlias");
    ("UnionType", "object", "UnionType");
    ("TypeAliasType", "object", "TypeAliasType");
    ("TypeVar", "object", "TypeVar");
  ]

let new_class st ?builtin ?mro_tail ~bases ~dict_pairs cname =
  let dict_ref, st = alloc st (Dict dict_pairs) in
  let mro_tail =
    match mro_tail with
    | Some m -> m
    | None -> (
        match bases with
        | [] -> []
        | b :: _ -> ( match heap_get st b with Class c -> c.mro | _ -> []))
  in
  let caddr = st.next in
  let _, st =
    alloc st
      (Class
         {
           cname;
           bases;
           mro = caddr :: mro_tail;
           cdict = addr dict_ref;
           builtin;
           meta = None;
         })
  in
  (caddr, st)

let boot () : state =
  let st =
    { heap = Int_map.empty; next = 0; out = []; cur_exc = None_; builtins = 0 }
  in
  (* Builtin types. [object]'s mro is just itself. *)
  let types, st =
    List.fold_left
      (fun (acc, st) (name, base, tag) ->
        let bases = if name = "object" then [] else [ List.assoc base acc ] in
        let dict_pairs =
          if name = "object" then
            (* ref: 3.3.1 (__init__/__new__) and 3.3.2 Customizing attribute
               access (__getattribute__/__setattr__/__delattr__) — object's
               defaults, which user overrides delegate back to *)
            [
              (Str "__init__", Builtin "object.__init__");
              (Str "__new__", Builtin "object.__new__");
              (Str "__getattribute__", Builtin "object.__getattribute__");
              (Str "__setattr__", Builtin "object.__setattr__");
              (Str "__delattr__", Builtin "object.__delattr__");
              (Str "__init_subclass__", Builtin "object.__init_subclass__");
            ]
          else if name = "type" then
            (* ref: 3.3.3 — the default metaclass [type] supplies the class
               builder (__new__), a no-op __init__, and __call__ (which
               instantiates the class). User metaclasses inherit these and
               reach them through super(). *)
            [
              (Str "__new__", Builtin "type.__new__");
              (Str "__init__", Builtin "type.__init__");
              (Str "__call__", Builtin "type.__call__");
            ]
          else if name = "list" || name = "dict" || name = "set" then
            (* ref: 3.2 — a mutable container's __init__ (re)fills the payload;
               subclasses reach it via super().__init__(iterable) *)
            [ (Str "__init__", Builtin (name ^ ".__init__")) ]
          else []
        in
        let caddr, st = new_class st ~builtin:tag ~bases ~dict_pairs name in
        ((name, caddr) :: acc, st))
      ([], st) builtin_types
  in
  let object_addr = List.assoc "object" types in
  (* Exception classes. *)
  let excs, st =
    List.fold_left
      (fun (acc, st) (name, parent) ->
        let bases =
          match parent with
          | None -> [ object_addr ]
          | Some p -> [ List.assoc p acc ]
        in
        let dict_pairs =
          if name = "BaseException" then
            [
              (Str "__init__", Builtin "BaseException.__init__");
              (Str "__str__", Builtin "BaseException.__str__");
              (Str "add_note", Builtin "BaseException.add_note");
              (Str "with_traceback", Builtin "BaseException.with_traceback");
            ]
          else if name = "KeyError" then
            [ (Str "__str__", Builtin "KeyError.__str__") ]
          else []
        in
        let caddr, st = new_class st ~bases ~dict_pairs name in
        ((name, caddr) :: acc, st))
      ([], st) exception_tree
  in
  (* ref: 8.4 — exception groups. BaseExceptionGroup derives from
     BaseException; ExceptionGroup additionally derives from Exception (so it is
     caught by `except Exception`). The MRO is supplied explicitly because
     new_class does not C3-merge multiple bases. *)
  let excs, st =
    let base_exc = List.assoc "BaseException" excs in
    let exc = List.assoc "Exception" excs in
    let group_dict =
      [
        (Str "__init__", Builtin "BaseExceptionGroup.__init__");
        (Str "__str__", Builtin "BaseExceptionGroup.__str__");
        (Str "split", Builtin "BaseExceptionGroup.split");
        (Str "subgroup", Builtin "BaseExceptionGroup.subgroup");
        (Str "derive", Builtin "BaseExceptionGroup.derive");
      ]
    in
    let beg, st =
      new_class st ~bases:[ base_exc ] ~dict_pairs:group_dict
        "BaseExceptionGroup"
    in
    let eg, st =
      new_class st ~bases:[ beg; exc ]
        ~mro_tail:[ beg; exc; base_exc; object_addr ]
        ~dict_pairs:group_dict "ExceptionGroup"
    in
    ( (* registered before the rest so `excs` lookups still resolve *)
      ("ExceptionGroup", eg) :: ("BaseExceptionGroup", beg) :: excs,
      st )
  in
  let entries =
    List.map (fun (n, a) -> (Str n, Ref a)) (types @ excs)
    @ List.map (fun n -> (Str n, Builtin n)) builtin_functions
      (* ref: 3.2.2 NotImplemented / 3.2.3 Ellipsis — the built-in names *)
    @ [ (Str "NotImplemented", Not_implemented); (Str "Ellipsis", Ellipsis) ]
  in
  let builtins_ref, st = alloc st (Dict entries) in
  { st with builtins = addr builtins_ref }

(* Pure lookup in the builtins namespace (keys are all Str). *)
let lookup_builtin st name =
  match heap_get st st.builtins with
  | Dict pairs ->
      List.find_map
        (function Str k, v when k = name -> Some v | _ -> None)
        pairs
  | _ -> None

let builtin_class_addr st name =
  match lookup_builtin st name with
  | Some (Ref a) -> a
  | _ -> invalid_arg ("boot class missing: " ^ name)

(* Python slice arithmetic: the indices selected by [start:stop:step] on a
   sequence of length [len], as a list. *)
(* normalize a slice over [len] to (start, stop, step) ints — CPython's
   slice.indices(len) algorithm *)
let slice_bounds ~len start stop step =
  let z = Z.to_int in
  let step = match step with None -> 1 | Some s -> z s in
  let clamp lo hi v = if v < lo then lo else if v > hi then hi else v in
  let norm dflt_fwd dflt_bwd = function
    | None -> if step > 0 then dflt_fwd else dflt_bwd
    | Some i ->
        let i = z i in
        let i = if i < 0 then i + len else i in
        if step > 0 then clamp 0 len i else clamp (-1) (len - 1) i
  in
  (norm 0 (len - 1) start, norm len (-1) stop, step)

let slice_indices ~len start stop step =
  let z = Z.to_int in
  let step = match step with None -> 1 | Some s -> z s in
  let clamp lo hi v = if v < lo then lo else if v > hi then hi else v in
  let norm dflt_fwd dflt_bwd = function
    | None -> if step > 0 then dflt_fwd else dflt_bwd
    | Some i ->
        let i = z i in
        let i = if i < 0 then i + len else i in
        if step > 0 then clamp 0 len i else clamp (-1) (len - 1) i
  in
  let start = norm 0 (len - 1) start in
  let stop = norm len (-1) stop in
  let rec go i acc =
    if (step > 0 && i >= stop) || (step < 0 && i <= stop) then List.rev acc
    else go (i + step) (i :: acc)
  in
  go start []

let list_set_nth xs i v = List.mapi (fun j x -> if j = i then v else x) xs
let list_del_nth xs i = List.filteri (fun j _ -> j <> i) xs

(* ref: 3.3.1 __hash__ — CPython reduces integer (and integral float) hashes
   modulo 2**61-1, keeping the sign, with the special case hash(-1) == -2. *)
let hash_modulus = Z.sub (Z.shift_left Z.one 61) Z.one

let int_hash z =
  let h = Z.rem z hash_modulus in
  if Z.equal h Z.minus_one then Z.of_int (-2) else h

(* Methods of builtin types, dispatched as [Builtin "str.upper"] etc. *)
let str_methods =
  [
    "upper";
    "lower";
    "strip";
    "lstrip";
    "rstrip";
    "split";
    "join";
    "replace";
    "startswith";
    "endswith";
    "find";
    "index";
    "isdigit";
    "isalpha";
    "isupper";
    "islower";
    "title";
    "center";
    "zfill";
    "ljust";
    "rjust";
    "partition";
    "rpartition";
    "count";
    "splitlines";
    "capitalize";
    "swapcase";
    "format";
    "removeprefix";
    "removesuffix";
    "rfind";
    "rindex";
    "istitle";
    "isspace";
    "isalnum";
    "isnumeric";
    "isdecimal";
    "isidentifier";
    "translate";
    "casefold";
    "expandtabs";
    "encode";
  ]

let list_methods =
  [
    "append";
    "extend";
    "insert";
    "pop";
    "remove";
    "index";
    "count";
    "reverse";
    "sort";
    "copy";
    "clear";
  ]

let dict_methods =
  [
    "get";
    "keys";
    "values";
    "items";
    "pop";
    "setdefault";
    "update";
    "copy";
    "clear";
    "popitem";
  ]

let set_methods =
  [
    "add";
    "discard";
    "remove";
    "union";
    "copy";
    "intersection";
    "difference";
    "symmetric_difference";
    "issubset";
    "issuperset";
    "isdisjoint";
    "clear";
    "pop";
    "update";
  ]

let tuple_methods = [ "count"; "index" ]

let int_methods =
  [
    "bit_length"; "bit_count"; "to_bytes"; "from_bytes"; "conjugate"; "__add__";
  ]

let float_methods = [ "is_integer"; "conjugate"; "as_integer_ratio" ]

let bytes_methods =
  [
    "decode";
    "upper";
    "lower";
    "split";
    "replace";
    "startswith";
    "endswith";
    "find";
    "index";
    "rfind";
    "count";
    "strip";
    "lstrip";
    "rstrip";
    "hex";
    "join";
  ]

let bytearray_methods = [ "decode"; "append"; "extend" ]
let complex_methods = [ "conjugate" ]
let gen_methods = [ "send"; "close"; "throw"; "__next__" ]

(* ------------------------------------------------------------------ *)
(* The interpreter proper: one big recursive knot                      *)
(* ------------------------------------------------------------------ *)

let rec make_exc st cls_addr (args : value list) : value * state =
  let dict_ref, st = alloc st (Dict [ (Str "args", Tuple args) ]) in
  alloc st (Instance { cls = cls_addr; dict = addr dict_ref; native = None_ })

and raise_py : 'a. state -> string -> string -> 'a r =
 fun st clsname msg ->
  let cls = builtin_class_addr st clsname in
  let args = if msg = "" then [] else [ Str msg ] in
  let exc, st = make_exc st cls args in
  Error (exc, st)

and unsupported : 'a. state -> string -> 'a r =
 fun st what -> raise_py st "RuntimeError" ("pytecode unsupported: " ^ what)

(* KeyError carries the missing *key* (its [__str__] reprs it). *)
and raise_key : 'a. state -> value -> 'a r =
 fun st key ->
  let cls = builtin_class_addr st "KeyError" in
  let exc, st = make_exc st cls [ key ] in
  Error (exc, st)

(* ---------- dictionaries (insertion-ordered association lists) ----- *)

and dict_pairs st a =
  match heap_get st a with Dict ps -> ps | _ -> invalid_arg "dict_pairs"

and dict_find st pairs key : value option r =
  match pairs with
  | [] -> Ok (None, st)
  | (k, v) :: rest ->
      let* eq, st = py_eq st k key in
      if eq then Ok (Some v, st) else dict_find st rest key

(* ref: 3.2.7.1 Dictionaries (keys must be hashable; mutable types rejected) and
   3.2.6 Set types (same immutability rules for elements). The by-value-compared
   mutable containers (list/dict/set) are unhashable. *)
and check_hashable st key : unit r =
  match deref st key with
  | Some (List _) -> raise_py st "TypeError" "unhashable type: 'list'"
  | Some (Dict _) -> raise_py st "TypeError" "unhashable type: 'dict'"
  | Some (Set _) -> raise_py st "TypeError" "unhashable type: 'set'"
  | Some (Bytearray _) -> raise_py st "TypeError" "unhashable type: 'bytearray'"
  | Some (Instance { cls; _ }) -> (
      (* ref: 3.3.1 __hash__ — a class that overrides __eq__ without defining
         __hash__ (or sets __hash__ = None) has unhashable instances *)
      let unhashable st =
        raise_py st "TypeError"
          (Printf.sprintf "unhashable type: '%s'" (type_name st key))
      in
      let* h, st = type_lookup st cls "__hash__" in
      match h with
      | Some None_ -> unhashable st
      | Some _ -> Ok ((), st)
      | None -> (
          let* eq, st = type_lookup st cls "__eq__" in
          match eq with Some _ -> unhashable st | None -> Ok ((), st)))
  | _ -> Ok ((), st)

and dict_set st a key v : unit r =
  let* (), st = check_hashable st key in
  let rec go st acc = function
    | [] -> Ok (List.rev_append acc [ (key, v) ], st)
    | (k, v0) :: rest ->
        let* eq, st = py_eq st k key in
        if eq then Ok (List.rev_append acc ((k, v) :: rest), st)
        else go st ((k, v0) :: acc) rest
  in
  let* pairs, st = go st [] (dict_pairs st a) in
  Ok ((), heap_set st a (Dict pairs))

and dict_del st a key : bool r =
  let rec go st acc = function
    | [] -> Ok (None, st)
    | (k, v0) :: rest ->
        let* eq, st = py_eq st k key in
        if eq then Ok (Some (List.rev_append acc rest), st)
        else go st ((k, v0) :: acc) rest
  in
  let* removed, st = go st [] (dict_pairs st a) in
  match removed with
  | Some pairs -> Ok (true, heap_set st a (Dict pairs))
  | None -> Ok (false, st)

and dget st a key = dict_find st (dict_pairs st a) key

(* ---------- numbers ----------------------------------------------- *)

and as_z = function
  | Int z -> Some z
  | Bool b -> Some (if b then Z.one else Z.zero)
  | _ -> None

and as_float = function
  | Float f -> Some f
  | Int z -> Some (Z.to_float z)
  | Bool b -> Some (if b then 1. else 0.)
  | _ -> None

and is_number v = as_float v <> None
and is_complex = function Complex _ -> true | _ -> false

(* any numeric operand, including complex (real types embed as imag 0) *)
and is_numeric v = is_number v || is_complex v

and as_complex = function
  | Complex (re, im) -> Some (re, im)
  | v -> ( match as_float v with Some f -> Some (f, 0.) | None -> None)

(* ref: 3.2.5 — the raw bytes of a bytes-like object (bytes or bytearray); used
   for cross-type comparison, concatenation and membership. *)
and as_bytes st v =
  match v with
  | Bytes s -> Some s
  | Ref a -> ( match heap_get st a with Bytearray s -> Some s | _ -> None)
  | _ -> None

(* ref: 6.10.1 Value comparisons (numbers compared mathematically) and 3.2.4.2
   Real — IEEE 754 semantics. Numeric equality and ordering; assume both are
   numbers. Integers and bools compare exactly; floats follow IEEE 754 via
   OCaml's comparison *operators* (NaN is unordered and unequal to everything,
   0.0 = -0.0) — unlike [compare], which imposes a total order treating NaN as
   equal to itself. *)
and num_eq a b =
  match (as_z a, as_z b) with
  | Some x, Some y -> Z.equal x y
  | _ -> Option.get (as_float a) = Option.get (as_float b)

and num_lt a b =
  match (as_z a, as_z b) with
  | Some x, Some y -> Z.lt x y
  | _ -> Option.get (as_float a) < Option.get (as_float b)

and num_le a b =
  match (as_z a, as_z b) with
  | Some x, Some y -> Z.leq x y
  | _ -> Option.get (as_float a) <= Option.get (as_float b)

(* ---------- equality ----------------------------------------------- *)

(* ref: 3.2 — a built-in-type subclass instance that does not override the
   relevant comparison dunder(s) compares using its underlying payload. *)
and cmp_unwrap st v dunders : value r =
  match deref st v with
  | Some (Instance { cls; native; _ }) when native <> None_ ->
      let rec ov st = function
        | [] -> Ok (false, st)
        | d :: rest ->
            let* m, st = type_lookup st cls d in
            if m <> None then Ok (true, st) else ov st rest
      in
      let* overridden, st = ov st dunders in
      Ok ((if overridden then v else native), st)
  | _ -> Ok (v, st)

and eq_dunders = [ "__eq__"; "__ne__" ]
and order_dunders = [ "__lt__"; "__gt__"; "__le__"; "__ge__" ]

and py_eq st a b : bool r =
  let* a, st = cmp_unwrap st a eq_dunders in
  let* b, st = cmp_unwrap st b eq_dunders in
  match (a, b) with
  | _ when is_number a && is_number b -> Ok (num_eq a b, st)
  | _ when (is_complex a && is_numeric b) || (is_numeric a && is_complex b) ->
      let ar, ai = Option.get (as_complex a)
      and br, bi = Option.get (as_complex b) in
      Ok (ar = br && ai = bi, st)
  | Str x, Str y -> Ok (x = y, st)
  | _, _ when as_bytes st a <> None && as_bytes st b <> None ->
      (* ref: 3.2.5 — bytes and bytearray compare across their types *)
      Ok (as_bytes st a = as_bytes st b, st)
  | None_, None_ -> Ok (true, st)
  | Tuple xs, Tuple ys -> seq_eq st xs ys
  | Ref x, Ref y when x = y -> Ok (true, st)
  | Ref x, Ref y -> (
      match (heap_get st x, heap_get st y) with
      | List xs, List ys -> seq_eq st xs ys
      | Dict xs, Dict ys -> dict_eq st xs ys
      | (Set xs | Frozenset xs), (Set ys | Frozenset ys) -> set_eq st xs ys
      | Instance _, _ | _, Instance _ -> instance_eq st a b
      | _ -> Ok (false, st))
  | (Ref _, _ | _, Ref _) when is_instance_value st a || is_instance_value st b
    ->
      instance_eq st a b
  (* singletons and other immediates compare by identity: None/Ellipsis/
     NotImplemented/builtins are equal only to themselves *)
  | _ -> Ok (a = b, st)

and seq_eq st xs ys =
  if List.length xs <> List.length ys then Ok (false, st)
  else
    fold_m st
      (fun st acc (x, y) -> if not acc then Ok (false, st) else py_eq st x y)
      true (List.combine xs ys)

and dict_eq st xs ys =
  if List.length xs <> List.length ys then Ok (false, st)
  else
    fold_m st
      (fun st acc (k, v) ->
        if not acc then Ok (false, st)
        else
          let* found, st = dict_find st ys k in
          match found with None -> Ok (false, st) | Some v' -> py_eq st v v')
      true xs

and set_eq st xs ys =
  if List.length xs <> List.length ys then Ok (false, st)
  else set_subset st xs ys

and set_subset st xs ys =
  fold_m st
    (fun st acc x ->
      if not acc then Ok (false, st)
      else
        let* m, st = set_mem st ys x in
        Ok (m, st))
    true xs

(* Object identity, matching the `is` operator: heap objects by address, other
   immediates structurally. *)
and val_identical a b =
  match (a, b) with
  | Ref x, Ref y -> x = y
  | Null, Null -> true
  | Ref _, _ | _, Ref _ -> false
  | x, y -> x = y

and set_mem st elems x =
  (* ref: 6.10.2 Membership test operations — x in y is
     any(x is e or x == e for e in y); the identity check short-circuits so an
     element is found even if its __eq__ disagrees. *)
  fold_m st
    (fun st acc e ->
      if acc || val_identical e x then Ok (true, st) else py_eq st e x)
    false elems

and is_instance_value st = function
  | Ref a -> ( match heap_get st a with Instance _ -> true | _ -> false)
  | _ -> false

(* ref: 3.3.1 (rich comparison methods __lt__../__eq__..) and 3.2.2 NotImplemented.
   Call a rich-comparison dunder, treating both "not defined" and a
   NotImplemented return as "no answer here" (so the protocol falls through to
   the reflected method, then to the default). *)
and try_richcmp st v name args : value option r =
  let* m, st = find_dunder st v name in
  match m with
  | Some f -> (
      let* r, st = call st f args [] in
      match r with Not_implemented -> Ok (None, st) | _ -> Ok (Some r, st))
  | None -> Ok (None, st)

(* ref: 3.3.1 / 6.10.1 / 3.3.8 — reflected-operand priority. If both operands are
   instances, type(b) is a *proper* subclass of type(a), and type(b) overrides
   the reflected method, the reflected method is tried before the left's method. *)
and reflected_priority st a b reflected : bool r =
  match (deref st a, deref st b) with
  | Some (Instance { cls = ca; _ }), Some (Instance { cls = cb; _ })
    when ca <> cb && List.mem ca (cls_of st cb).mro ->
      let* mb, st = type_lookup st cb reflected in
      let* ma, st = type_lookup st ca reflected in
      Ok (mb <> None && mb <> ma, st)
  | _ -> Ok (false, st)

and instance_eq st a b : bool r =
  let* v, st = instance_eq_value st a b in
  py_truth st v

(* ref: 3.3.1 — default __eq__ is identity, == falls back to `is` when no method
   gives a result. __eq__ is its own reflection, so subclass priority just swaps
   which operand is tried first. Returns the *raw* result (may be non-bool;
   6.10.1). *)
and instance_eq_value st a b : value r =
  let* prio, st = reflected_priority st a b "__eq__" in
  let first, second = if prio then (b, a) else (a, b) in
  let* r, st = try_richcmp st first "__eq__" [ second ] in
  match r with
  | Some v -> Ok (v, st)
  | None -> (
      let* r, st = try_richcmp st second "__eq__" [ first ] in
      match r with Some v -> Ok (v, st) | None -> Ok (Bool (a = b), st))

(* raw result of x != y for instances: a custom __ne__, else `not __eq__` *)
and instance_ne_value st a b : value r =
  let* prio, st = reflected_priority st a b "__ne__" in
  let first, second = if prio then (b, a) else (a, b) in
  let* r, st = try_richcmp st first "__ne__" [ second ] in
  match r with
  | Some v -> Ok (v, st)
  | None -> (
      let* r, st = try_richcmp st second "__ne__" [ first ] in
      match r with
      | Some v -> Ok (v, st)
      | None ->
          let* e, st = py_eq st a b in
          Ok (Bool (not e), st))

(* ---------- ordering ----------------------------------------------- *)

and py_lt st a b : bool r =
  let* a, st = cmp_unwrap st a order_dunders in
  let* b, st = cmp_unwrap st b order_dunders in
  match (a, b) with
  | _ when is_number a && is_number b -> Ok (num_lt a b, st)
  | Str x, Str y -> Ok (String.compare x y < 0, st)
  | _, _ when as_bytes st a <> None && as_bytes st b <> None ->
      let x = Option.get (as_bytes st a) and y = Option.get (as_bytes st b) in
      Ok (String.compare x y < 0, st)
  | Tuple xs, Tuple ys -> seq_lt st xs ys
  | Ref x, Ref y -> (
      match (heap_get st x, heap_get st y) with
      | List xs, List ys -> seq_lt st xs ys
      | (Set xs | Frozenset xs), (Set ys | Frozenset ys) ->
          (* ref: 6.10.1 — sets compare by (strict) subset *)
          let* sub, st = set_subset st xs ys in
          Ok (sub && List.length xs < List.length ys, st)
      | Instance _, _ | _, Instance _ ->
          instance_order st a b "__lt__" "__gt__" "<"
      | _ -> order_type_error st a b "<")
  | _ when is_instance_value st a || is_instance_value st b ->
      instance_order st a b "__lt__" "__gt__" "<"
  | _ -> order_type_error st a b "<"

and seq_lt st xs ys =
  match (xs, ys) with
  | [], [] -> Ok (false, st)
  | [], _ -> Ok (true, st)
  | _, [] -> Ok (false, st)
  | x :: xs, y :: ys ->
      let* eq, st = py_eq st x y in
      if eq then seq_lt st xs ys else py_lt st x y

(* ref: 3.3.1 — __lt__/__gt__ (and __le__/__ge__) are each other's reflections;
   try the left operand's method, then the right operand's reflected method,
   else raise (6.10.1: order comparisons of unsupported types raise TypeError). *)
and instance_order st a b dunder rdunder sym : bool r =
  let* v, st = instance_order_value st a b dunder rdunder sym in
  py_truth st v

and instance_order_value st a b dunder rdunder sym : value r =
  let* prio, st = reflected_priority st a b rdunder in
  let t1, n1, a1, t2, n2, a2 =
    if prio then (b, rdunder, a, a, dunder, b) else (a, dunder, b, b, rdunder, a)
  in
  let* r, st = try_richcmp st t1 n1 [ a1 ] in
  match r with
  | Some v -> Ok (v, st)
  | None -> (
      let* r, st = try_richcmp st t2 n2 [ a2 ] in
      match r with Some v -> Ok (v, st) | None -> order_type_error st a b sym)

and order_type_error : 'a. state -> value -> value -> string -> 'a r =
 fun st a b sym ->
  raise_py st "TypeError"
    (Printf.sprintf "'%s' not supported between instances of '%s' and '%s'" sym
       (type_name st a) (type_name st b))

(* ref: 6.10.1 — a comparison yields the rich-comparison method's raw result,
   which need not be a bool; bool() is only applied in a boolean context (the
   bytecode's COMPARE_OP bool-flag, surfaced as coerce_bool). For builtins and
   for the identity/ordering fallbacks the result is already a bool. *)
and py_compare_value st (op : Phir.cmpop) a b : value r =
  let dunders = match op with Eq | Ne -> eq_dunders | _ -> order_dunders in
  let* a, st = cmp_unwrap st a dunders in
  let* b, st = cmp_unwrap st b dunders in
  if is_instance_value st a || is_instance_value st b then
    match op with
    | Eq -> instance_eq_value st a b
    | Ne -> instance_ne_value st a b
    | Lt -> instance_order_value st a b "__lt__" "__gt__" "<"
    | Gt -> instance_order_value st a b "__gt__" "__lt__" ">"
    | Le -> instance_order_value st a b "__le__" "__ge__" "<="
    | Ge -> instance_order_value st a b "__ge__" "__le__" ">="
  else
    let* v, st = py_compare st op a b in
    Ok (Bool v, st)

and py_compare st (op : Phir.cmpop) a b : bool r =
  match op with
  | Eq -> py_eq st a b
  | Ne ->
      (* ref: 3.3.1 — x != y calls x.__ne__(y) (then the reflected y.__ne__(x));
         the default object.__ne__ delegates to __eq__ and inverts it. *)
      if is_instance_value st a || is_instance_value st b then
        let* prio, st = reflected_priority st a b "__ne__" in
        let first, second = if prio then (b, a) else (a, b) in
        let* r, st = try_richcmp st first "__ne__" [ second ] in
        match r with
        | Some v -> py_truth st v
        | None -> (
            let* r, st = try_richcmp st second "__ne__" [ first ] in
            match r with
            | Some v -> py_truth st v
            | None ->
                let* e, st = py_eq st a b in
                Ok (not e, st))
      else
        let* e, st = py_eq st a b in
        Ok (not e, st)
  | Lt -> py_lt st a b
  | Gt -> py_lt st b a
  | Le -> (
      if
        (* ref: 6.10.1 — numbers use IEEE <= directly (so any NaN comparison is
         false); sets use subset; otherwise a<=b is "not (b<a)" with __le__. *)
        is_number a && is_number b
      then Ok (num_le a b, st)
      else
        match (deref st a, deref st b) with
        | Some (Set xs | Frozenset xs), Some (Set ys | Frozenset ys) ->
            set_subset st xs ys
        | _ ->
            let* gt, st = py_lt st b a in
            if gt then Ok (false, st) else le_fallback st a b)
  | Ge -> (
      if is_number a && is_number b then Ok (num_le b a, st)
      else
        match (deref st a, deref st b) with
        | Some (Set xs | Frozenset xs), Some (Set ys | Frozenset ys) ->
            set_subset st ys xs
        | _ -> le_fallback_ge st a b)

and le_fallback st a b =
  if is_instance_value st a || is_instance_value st b then
    instance_order st a b "__le__" "__ge__" "<="
  else Ok (true, st)
(* not (b < a) was already established *)

and le_fallback_ge st a b =
  if is_instance_value st a || is_instance_value st b then
    instance_order st a b "__ge__" "__le__" ">="
  else
    let* lt, st = py_lt st a b in
    Ok (not lt, st)

(* ---------- truthiness and length ---------------------------------- *)

(* ref: 3.3.1 __bool__ — truth-value testing; with no __bool__, __len__ is used
   (nonzero is true); with neither, instances are always true. Builtin types:
   numbers nonzero, empty containers/str false. *)
and py_truth st (v : value) : bool r =
  match v with
  | None_ -> Ok (false, st)
  | Bool b -> Ok (b, st)
  | Int z -> Ok (not (Z.equal z Z.zero), st)
  | Float f -> Ok (f <> 0., st)
  | Complex (re, im) -> Ok (re <> 0. || im <> 0., st)
  | Str s -> Ok (s <> "", st)
  | Bytes s -> Ok (s <> "", st)
  | Tuple xs -> Ok (xs <> [], st)
  | Range _ ->
      let* n, st = py_len st v in
      Ok (n > 0, st)
  | Ref a -> (
      match heap_get st a with
      | List xs -> Ok (xs <> [], st)
      | Dict ps -> Ok (ps <> [], st)
      | Set xs | Frozenset xs -> Ok (xs <> [], st)
      | Bytearray s -> Ok (s <> "", st)
      | Instance _ -> (
          let* m, st = find_dunder st v "__bool__" in
          match m with
          | Some f -> (
              let* b, st = call st f [] [] in
              match b with
              | Bool b -> Ok (b, st)
              | _ ->
                  raise_py st "TypeError"
                    (Printf.sprintf "__bool__ should return bool, returned %s"
                       (type_name st b)))
          | None -> (
              let* m, st = find_dunder st v "__len__" in
              match m with
              | Some f ->
                  let* n, st = call st f [] [] in
                  py_truth st n
              | None -> (
                  match native_of st v with
                  | Some p -> py_truth st p
                  | None -> Ok (true, st))))
      | _ -> Ok (true, st))
  | _ -> Ok (true, st)

and py_len st (v : value) : int r =
  match v with
  | Str s -> Ok (utf8_length s, st)
  | Bytes s -> Ok (String.length s, st)
  | Tuple xs -> Ok (List.length xs, st)
  | Range (start, stop, step) ->
      let open Z in
      let n =
        if gt step zero then
          if geq start stop then zero
          else fdiv (add (sub stop start) (sub step one)) step
        else if leq start stop then zero
        else fdiv (add (sub start stop) (sub (neg step) one)) (neg step)
      in
      Ok (Z.to_int n, st)
  | Ref a -> (
      match heap_get st a with
      | List xs -> Ok (List.length xs, st)
      | Dict ps -> Ok (List.length ps, st)
      | Set xs | Frozenset xs -> Ok (List.length xs, st)
      | Bytearray s -> Ok (String.length s, st)
      | Instance _ -> (
          let* m, st = find_dunder st v "__len__" in
          match m with
          | Some f -> (
              let* n, st = call st f [] [] in
              match as_z n with
              | Some z -> Ok (Z.to_int z, st)
              | None -> raise_py st "TypeError" "__len__ should return an int")
          | None -> (
              match native_of st v with
              | Some p -> py_len st p
              | None -> no_len st v))
      | _ -> no_len st v)
  | _ -> no_len st v

and no_len : 'a. state -> value -> 'a r =
 fun st v ->
  raise_py st "TypeError"
    (Printf.sprintf "object of type '%s' has no len()" (type_name st v))

(* ref: 3.3.1 __hash__ / hash() — equal objects hash equally. int/bool and
   integral floats use CPython's modular int hash; an instance uses its custom
   __hash__ (its result reduced the same way), or is unhashable when its class
   overrides __eq__ without __hash__ or sets __hash__ = None. Hashes of str,
   non-integral float and tuple are deterministic but not CPython-identical
   (CPython salts/derives them); they are never compared byte-for-byte. *)
and py_hash st (v : value) : value r =
  let unhashable st =
    raise_py st "TypeError"
      (Printf.sprintf "unhashable type: '%s'" (type_name st v))
  in
  match v with
  | Bool b -> Ok (Int (if b then Z.one else Z.zero), st)
  | Int z -> Ok (Int (int_hash z), st)
  | Float f when Float.is_integer f && Float.abs f < 9.0e18 ->
      Ok (Int (int_hash (Z.of_float f)), st)
  | Float f ->
      Ok (Int (int_hash (Z.of_int (Int64.to_int (Int64.bits_of_float f)))), st)
  | Str s | Bytes s ->
      let h = String.fold_left (fun a c -> (a * 1000003) + Char.code c) 0 s in
      Ok (Int (int_hash (Z.of_int h)), st)
  | None_ | Ellipsis | Not_implemented -> Ok (Int Z.zero, st)
  | Tuple xs ->
      let* hs, st = map_m st py_hash xs in
      let h_red z = Z.to_int (Z.rem (Z.abs z) (Z.of_int 1000000007)) in
      let combine a = function Int z -> (a * 1000003) + h_red z | _ -> a in
      Ok (Int (int_hash (Z.of_int (List.fold_left combine 1 hs))), st)
  | Ref a -> (
      match heap_get st a with
      | List _ | Dict _ | Set _ | Bytearray _ -> unhashable st
      | Frozenset xs ->
          (* frozensets are hashable; combine element hashes order-independently *)
          let* hs, st = map_m st py_hash xs in
          let combine a = function
            | Int z -> a lxor Z.to_int (Z.rem (Z.abs z) (Z.of_int 1000000007))
            | _ -> a
          in
          Ok (Int (int_hash (Z.of_int (List.fold_left combine 1 hs))), st)
      | Instance { cls; _ } -> (
          let* h, st = type_lookup st cls "__hash__" in
          match h with
          | Some None_ -> unhashable st
          | Some f -> (
              let* r, st =
                call st (bind_class_value st f ~inst:v ~cls_addr:cls) [] []
              in
              match r with
              | Int z -> Ok (Int (int_hash z), st)
              | Bool b -> Ok (Int (if b then Z.one else Z.zero), st)
              | _ ->
                  raise_py st "TypeError"
                    "__hash__ method should return an integer")
          | None -> (
              (* ref: 3.2 — a built-in subclass hashes via its payload (mutable
                 ones stay unhashable); otherwise identity hash unless __eq__ is
                 defined without __hash__ *)
              match native_of st v with
              | Some p -> py_hash st p
              | None -> (
                  let* eq, st = type_lookup st cls "__eq__" in
                  match eq with
                  | Some _ -> unhashable st
                  | None -> Ok (Int (int_hash (Z.of_int a)), st))))
      | _ -> Ok (Int (int_hash (Z.of_int a)), st))
  | _ -> Ok (Int Z.zero, st)

(* ---------- repr and str ------------------------------------------- *)

and py_repr st (v : value) : string r =
  match v with
  | None_ -> Ok ("None", st)
  | Bool true -> Ok ("True", st)
  | Bool false -> Ok ("False", st)
  | Int z -> Ok (Z.to_string z, st)
  | Float f -> Ok (float_repr f, st)
  | Complex (re, im) -> Ok (complex_repr re im, st)
  | Str s -> Ok (str_repr s, st)
  | Bytes s -> Ok (bytes_repr s, st)
  | Tuple [ x ] ->
      let* r, st = py_repr st x in
      Ok ("(" ^ r ^ ",)", st)
  | Tuple xs ->
      let* rs, st = map_m st py_repr xs in
      Ok ("(" ^ String.concat ", " rs ^ ")", st)
  | Range (a, b, c) ->
      Ok
        ( (if Z.equal c Z.one then
             Printf.sprintf "range(%s, %s)" (Z.to_string a) (Z.to_string b)
           else
             Printf.sprintf "range(%s, %s, %s)" (Z.to_string a) (Z.to_string b)
               (Z.to_string c)),
          st )
  | Slice (a, b, c) ->
      let* rs, st = map_m st py_repr [ a; b; c ] in
      Ok ("slice(" ^ String.concat ", " rs ^ ")", st)
  | Builtin name -> Ok ("<built-in function " ^ name ^ ">", st)
  | Bound (f, _) ->
      let* r, st = py_repr st f in
      Ok ("<bound method of " ^ r ^ ">", st)
  | Code_obj c -> Ok ("<code object " ^ c.qualname ^ ">", st)
  | Null -> Ok ("<NULL>", st)
  | Not_implemented -> Ok ("NotImplemented", st)
  | Ellipsis -> Ok ("Ellipsis", st)
  | Ref a -> (
      match heap_get st a with
      | List xs ->
          let* rs, st = map_m st py_repr xs in
          Ok ("[" ^ String.concat ", " rs ^ "]", st)
      | Dict [] -> Ok ("{}", st)
      | Dict ps ->
          let* rs, st =
            map_m st
              (fun st (k, v) ->
                let* rk, st = py_repr st k in
                let* rv, st = py_repr st v in
                Ok (rk ^ ": " ^ rv, st))
              ps
          in
          Ok ("{" ^ String.concat ", " rs ^ "}", st)
      | Set [] -> Ok ("set()", st)
      | Set xs ->
          let* rs, st = map_m st py_repr xs in
          Ok ("{" ^ String.concat ", " rs ^ "}", st)
      | Frozenset [] -> Ok ("frozenset()", st)
      | Frozenset xs ->
          (* ref: 3.2.6 — repr is frozenset({...}) *)
          let* rs, st = map_m st py_repr xs in
          Ok ("frozenset({" ^ String.concat ", " rs ^ "})", st)
      | Bytearray s -> Ok ("bytearray(" ^ bytes_repr s ^ ")", st)
      | Func fn -> Ok ("<function " ^ fn.code.qualname ^ ">", st)
      | Class _ ->
          let* n, st = class_qualified_name st a in
          Ok ("<class '" ^ n ^ "'>", st)
      | Instance _ -> (
          let* m, st = find_dunder st v "__repr__" in
          match m with
          | Some f -> (
              let* r, st = call st f [] [] in
              match r with
              | Str s -> Ok (s, st)
              | _ ->
                  raise_py st "TypeError"
                    (Printf.sprintf "__repr__ returned non-string (type %s)"
                       (type_name st r)))
          | None -> (
              (* ref: 3.2 — a built-in subclass uses the payload's repr; an
                 exception reprs as Type(arg, ...); a plain object as <Type
                 object> *)
              match native_of st v with
              | Some p -> py_repr st p
              | None ->
                  if is_exception_instance st v then exc_repr st v
                  else Ok (Printf.sprintf "<%s object>" (type_name st v), st)))
      | Gen _ -> Ok ("<generator object>", st)
      | Super _ -> Ok ("<super>", st)
      | Property _ -> Ok ("<property object>", st)
      | Classmethod _ -> Ok ("<classmethod object>", st)
      | Staticmethod _ -> Ok ("<staticmethod object>", st)
      | Cell _ -> Ok ("<cell>", st)
      | Iter _ -> Ok ("<iterator>", st)
      (* ref: 3.3.5 — list[int] reprs as origin[arg, ...] *)
      | Generic_alias { ga_origin; ga_args } ->
          let* o, st = type_repr st ga_origin in
          let* parts, st = map_m st type_repr ga_args in
          Ok (o ^ "[" ^ String.concat ", " parts ^ "]", st)
      (* ref: 6.7 — int | str reprs as 'a | b | ...' (None shown as None) *)
      | Union_type members ->
          let* parts, st = map_m st type_repr members in
          Ok (String.concat " | " parts, st)
      (* ref: 7.14 — a type alias reprs as its bare name *)
      | Type_alias { ta_name; _ } -> Ok (ta_name, st)
      (* ref: 8.10 — a TypeVar reprs as its name *)
      | Typevar { tv_name; _ } -> Ok (tv_name, st))

(* ref: 3.3.5/6.7 — the special "type repr" used inside GenericAlias and
   UnionType displays: a class by its (qualified) name, None as None, Ellipsis
   as ..., and nested aliases/unions recursively. *)
and type_repr st (v : value) : string r =
  match v with
  | None_ -> Ok ("None", st)
  | Ellipsis -> Ok ("...", st)
  | Ref a -> (
      match heap_get st a with
      | Class { cname = "NoneType"; _ } -> Ok ("None", st)
      | Class _ -> class_qualified_name st a
      | Generic_alias _ | Union_type _ -> py_repr st v
      | _ -> py_repr st v)
  | _ -> py_repr st v

and py_str st (v : value) : string r =
  match v with
  | Str s -> Ok (s, st)
  | Ref a -> (
      match heap_get st a with
      | Instance _ -> (
          let* m, st = find_dunder st v "__str__" in
          match m with
          | Some f -> (
              let* r, st = call st f [] [] in
              match r with
              | Str s -> Ok (s, st)
              | _ ->
                  raise_py st "TypeError"
                    (Printf.sprintf "__str__ returned non-string (type %s)"
                       (type_name st r)))
          | None -> (
              match native_of st v with
              | Some p -> py_str st p
              | None -> py_repr st v))
      | _ -> py_repr st v)
  | _ -> py_repr st v

(* ---------- iteration ---------------------------------------------- *)

and py_iter st (v : value) : value r =
  match v with
  | Str s ->
      let it, st = alloc st (Iter (It_str (s, 0))) in
      Ok (it, st)
  | Bytes s ->
      (* ref: 3.2.5.1 — iterating bytes yields the integer byte values *)
      let items =
        List.map
          (fun c -> Int (Z.of_int (Char.code c)))
          (List.of_seq (String.to_seq s))
      in
      let it, st = alloc st (Iter (It_seq items)) in
      Ok (it, st)
  | Tuple xs ->
      let it, st = alloc st (Iter (It_seq xs)) in
      Ok (it, st)
  | Range (start, stop, step) ->
      let it, st = alloc st (Iter (It_range (start, stop, step))) in
      Ok (it, st)
  | Ref a -> (
      match heap_get st a with
      | List _ ->
          let it, st = alloc st (Iter (It_list (a, 0))) in
          Ok (it, st)
      | Dict ps ->
          let it, st = alloc st (Iter (It_seq (List.map fst ps))) in
          Ok (it, st)
      | Set xs | Frozenset xs ->
          let it, st = alloc st (Iter (It_seq xs)) in
          Ok (it, st)
      | Bytearray s ->
          let items =
            List.map
              (fun c -> Int (Z.of_int (Char.code c)))
              (List.of_seq (String.to_seq s))
          in
          let it, st = alloc st (Iter (It_seq items)) in
          Ok (it, st)
      | Iter _ | Gen _ -> Ok (v, st)
      | Instance _ -> (
          let* m, st = find_dunder st v "__iter__" in
          match m with
          | Some f -> call st f [] []
          | None -> (
              match native_of st v with
              | Some p -> py_iter st p
              | None -> not_iterable st v))
      | _ -> not_iterable st v)
  | _ -> not_iterable st v

and not_iterable : 'a. state -> value -> 'a r =
 fun st v ->
  raise_py st "TypeError"
    (Printf.sprintf "'%s' object is not iterable" (type_name st v))

(* One iteration step: [Some v] or [None] when exhausted. *)
and py_next st (itv : value) : value option r =
  match itv with
  | Ref a -> (
      match heap_get st a with
      | Iter it -> iter_step st a it
      | Gen _ -> (
          let* step, st = gen_resume st a None_ in
          match step with
          | `Yield v -> Ok (Some v, st)
          | `Return _ -> Ok (None, st))
      | Instance _ -> (
          let* m, st = find_dunder st itv "__next__" in
          match m with
          | Some f -> (
              match call st f [] [] with
              | Ok (v, st) -> Ok (Some v, st)
              | Error (exc, st) ->
                  let* is_stop, st = exc_is st exc "StopIteration" in
                  if is_stop then Ok (None, st) else Error (exc, st))
          | None -> not_iterator st itv)
      | _ -> not_iterator st itv)
  | _ -> not_iterator st itv

and not_iterator : 'a. state -> value -> 'a r =
 fun st v ->
  raise_py st "TypeError"
    (Printf.sprintf "'%s' object is not an iterator" (type_name st v))

and iter_step st a (it : iter) : value option r =
  match it with
  | It_list (la, i) -> (
      match heap_get st la with
      | List xs when i < List.length xs ->
          let st = heap_set st a (Iter (It_list (la, i + 1))) in
          Ok (Some (List.nth xs i), st)
      | _ -> Ok (None, st))
  | It_seq [] -> Ok (None, st)
  | It_seq (x :: rest) ->
      let st = heap_set st a (Iter (It_seq rest)) in
      Ok (Some x, st)
  | It_str (s, off) ->
      if off >= String.length s then Ok (None, st)
      else
        let n = utf8_seq_len s.[off] in
        let st = heap_set st a (Iter (It_str (s, off + n))) in
        Ok (Some (Str (String.sub s off n)), st)
  | It_range (cur, stop, step) ->
      let exhausted =
        if Z.gt step Z.zero then Z.geq cur stop else Z.leq cur stop
      in
      if exhausted then Ok (None, st)
      else
        let st = heap_set st a (Iter (It_range (Z.add cur step, stop, step))) in
        Ok (Some (Int cur), st)
  | It_zip iters -> (
      let* nexts, st = map_m st py_next iters in
      match List.for_all Option.is_some nexts with
      | true -> Ok (Some (Tuple (List.map Option.get nexts)), st)
      | false -> Ok (None, st))
  | It_map (f, iters) -> (
      let* nexts, st = map_m st py_next iters in
      match List.for_all Option.is_some nexts with
      | true ->
          let* v, st = call st f (List.map Option.get nexts) [] in
          Ok (Some v, st)
      | false -> Ok (None, st))
  | It_filter (pred, it) -> (
      let* nx, st = py_next st it in
      match nx with
      | None -> Ok (None, st)
      | Some v ->
          let* keep, st =
            match pred with
            | None_ -> py_truth st v
            | f ->
                let* r, st = call st f [ v ] [] in
                py_truth st r
          in
          if keep then Ok (Some v, st)
          else iter_step st a (It_filter (pred, it)))
  | It_enum (i, it) -> (
      let* nx, st = py_next st it in
      match nx with
      | None -> Ok (None, st)
      | Some v ->
          let st = heap_set st a (Iter (It_enum (Z.add i Z.one, it))) in
          Ok (Some (Tuple [ Int i; v ]), st))

(* Drain an iterable into an OCaml list. *)
and to_list st (v : value) : value list r =
  let* itv, st = py_iter st v in
  let rec drain st acc =
    let* nx, st = py_next st itv in
    match nx with None -> Ok (List.rev acc, st) | Some x -> drain st (x :: acc)
  in
  drain st []

(* ---------- classes and attributes --------------------------------- *)

and cls_of st a =
  match heap_get st a with Class c -> c | _ -> invalid_arg "cls_of"

(* ref: 3.3.2.5 __slots__ — names listed in a class's own __slots__ (a str names
   one slot; a tuple/list names several). *)
and slot_names_of st = function
  | Str s -> [ s ]
  | Tuple xs -> List.filter_map (function Str s -> Some s | _ -> None) xs
  | Ref a -> (
      match heap_get st a with
      | List xs -> List.filter_map (function Str s -> Some s | _ -> None) xs
      | _ -> [])
  | _ -> []

(* A class's own __slots__ value (not inherited), if any. *)
and class_own_slots st cls_addr =
  List.find_map
    (function Str "__slots__", v -> Some v | _ -> None)
    (dict_pairs st (cls_of st cls_addr).cdict)

(* ref: 3.3.2.5 — instances have no __dict__ iff every class in the MRO (other
   than object) defines __slots__; then the only assignable attribute names are
   the union of those slots. Returns Some slot_names when there is no __dict__. *)
and instance_slots st cls_addr : string list option =
  let object_addr = builtin_class_addr st "object" in
  let classes =
    List.filter (fun a -> a <> object_addr) (cls_of st cls_addr).mro
  in
  let rec go acc = function
    | [] -> Some acc
    | c :: rest -> (
        match class_own_slots st c with
        | None -> None
        | Some sv -> go (acc @ slot_names_of st sv) rest)
  in
  go [] classes

(* Look [name] up through a class's MRO; the raw stored value, unbound. *)
and type_lookup st cls_addr name : value option r =
  let rec go st = function
    | [] -> Ok (None, st)
    | c :: rest -> (
        let* f, st = dget st (cls_of st c).cdict (Str name) in
        match f with Some v -> Ok (Some v, st) | None -> go st rest)
  in
  go st (cls_of st cls_addr).mro

(* Bind a value found on a class, as attribute access on an instance does. *)
and bind_class_value st found ~inst ~cls_addr =
  match deref st found with
  | Some (Func _) -> Bound (found, inst)
  | Some (Classmethod m) -> Bound (m, Ref cls_addr)
  | Some (Staticmethod m) -> m
  | _ -> ( match found with Builtin _ -> Bound (found, inst) | _ -> found)

(* A *special method*: looked up on the type only (never the instance
   dict), returned bound. [None] for non-instances and missing methods. *)
and find_dunder st v name : value option r =
  match deref st v with
  | Some (Instance { cls; _ }) -> (
      let* found, st = type_lookup st cls name in
      match found with
      | Some f -> Ok (Some (bind_class_value st f ~inst:v ~cls_addr:cls), st)
      | None -> Ok (None, st))
  | _ -> Ok (None, st)

and attribute_error : 'a. state -> value -> string -> 'a r =
 fun st v name ->
  raise_py st "AttributeError"
    (Printf.sprintf "'%s' object has no attribute '%s'" (type_name st v) name)

(* ref: 3.3.2 (__getattribute__) and 3.3.2.4 Invoking Descriptors — the lookup
   chain: data descriptors, then the instance dict, then class attributes (incl.
   non-data descriptors); raises AttributeError if not found (the __getattr__
   fallback is applied by [instance_getattr]). *)
and object_getattribute st inst_v cls dict name : value r =
  if name = "__dict__" then
    (* ref: 3.3.2.5 — a fully-slotted instance has no __dict__ *)
    match instance_slots st cls with
    | Some _ -> attribute_error st inst_v "__dict__"
    | None -> Ok (Ref dict, st)
  else if name = "__class__" then Ok (Ref cls, st)
  else
    let* found, st = type_lookup st cls name in
    match found with
    | Some f -> (
        let* kind, st = descr_kind st f in
        match kind with
        | `Data -> descr_get st f inst_v (Ref cls)
        | _ -> (
            let* own, st = dget st dict (Str name) in
            match own with
            | Some v -> Ok (v, st)
            | None -> (
                match kind with
                | `Non_data -> descr_get st f inst_v (Ref cls)
                | _ -> Ok (bind_class_value st f ~inst:inst_v ~cls_addr:cls, st)
                )))
    | None -> (
        let* own, st = dget st dict (Str name) in
        match own with
        | Some v -> Ok (v, st)
        | None -> (
            (* ref: 3.2 — a built-in subclass inherits the payload's methods
               (d.get, lst.append, …), bound to the underlying payload *)
            match native_of st inst_v with
            | Some p -> getattr_value st p name
            | None -> attribute_error st inst_v name))

and instance_getattr st inst_v cls dict name : value r =
  (* ref: 3.3.2 __getattribute__ / __getattr__ — a user-defined __getattribute__
     runs unconditionally; on AttributeError (from the normal lookup or a custom
     __getattribute__) the lookup falls back to __getattr__ if defined. The
     asymmetry (3.3.2): __getattr__ runs only when normal access fails. *)
  let* custom, st = type_lookup st cls "__getattribute__" in
  let primary =
    match custom with
    | Some f when f <> Builtin "object.__getattribute__" ->
        call st
          (bind_class_value st f ~inst:inst_v ~cls_addr:cls)
          [ Str name ] []
    | _ -> object_getattribute st inst_v cls dict name
  in
  match primary with
  | Ok _ -> primary
  | Error (exc, st) -> (
      let* is_ae, st =
        isinstance_value st exc (Ref (builtin_class_addr st "AttributeError"))
      in
      if not is_ae then Error (exc, st)
      else
        let* fallback, st = type_lookup st cls "__getattr__" in
        match fallback with
        | Some f ->
            call st
              (bind_class_value st f ~inst:inst_v ~cls_addr:cls)
              [ Str name ] []
        | None ->
            (* ref: StopIteration.value — the value carried by a generator
               return (args[0], or None when absent) *)
            if
              name = "value"
              && List.mem
                   (builtin_class_addr st "StopIteration")
                   (cls_of st cls).mro
            then
              let* args, st = exc_args st inst_v in
              Ok ((match args with a :: _ -> a | [] -> None_), st)
            else Error (exc, st))

and class_defines st cls name : bool r =
  let* m, st = type_lookup st cls name in
  Ok (m <> None, st)

(* ref: 3.3.2.4 Invoking Descriptors — classify a value found in a class dict:
   defining __set__/__delete__ makes it a data descriptor (overrides the
   instance dict), defining only __get__ a non-data descriptor (overridable by
   the instance dict). property is a data descriptor. *)
and descr_kind st f :
    ([ `Data | `Non_data | `Not ] * state, value * state) result =
  match deref st f with
  | Some (Property _) -> Ok (`Data, st)
  | Some (Instance { cls; _ }) ->
      let* set, st = class_defines st cls "__set__" in
      let* del, st = class_defines st cls "__delete__" in
      if set || del then Ok (`Data, st)
      else
        let* get, st = class_defines st cls "__get__" in
        Ok ((if get then `Non_data else `Not), st)
  | _ -> Ok (`Not, st)

(* ref: 3.3.2.3 (__get__) / 3.3.2.4 — invoke a descriptor's __get__(instance,
   owner); property uses its getter, and accessing via the owner class passes
   None_ as the instance. *)
and descr_get st f inst owner : value r =
  match deref st f with
  | Some (Property { fget; _ }) -> (
      match inst with None_ -> Ok (f, st) | _ -> call st fget [ inst ] [])
  | _ -> (
      let* m, st = find_dunder st f "__get__" in
      match m with Some g -> call st g [ inst; owner ] [] | None -> Ok (f, st))

(* a class's module-qualified name as repr/GenericAlias display use it: bare
   name for the builtins module (int, ValueError, …), else "module.name". *)
and class_qualified_name st a : string r =
  let c = cls_of st a in
  let* m, st = type_lookup st a "__module__" in
  let modname = match m with Some (Str s) -> s | _ -> "builtins" in
  Ok ((if modname = "builtins" then c.cname else modname ^ "." ^ c.cname), st)

and class_getattr st cls_addr name : value r =
  let c = cls_of st cls_addr in
  match name with
  | "__name__" | "__qualname__" -> Ok (Str c.cname, st)
  | "__module__" ->
      (* ref: 3.x — classes expose __module__; built-ins default to "builtins" *)
      let* m, st = type_lookup st cls_addr "__module__" in
      Ok (Option.value m ~default:(Str "builtins"), st)
  | "__mro__" -> Ok (Tuple (List.map (fun a -> Ref a) c.mro), st)
  | "__dict__" -> Ok (Ref c.cdict, st)
  | "__bases__" -> Ok (Tuple (List.map (fun a -> Ref a) c.bases), st)
  | "__doc__" ->
      (* a class's docstring is class-local: look only in its own namespace,
         defaulting to None, never inheriting it from a base class. *)
      let* d, st = dget st c.cdict (Str "__doc__") in
      Ok (Option.value d ~default:None_, st)
  | _ -> (
      let* found, st = type_lookup st cls_addr name in
      match found with
      | Some f -> (
          match deref st f with
          | Some (Classmethod m) -> Ok (Bound (m, Ref cls_addr), st)
          | Some (Staticmethod m) -> Ok (m, st)
          | Some (Property _) -> Ok (f, st)
          | Some (Instance _) -> (
              (* descriptor accessed via the class: invoke __get__(None, cls) *)
              let* kind, st = descr_kind st f in
              match kind with
              | `Data | `Non_data -> descr_get st f None_ (Ref cls_addr)
              | `Not -> Ok (f, st))
          | _ -> Ok (f, st))
      | None -> (
          (* methods of builtin types accessed unbound: int.bit_length *)
          match c.builtin with
          | Some tag when List.mem name (builtin_method_names tag) ->
              Ok (Builtin (tag ^ "." ^ name), st)
          | _ -> attribute_error st (Ref cls_addr) name))

and builtin_method_names = function
  | "str" -> str_methods
  | "list" -> list_methods
  | "dict" -> dict_methods
  | "set" -> set_methods
  | "tuple" -> tuple_methods
  | "int" | "bool" -> int_methods
  | "float" -> float_methods
  | "complex" -> complex_methods
  | "bytes" -> bytes_methods
  | "bytearray" -> bytearray_methods
  | _ -> []

(* General attribute access, dispatching on the value's kind. *)
and getattr_value st (v : value) name : value r =
  let bound_builtin tag =
    if List.mem name (builtin_method_names tag) then
      Ok (Bound (Builtin (tag ^ "." ^ name), v), st)
    else attribute_error st v name
  in
  match v with
  | Str _ -> bound_builtin "str"
  | Bytes _ -> bound_builtin "bytes"
  | Int z -> (
      (* ref: 3.2.4.1 — an int's numeric-tower attributes *)
      match name with
      | "real" | "numerator" -> Ok (v, st)
      | "imag" -> Ok (Int Z.zero, st)
      | "denominator" -> Ok (Int Z.one, st)
      | _ ->
          ignore z;
          bound_builtin "int")
  | Bool _ -> (
      match name with
      | "real" | "numerator" ->
          Ok (Int (if v = Bool true then Z.one else Z.zero), st)
      | "imag" -> Ok (Int Z.zero, st)
      | "denominator" -> Ok (Int Z.one, st)
      | _ -> bound_builtin "int")
  | Float f -> (
      (* ref: 3.2.4.2 — a float's real/imag *)
      match name with
      | "real" -> Ok (Float f, st)
      | "imag" -> Ok (Float 0., st)
      | _ -> bound_builtin "float")
  | Complex (re, im) -> (
      (* ref: 3.2.4.3 Complex — read-only real/imag, conjugate() *)
      match name with
      | "real" -> Ok (Float re, st)
      | "imag" -> Ok (Float im, st)
      | _ -> bound_builtin "complex")
  | Slice (start, stop, step) -> (
      (* ref: 3.2.13 Internal types — slice objects expose start/stop/step *)
      match name with
      | "start" -> Ok (start, st)
      | "stop" -> Ok (stop, st)
      | "step" -> Ok (step, st)
      | _ -> attribute_error st v name)
  | Tuple _ -> bound_builtin "tuple"
  | Ref a -> (
      match heap_get st a with
      | List _ -> bound_builtin "list"
      | Dict _ -> bound_builtin "dict"
      | Set _ -> bound_builtin "set"
      | Frozenset _ -> bound_builtin "frozenset"
      | Bytearray _ -> bound_builtin "bytearray"
      | Instance { cls; dict; _ } -> instance_getattr st v cls dict name
      | Class _ -> class_getattr st a name
      | Func fn -> (
          match name with
          | "__name__" -> Ok (Str fn.code.name, st)
          | "__qualname__" -> Ok (Str fn.code.qualname, st)
          | "__doc__" ->
              Ok
                ( (match fn.code.docstring with
                  | Some s -> Str s
                  | None -> None_),
                  st )
          | "__defaults__" ->
              Ok ((if fn.defaults = [] then None_ else Tuple fn.defaults), st)
          | _ -> (
              let* own, st = dget st fn.fdict (Str name) in
              match own with
              | Some v -> Ok (v, st)
              | None ->
                  (* ref: 8.7/8.10 — __annotations__ defaults to {}, and
                     __type_params__ to () *)
                  if name = "__annotations__" then Ok (alloc st (Dict []))
                  else if name = "__type_params__" then Ok (Tuple [], st)
                  else attribute_error st v name))
      | Gen _ ->
          if List.mem name gen_methods then
            Ok (Bound (Builtin ("generator." ^ name), v), st)
          else attribute_error st v name
      | Super { cls; self } -> super_getattr st ~cls ~self name
      | Property ({ fget; _ } as p) -> (
          match name with
          | "setter" -> Ok (Bound (Builtin "property.setter", v), st)
          | "getter" -> Ok (Bound (Builtin "property.getter", v), st)
          | "fget" -> Ok (fget, st)
          | "fset" -> Ok (Option.value p.fset ~default:None_, st)
          | _ -> attribute_error st v name)
      (* ref: 3.3.5 — a GenericAlias exposes __origin__/__args__ and otherwise
         delegates attribute access to its origin class *)
      | Generic_alias { ga_origin; ga_args } -> (
          match name with
          | "__origin__" -> Ok (ga_origin, st)
          | "__args__" -> Ok (Tuple ga_args, st)
          | _ -> getattr_value st ga_origin name)
      (* ref: 6.7 — a UnionType exposes its members as __args__ *)
      | Union_type members -> (
          match name with
          | "__args__" -> Ok (Tuple members, st)
          | _ -> attribute_error st v name)
      (* ref: 7.14 — __value__ lazily evaluates the alias's value expression *)
      | Type_alias { ta_name; ta_value; ta_type_params } -> (
          match name with
          | "__name__" -> Ok (Str ta_name, st)
          | "__value__" -> call st ta_value [] []
          | "__type_params__" -> Ok (ta_type_params, st)
          | _ -> attribute_error st v name)
      (* ref: 8.10 — a TypeVar exposes __name__/__bound__/__constraints__; bound
         and constraints are evaluated lazily on access *)
      | Typevar { tv_name; tv_bound; tv_constraints } -> (
          match name with
          | "__name__" -> Ok (Str tv_name, st)
          | "__bound__" -> (
              match tv_bound with
              | None_ -> Ok (None_, st)
              | f -> call st f [] [])
          | "__constraints__" -> (
              match tv_constraints with
              | None_ -> Ok (Tuple [], st)
              | f -> call st f [] [])
          | _ -> attribute_error st v name)
      | _ -> attribute_error st v name)
  | Bound (func, self) -> (
      (* ref: 3.2.8 Instance methods — a bound method exposes __self__/__func__
         and otherwise delegates to the underlying function object (so
         m.__name__, m.tag, ... work). *)
      match name with
      | "__self__" -> Ok (self, st)
      | "__func__" -> Ok (func, st)
      | _ -> getattr_value st func name)
  | _ -> attribute_error st v name

(* super: look up after [cls] in the MRO of [type(self)], bind to self. *)
and super_getattr st ~cls ~self name : value r =
  let self_cls =
    match deref st self with
    | Some (Instance { cls; _ }) -> cls
    | Some (Class _) ->
        (* When [self] is a class, super() can be bound in two ways: in a
           classmethod (self is cls or a subclass — search its own MRO) or in a
           metaclass instance method (self is a *class* whose type, the
           metaclass, derives from cls — search the metaclass MRO). ref: 3.3.3 *)
        if List.mem cls (cls_of st (addr self)).mro then addr self
        else metaclass_addr st (addr self)
    | _ -> -1
  in
  let mro = (cls_of st self_cls).mro in
  let rec after = function
    | [] -> []
    | c :: rest -> if c = cls then rest else after rest
  in
  let rec go st = function
    | [] -> attribute_error st self name
    | c :: rest -> (
        let* f, st = dget st (cls_of st c).cdict (Str name) in
        match f with
        | Some f -> Ok (bind_class_value st f ~inst:self ~cls_addr:self_cls, st)
        | None -> go st rest)
  in
  go st (after mro)

(* ref: 3.3.2 __setattr__ (the object default) and 3.3.2.4 (a data descriptor's
   __set__ takes priority): honour a data-descriptor setter, else write the
   instance dict. *)
and object_setattr st (v : value) name x : unit r =
  match deref st v with
  | Some (Instance { cls; dict; _ }) -> (
      (* ref: 3.3.2.5 — without a __dict__, only declared slot names are
         assignable; otherwise the assignment writes the instance dict *)
      let store st =
        match instance_slots st cls with
        | Some names when not (List.mem name names) ->
            raise_py st "AttributeError"
              (Printf.sprintf
                 "'%s' object has no attribute '%s' and no __dict__ for \
                  setting new attributes"
                 (type_name st v) name)
        | _ -> dict_set st dict (Str name) x
      in
      let* found, st = type_lookup st cls name in
      match found with
      | Some f -> (
          match deref st f with
          | Some (Property { fset = Some setter; _ }) ->
              let* _, st = call st setter [ v; x ] [] in
              Ok ((), st)
          | Some (Property { fset = None; _ }) ->
              raise_py st "AttributeError"
                (Printf.sprintf "property '%s' has no setter" name)
          | Some (Instance _) -> (
              (* data descriptor: __set__ overrides the instance dict *)
              let* m, st = find_dunder st f "__set__" in
              match m with
              | Some setter ->
                  let* _, st = call st setter [ v; x ] [] in
                  Ok ((), st)
              | None -> store st)
          | _ -> store st)
      | None -> store st)
  | Some (Class c) -> dict_set st c.cdict (Str name) x
  | Some (Func fn) -> dict_set st fn.fdict (Str name) x
  | _ ->
      raise_py st "AttributeError"
        (Printf.sprintf "'%s' object has no attribute '%s'" (type_name st v)
           name)

and setattr_value st (v : value) name x : unit r =
  (* ref: 3.3.2 __setattr__ — a user-defined __setattr__ intercepts every
     assignment (delegating via object.__setattr__ does the normal store) *)
  match deref st v with
  | Some (Instance { cls; _ }) -> (
      let* m, st = type_lookup st cls "__setattr__" in
      match m with
      | Some f when f <> Builtin "object.__setattr__" ->
          let* _, st =
            call st
              (bind_class_value st f ~inst:v ~cls_addr:cls)
              [ Str name; x ] []
          in
          Ok ((), st)
      | _ -> object_setattr st v name x)
  | _ -> object_setattr st v name x

(* ref: 3.3.2 __delattr__ (object default) and 3.3.2.4 (a data descriptor's
   __delete__ handles deletion). *)
and object_delattr st (v : value) name : unit r =
  match deref st v with
  | Some (Instance { cls; dict; _ }) ->
      (* a data descriptor's __delete__ handles deletion *)
      let* found, st = type_lookup st cls name in
      let* via_descr, st =
        match found with
        | Some f -> (
            match deref st f with
            | Some (Instance _) -> (
                let* m, st = find_dunder st f "__delete__" in
                match m with
                | Some d ->
                    let* _, st = call st d [ v ] [] in
                    Ok (true, st)
                | None -> Ok (false, st))
            | _ -> Ok (false, st))
        | None -> Ok (false, st)
      in
      if via_descr then Ok ((), st)
      else
        let* removed, st = dict_del st dict (Str name) in
        if removed then Ok ((), st) else attribute_error st v name
  | Some (Class c) ->
      let* removed, st = dict_del st c.cdict (Str name) in
      if removed then Ok ((), st) else attribute_error st v name
  | _ -> attribute_error st v name

and delattr_value st (v : value) name : unit r =
  (* ref: 3.3.2 __delattr__ — a user-defined __delattr__ intercepts deletion *)
  match deref st v with
  | Some (Instance { cls; _ }) -> (
      let* m, st = type_lookup st cls "__delattr__" in
      match m with
      | Some f when f <> Builtin "object.__delattr__" ->
          let* _, st =
            call st
              (bind_class_value st f ~inst:v ~cls_addr:cls)
              [ Str name ] []
          in
          Ok ((), st)
      | _ -> object_delattr st v name)
  | _ -> object_delattr st v name

(* ---------- isinstance / issubclass -------------------------------- *)

and value_matches_builtin st tag (v : value) =
  match tag with
  | "object" -> true
  | "int" -> ( match v with Int _ | Bool _ -> true | _ -> false)
  | "bool" -> ( match v with Bool _ -> true | _ -> false)
  | "float" -> ( match v with Float _ -> true | _ -> false)
  | "complex" -> ( match v with Complex _ -> true | _ -> false)
  | "str" -> ( match v with Str _ -> true | _ -> false)
  | "bytes" -> ( match v with Bytes _ -> true | _ -> false)
  | "tuple" -> ( match v with Tuple _ -> true | _ -> false)
  | "range" -> ( match v with Range _ -> true | _ -> false)
  | "list" -> ( match deref st v with Some (List _) -> true | _ -> false)
  | "dict" -> ( match deref st v with Some (Dict _) -> true | _ -> false)
  | "set" -> ( match deref st v with Some (Set _) -> true | _ -> false)
  | "frozenset" -> (
      match deref st v with Some (Frozenset _) -> true | _ -> false)
  | "bytearray" -> (
      match deref st v with Some (Bytearray _) -> true | _ -> false)
  | "type" -> ( match deref st v with Some (Class _) -> true | _ -> false)
  | "property" -> (
      match deref st v with Some (Property _) -> true | _ -> false)
  (* ref: 3.2.1/3.2.2/3.2.3 — the singleton types match only their singleton *)
  | "NoneType" -> v = None_
  | "ellipsis" -> v = Ellipsis
  | "NotImplementedType" -> v = Not_implemented
  | _ -> false

and isinstance_value st (v : value) (cls_v : value) : bool r =
  match cls_v with
  | Tuple cs ->
      fold_m st
        (fun st acc c -> if acc then Ok (true, st) else isinstance_value st v c)
        false cs
  | Ref ca -> (
      match heap_get st ca with
      (* ref: 6.7 — isinstance(x, A | B) tests membership against each member *)
      | Union_type members ->
          fold_m st
            (fun st acc m ->
              if acc then Ok (true, st) else isinstance_value st v m)
            false members
      | Class { builtin = Some tag; _ } when tag <> "object" -> (
          if value_matches_builtin st tag v then Ok (true, st)
          else
            (* ref: 3.2 — a built-in-type subclass instance is also an instance
               of that built-in type (its MRO contains it) *)
            match deref st v with
            | Some (Instance { cls; _ }) ->
                Ok (List.mem ca (cls_of st cls).mro, st)
            | _ -> Ok (false, st))
      | Class { builtin = Some _; _ } -> Ok (true, st) (* object *)
      | Class _ -> (
          match deref st v with
          | Some (Instance { cls; _ }) ->
              Ok (List.mem ca (cls_of st cls).mro, st)
          (* ref: 3.3.3 — a class is an instance of its metaclass, so
             isinstance(C, Meta) walks the metaclass MRO *)
          | Some (Class _) ->
              Ok (List.mem ca (cls_of st (metaclass_addr st (addr v))).mro, st)
          | _ -> Ok (false, st))
      | _ ->
          raise_py st "TypeError" "isinstance() arg 2 must be a type or tuple")
  | _ -> raise_py st "TypeError" "isinstance() arg 2 must be a type or tuple"

and issubclass_value st (c : value) (parent : value) : bool r =
  match (c, parent) with
  (* ref: 3.3.4 — issubclass(C, (A, B, ...)) is true if C is a subclass of any *)
  | _, Tuple ps ->
      fold_m st
        (fun st acc p -> if acc then Ok (true, st) else issubclass_value st c p)
        false ps
  | Ref a, Ref b -> (
      match (heap_get st a, heap_get st b) with
      | Class ca, Class _ -> Ok (List.mem b ca.mro, st)
      | _ -> raise_py st "TypeError" "issubclass() arg 1 must be a class")
  | _ -> raise_py st "TypeError" "issubclass() arg 1 must be a class"

and exc_is st (exc : value) clsname : bool r =
  isinstance_value st exc (Ref (builtin_class_addr st clsname))

(* ---------- class creation ----------------------------------------- *)

and c3_linearize st bases : int list =
  let mro_of b = (cls_of st b).mro in
  let seqs = List.map mro_of bases @ [ bases ] in
  let rec merge seqs =
    let seqs = List.filter (fun s -> s <> []) seqs in
    if seqs = [] then []
    else
      let in_a_tail h = List.exists (fun s -> List.mem h (List.tl s)) seqs in
      match List.find_opt (fun s -> not (in_a_tail (List.hd s))) seqs with
      | None -> invalid_arg "inconsistent MRO"
      | Some s ->
          let h = List.hd s in
          h :: merge (List.map (List.filter (fun x -> x <> h)) seqs)
  in
  merge seqs

(* The metaclass of a class, as an address: its [meta] field, or the builtin
   [type] when unset (ref: 3.3.3 — type(C) is its metaclass). *)
and metaclass_addr st cls_addr =
  match (cls_of st cls_addr).meta with
  | Some m -> m
  | None -> builtin_class_addr st "type"

(* ref: 3.3.3 — look up an overriding hook (__instancecheck__ /
   __subclasscheck__) on the metaclass of [cls], bound to cls. Returns None
   when [cls] is not a class or its metaclass is the default [type] (which
   supplies no user override). *)
and metaclass_hook st (cls : value) name : value option r =
  match deref st cls with
  | Some (Class _) -> (
      let meta = metaclass_addr st (addr cls) in
      let* hook, st = type_lookup st meta name in
      match hook with
      | Some f -> Ok (Some (bind_class_value st f ~inst:cls ~cls_addr:meta), st)
      | None -> Ok (None, st))
  | _ -> Ok (None, st)

(* ref: 3.3.3.2 Determining the appropriate metaclass — the metaclass of a
   derived class must be a (non-strict) subclass of the metaclasses of all its
   bases; the winner is the most derived among the explicit hint and every
   base's metaclass. *)
and determine_metaclass st ~explicit ~bases : int r =
  let type_addr = builtin_class_addr st "type" in
  let is_sub a b = List.mem b (cls_of st a).mro in
  let start = match explicit with Some m -> m | None -> type_addr in
  let rec go st winner = function
    | [] -> Ok (winner, st)
    | b :: rest ->
        let bm = metaclass_addr st b in
        if is_sub winner bm then go st winner rest
        else if is_sub bm winner then go st bm rest
        else
          raise_py st "TypeError"
            "metaclass conflict: the metaclass of a derived class must be a \
             (non-strict) subclass of the metaclasses of all its bases"
  in
  go st start bases

and make_class st ~name ~bases ~ns_addr ~kwds : value r =
  (* ref: 3.3.3.1 — an explicit metaclass=... keyword selects the metaclass;
     it was filtered out of [kwds] (which carry __init_subclass__ arguments) by
     the caller and is passed here separately. *)
  let explicit, kwds =
    match List.assoc_opt "metaclass" kwds with
    | Some (Ref m) when match heap_get st m with Class _ -> true | _ -> false ->
        (Some m, List.filter (fun (k, _) -> k <> "metaclass") kwds)
    | _ -> (None, kwds)
  in
  let* mcs, st = determine_metaclass st ~explicit ~bases in
  let type_addr = builtin_class_addr st "type" in
  if mcs = type_addr then
    (* fast path: the default metaclass builds the class directly *)
    type_new st ~mcs ~name ~bases ~ns_addr ~kwds
  else
    (* ref: 3.3.3 — a user metaclass is *called* as metaclass(name, bases, ns,
       **kwds); its __new__ (typically via super().__new__ -> type.__new__)
       builds the class and its __init__ then runs. *)
    let base_refs = Tuple (List.map (fun a -> Ref a) bases) in
    let ns_dict = Ref ns_addr in
    instantiate st mcs [ Str name; base_refs; ns_dict ] kwds

(* ref: 3.3.3 type.__new__ — the actual class builder: allocates the class
   object (recording its metaclass), fills the __class__ cell for super(), and
   runs the __set_name__ and __init_subclass__ hooks. *)
and type_new st ~mcs ~name ~bases ~ns_addr ~kwds : value r =
  let bases =
    match bases with [] -> [ builtin_class_addr st "object" ] | bs -> bs
  in
  let base_mro = c3_linearize st bases in
  let meta = if mcs = builtin_class_addr st "type" then None else Some mcs in
  let caddr = st.next in
  let _, st =
    alloc st
      (Class
         {
           cname = name;
           bases;
           mro = caddr :: base_mro;
           cdict = ns_addr;
           builtin = None;
           meta;
         })
  in
  (* Fill the __class__ cell used by zero-argument super(), then drop the
     __classcell__ entry the class body stored for us. *)
  let* cell, st = dget st ns_addr (Str "__classcell__") in
  let* (), st =
    match cell with
    | Some (Ref cell_addr) ->
        let st = heap_set st cell_addr (Cell (Some (Ref caddr))) in
        let* _, st = dict_del st ns_addr (Str "__classcell__") in
        Ok ((), st)
    | _ -> Ok ((), st)
  in
  (* ref: 3.3.3 Customizing class creation — type.__new__ first calls the
     __set_name__ hooks on class variables, then __init_subclass__ on the
     parent. *)
  let* (), st = run_set_name_hooks st caddr ns_addr in
  let* (), st = run_init_subclass st caddr base_mro kwds in
  Ok (Ref caddr, st)

(* ref: 3.3.3 __set_name__ — called as v.__set_name__(owner, name) for each
   class variable (in definition order) whose value defines the hook. *)
and run_set_name_hooks st caddr ns_addr : unit r =
  fold_m st
    (fun st () (k, v) ->
      match k with
      | Str name -> (
          let* m, st = find_dunder st v "__set_name__" in
          match m with
          | Some f ->
              let* _, st = call st f [ Ref caddr; Str name ] [] in
              Ok ((), st)
          | None -> Ok ((), st))
      | _ -> Ok ((), st))
    () (dict_pairs st ns_addr)

(* ref: 3.3.3 __init_subclass__ — called on the parent (searched along the new
   class's MRO, excluding itself) when it is subclassed; cls is the new class
   and the hook is implicitly a classmethod. Keyword args from the class
   definition (metaclass excluded) are forwarded. *)
and run_init_subclass st caddr base_mro kwds : unit r =
  let rec find st = function
    | [] -> Ok (None, st)
    | c :: rest -> (
        let* m, st = dget st (cls_of st c).cdict (Str "__init_subclass__") in
        match m with Some f -> Ok (Some f, st) | None -> find st rest)
  in
  let* found, st = find st base_mro in
  match found with
  | None -> Ok ((), st)
  | Some f ->
      let callee =
        match deref st f with
        | Some (Classmethod m) -> Bound (m, Ref caddr)
        | _ -> Bound (f, Ref caddr)
      in
      let* _, st = call st callee [] kwds in
      Ok ((), st)

(* ref: 3.2 — the most-derived built-in type in a class's MRO (ignoring object),
   e.g. "dict" for class D(dict). None for a plain object subclass. *)
and builtin_base_tag st cls_addr : string option =
  List.find_map
    (fun a -> match (cls_of st a).builtin with Some "object" -> None | t -> t)
    (cls_of st cls_addr).mro

(* the built-in types whose subclassing is modelled (bool is excluded — CPython
   forbids subclassing it) *)
and is_native_tag = function
  | "dict" | "list" | "set" | "frozenset" | "int" | "float" | "str" | "tuple"
  | "bytes" ->
      true
  | _ -> false

(* the underlying payload of a built-in-subclass instance, if any *)
and native_of st v : value option =
  match deref st v with
  | Some (Instance { native; _ }) when native <> None_ -> Some native
  | _ -> None

and instantiate st cls_addr args kwargs : value r =
  match builtin_base_tag st cls_addr with
  | Some tag when is_native_tag tag -> (
      (* ref: 3.2 — a subclass of a built-in type: build the payload as the
         built-in __new__ would, then run a user-defined __init__ if any. For a
         mutable container with a custom __init__, the payload starts empty and
         __init__ (typically via super().__init__) fills it. *)
      let* init, st = type_lookup st cls_addr "__init__" in
      let user_init =
        match Option.map (deref st) init with
        | Some (Some (Func _)) -> init
        | _ -> None
      in
      let mutable_container =
        match tag with "list" | "dict" | "set" -> true | _ -> false
      in
      let* payload, st =
        if mutable_container && user_init <> None then
          builtin_class_call st tag [] []
        else builtin_class_call st tag args kwargs
      in
      let d, st = alloc st (Dict []) in
      let inst, st =
        alloc st (Instance { cls = cls_addr; dict = addr d; native = payload })
      in
      match user_init with
      | Some f -> (
          let* rv, st =
            call st (bind_class_value st f ~inst ~cls_addr) args kwargs
          in
          match rv with
          | None_ -> Ok (inst, st)
          | _ ->
              raise_py st "TypeError"
                (Printf.sprintf "__init__() should return None, not '%s'"
                   (type_name st rv)))
      | None -> Ok (inst, st))
  | _ -> instantiate_plain st cls_addr args kwargs

and instantiate_plain st cls_addr args kwargs : value r =
  (* ref: 3.3.1 __new__ / __init__ — __new__ creates the instance (implicitly a
     staticmethod, receiving the class); __init__ then initialises it, but only
     when __new__ returned an instance of cls, and __init__ must return None. *)
  let* new_m, st = type_lookup st cls_addr "__new__" in
  let* inst, st =
    match new_m with
    | Some f ->
        let f = match deref st f with Some (Staticmethod m) -> m | _ -> f in
        call st f (Ref cls_addr :: args) kwargs
    | None ->
        let d, st = alloc st (Dict []) in
        Ok
          (alloc st
             (Instance { cls = cls_addr; dict = addr d; native = None_ }))
  in
  let* is_inst, st = isinstance_value st inst (Ref cls_addr) in
  if not is_inst then Ok (inst, st)
  else
    let* init, st = type_lookup st cls_addr "__init__" in
    match init with
    | Some f -> (
        let* rv, st =
          call st (bind_class_value st f ~inst ~cls_addr) args kwargs
        in
        match rv with
        | None_ -> Ok (inst, st)
        | _ ->
            raise_py st "TypeError"
              (Printf.sprintf "__init__() should return None, not '%s'"
                 (type_name st rv)))
    | None -> Ok (inst, st)

(* ---------- generators --------------------------------------------- *)

and gen_resume st a (sent : value) : [ `Yield of value | `Return of value ] r =
  match heap_get st a with
  | Gen { gframe = None; _ } -> Ok (`Return None_, st)
  | Gen ({ gframe = Some fr; _ } as g) -> (
      let st = heap_set st a (Gen { g with gframe = None; gstarted = true }) in
      match run_frame st { fr with stack = sent :: fr.stack } with
      | Ok (Yielded (v, fr'), st) ->
          Ok
            ( `Yield v,
              heap_set st a (Gen { g with gframe = Some fr'; gstarted = true })
            )
      | Ok (Returned rv, st) -> Ok (`Return rv, st)
      | Error (exc, st) -> Error (exc, st))
  | _ -> invalid_arg "gen_resume"

(* resume a suspended frame as though its current instruction raised [exc]:
   consult the frame's exception table, like run_frame's error path. *)
and resume_with_error st (f : frame) exc : frame_outcome r =
  match find_handler f.code.exn_table f.idx with
  | None -> Error (exc, st)
  | Some e ->
      let stack = drop (List.length f.stack - e.depth) f.stack in
      let stack =
        if e.push_lasti then Int (Z.of_int f.idx) :: stack else stack
      in
      run_frame st { f with stack = exc :: stack; idx = e.target_idx }

(* ref: 6.2.9 — generator.throw raises [exc] at the suspended yield point *)
and gen_throw st a exc : [ `Yield of value | `Return of value ] r =
  match heap_get st a with
  | Gen { gframe = None; _ } -> Error (exc, st)
  | Gen ({ gframe = Some fr; _ } as g) -> (
      let st = heap_set st a (Gen { g with gframe = None; gstarted = true }) in
      match resume_with_error st fr exc with
      | Ok (Yielded (v, fr'), st) ->
          Ok
            ( `Yield v,
              heap_set st a (Gen { g with gframe = Some fr'; gstarted = true })
            )
      | Ok (Returned rv, st) -> Ok (`Return rv, st)
      | Error (exc, st) -> Error (exc, st))
  | _ -> invalid_arg "gen_throw"

(* ---------- operators ----------------------------------------------- *)

and binop_dunder : Phir.binop -> string = function
  | Add -> "add"
  | Sub -> "sub"
  | Mul -> "mul"
  | Div -> "truediv"
  | Floor_div -> "floordiv"
  | Mod -> "mod"
  | Pow -> "pow"
  | And -> "and"
  | Or -> "or"
  | Xor -> "xor"
  | Lshift -> "lshift"
  | Rshift -> "rshift"
  | Mat_mul -> "matmul"

and binop_symbol : Phir.binop -> string = function
  | Add -> "+"
  | Sub -> "-"
  | Mul -> "*"
  | Div -> "/"
  | Floor_div -> "//"
  | Mod -> "%"
  | Pow -> "**"
  | And -> "&"
  | Or -> "|"
  | Xor -> "^"
  | Lshift -> "<<"
  | Rshift -> ">>"
  | Mat_mul -> "@"

and binary st (op : Phir.binop) ~inplace a b : value r =
  let repeat_seq xs n =
    List.concat (List.init (max 0 (Z.to_int n)) (fun _ -> xs))
  in
  match (op, a, b) with
  | _, a, b when is_instance_value st a || is_instance_value st b ->
      instance_binop st op ~inplace a b
  (* ref: 6.7 (PEP 604) — X | Y over types/None builds a types.UnionType *)
  | Or, a, b when is_type_operand st a || is_type_operand st b ->
      if is_type_operand st a && is_type_operand st b then build_union st a b
      else
        let* na, st = operand_type_name st a in
        let* nb, st = operand_type_name st b in
        raise_py st "TypeError"
          (Printf.sprintf "unsupported operand type(s) for |: '%s' and '%s'" na
             nb)
  | _, a, b when is_complex a || is_complex b ->
      if is_numeric a && is_numeric b then complex_binop st op a b
      else binop_type_error st op a b
  | _, a, b when is_number a && is_number b -> num_binop st op a b
  | Add, Str x, Str y -> Ok (Str (x ^ y), st)
  | Add, Str _, _ -> concat_type_error st "str" b
  (* ref: printf-style string formatting — str % args *)
  | Mod, Str fmt, _ -> printf_format st fmt b
  | Mul, Str s, n when as_z n <> None ->
      Ok (Str (String.concat "" (repeat_seq [ s ] (Option.get (as_z n)))), st)
  | Mul, n, Str s when as_z n <> None ->
      Ok (Str (String.concat "" (repeat_seq [ s ] (Option.get (as_z n)))), st)
  (* ref: 3.2.5.1/3.2.5.2 — bytes/bytearray support + concatenation and *
     repetition; the result takes the left operand's type (bytes or bytearray) *)
  | Add, Bytes x, _ when as_bytes st b <> None ->
      Ok (Bytes (x ^ Option.get (as_bytes st b)), st)
  | Add, Bytes _, _ ->
      raise_py st "TypeError"
        (Printf.sprintf "can't concat %s to bytes" (type_name st b))
  | Mul, Bytes s, n when as_z n <> None ->
      Ok (Bytes (String.concat "" (repeat_seq [ s ] (Option.get (as_z n)))), st)
  | Mul, n, Bytes s when as_z n <> None ->
      Ok (Bytes (String.concat "" (repeat_seq [ s ] (Option.get (as_z n)))), st)
  | Add, (Ref _ as ba), _ when as_bytes st ba <> None && as_bytes st b <> None
    ->
      Ok
        (alloc st
           (Bytearray (Option.get (as_bytes st ba) ^ Option.get (as_bytes st b))))
  | Add, (Ref _ as ba), _ when as_bytes st ba <> None ->
      raise_py st "TypeError"
        (Printf.sprintf "can't concat %s to bytearray" (type_name st b))
  | Mul, (Ref _ as ba), n when as_bytes st ba <> None && as_z n <> None ->
      Ok
        (alloc st
           (Bytearray
              (String.concat ""
                 (repeat_seq
                    [ Option.get (as_bytes st ba) ]
                    (Option.get (as_z n))))))
  | Mul, n, (Ref _ as ba) when as_z n <> None && as_bytes st ba <> None ->
      Ok
        (alloc st
           (Bytearray
              (String.concat ""
                 (repeat_seq
                    [ Option.get (as_bytes st ba) ]
                    (Option.get (as_z n))))))
  | Add, Tuple xs, Tuple ys -> Ok (Tuple (xs @ ys), st)
  | Add, Tuple _, _ -> concat_type_error st "tuple" b
  | Mul, Tuple xs, n when as_z n <> None ->
      Ok (Tuple (repeat_seq xs (Option.get (as_z n))), st)
  | _, Ref x, _ when heap_is_list st x -> list_binop st op ~inplace x b
  | Mul, n, Ref y when as_z n <> None && heap_is_list st y -> (
      match heap_get st y with
      | List ys ->
          let l, st = alloc st (List (repeat_seq ys (Option.get (as_z n)))) in
          Ok (l, st)
      | _ -> assert false)
  (* ref: 6.7 (PEP 584) — dict union: d1 | d2 merges, right operand wins; |=
     updates the left in place *)
  | Or, Ref x, Ref y -> (
      match (heap_get st x, heap_get st y) with
      | Dict xs, Dict ys ->
          let target, st =
            if inplace then (Ref x, st) else alloc st (Dict xs)
          in
          let* (), st =
            fold_m st (fun st () (k, v) -> dict_set st (addr target) k v) () ys
          in
          Ok (target, st)
      | _ -> set_or_binop st op a b x y)
  | _, Ref x, Ref y -> set_or_binop st op a b x y
  | _ -> binop_type_error st op a b

(* set algebra within/across set & frozenset; result takes the left's type *)
and set_or_binop st op a b x y : value r =
  let elems = function Set e | Frozenset e -> Some e | _ -> None in
  match (elems (heap_get st x), elems (heap_get st y)) with
  | Some xs, Some ys ->
      let frozen =
        match heap_get st x with Frozenset _ -> true | _ -> false
      in
      set_binop st op xs ys ~frozen
  | _ -> binop_type_error st op a b

and heap_is_list st a = match heap_get st a with List _ -> true | _ -> false

(* ref: 6.7 — operands that can take part in a X | Y type union: classes, None,
   and existing unions/generic aliases. *)
and is_type_operand st v =
  match v with
  | None_ -> true
  | Ref a -> (
      match heap_get st a with
      | Class _ | Generic_alias _ | Union_type _ -> true
      | _ -> false)
  | _ -> false

(* the type-name of an operand as CPython's binop error reports it: type(v) *)
and operand_type_name st v : string r =
  let* c, st = class_of_value st v in
  Ok ((cls_of st (addr c)).cname, st)

(* ref: 6.7 — expand a union operand to member types, mapping None -> NoneType *)
and union_members st v =
  match v with
  | None_ -> [ Ref (builtin_class_addr st "NoneType") ]
  | Ref a -> ( match heap_get st a with Union_type ms -> ms | _ -> [ v ])
  | _ -> [ v ]

and build_union st a b : value r =
  let raw = union_members st a @ union_members st b in
  (* dedup preserving order (classes compare by heap address) *)
  let rec dedup seen = function
    | [] -> []
    | x :: rest ->
        if List.mem x seen then dedup seen rest else x :: dedup (x :: seen) rest
  in
  let members = dedup [] raw in
  (* a single distinct member collapses to that type (int | int is int) *)
  match members with
  | [ only ] -> Ok (only, st)
  | _ -> Ok (alloc st (Union_type members))

and binop_type_error : 'a. state -> Phir.binop -> value -> value -> 'a r =
 fun st op a b ->
  raise_py st "TypeError"
    (Printf.sprintf "unsupported operand type(s) for %s: '%s' and '%s'"
       (binop_symbol op) (type_name st a) (type_name st b))

(* CPython gives sequence concatenation (str/list/tuple + wrong type) a
   dedicated message rather than the generic "unsupported operand type(s)". *)
and concat_type_error : 'a. state -> string -> value -> 'a r =
 fun st ltype b ->
  raise_py st "TypeError"
    (Printf.sprintf "can only concatenate %s (not \"%s\") to %s" ltype
       (type_name st b) ltype)

and num_binop st op a b : value r =
  let both_int =
    match (as_z a, as_z b) with Some x, Some y -> Some (x, y) | _ -> None
  in
  let fa = Option.get (as_float a) and fb = Option.get (as_float b) in
  match op with
  | (Lshift | Rshift)
    when match both_int with Some (_, y) -> Z.sign y < 0 | None -> false ->
      (* ref: 6.8 Shifting operations — a negative shift count is a ValueError *)
      raise_py st "ValueError" "negative shift count"
  | Add | Sub | Mul | And | Or | Xor | Lshift | Rshift -> (
      match both_int with
      | Some (x, y) ->
          let z =
            match op with
            | Add -> Z.add x y
            | Sub -> Z.sub x y
            | Mul -> Z.mul x y
            | And -> Z.logand x y
            | Or -> Z.logor x y
            | Xor -> Z.logxor x y
            | Lshift -> Z.shift_left x (Z.to_int y)
            | Rshift -> Z.shift_right x (Z.to_int y)
            | _ -> assert false
          in
          Ok (Int z, st)
      | None -> (
          match op with
          | Add -> Ok (Float (fa +. fb), st)
          | Sub -> Ok (Float (fa -. fb), st)
          | Mul -> Ok (Float (fa *. fb), st)
          | _ -> binop_type_error st op a b))
  | Div ->
      if fb = 0. then
        raise_py st "ZeroDivisionError"
          (if both_int <> None then "division by zero"
           else "float division by zero")
      else Ok (Float (fa /. fb), st)
  | Floor_div -> (
      match both_int with
      | Some (x, y) ->
          if Z.equal y Z.zero then
            raise_py st "ZeroDivisionError" "integer division or modulo by zero"
          else Ok (Int (z_floordiv x y), st)
      | None ->
          if fb = 0. then
            raise_py st "ZeroDivisionError" "float floor division by zero"
          else Ok (Float (py_float_floordiv fa fb), st))
  | Mod -> (
      match both_int with
      | Some (x, y) ->
          if Z.equal y Z.zero then
            raise_py st "ZeroDivisionError" "integer modulo by zero"
          else Ok (Int (z_mod x y), st)
      | None ->
          if fb = 0. then raise_py st "ZeroDivisionError" "float modulo"
          else Ok (Float (py_float_mod fa fb), st))
  | Pow -> (
      if
        (* ref: 6.5 The power operator — raising zero to a negative power is a
         ZeroDivisionError; int**int is int unless the exponent is negative
         (then float, via the fallback). *)
        fa = 0. && fb < 0.
      then
        raise_py st "ZeroDivisionError"
          "0.0 cannot be raised to a negative power"
      else
        match both_int with
        | Some (x, y) when Z.geq y Z.zero -> Ok (Int (Z.pow x (Z.to_int y)), st)
        | _ when fa < 0. && not (Float.is_integer fb) ->
            (* ref: 6.5 — a negative base raised to a non-integer power yields a
               complex result *)
            complex_binop st Pow (Complex (fa, 0.)) (Complex (fb, 0.))
        | _ ->
            let r = Float.pow fa fb in
            if
              Float.is_finite fa && Float.is_finite fb
              && Float.abs r = Float.infinity
            then
              (* ref: 6.5 — a finite ** finite that overflows is OverflowError *)
              let cls = builtin_class_addr st "OverflowError" in
              let exc, st =
                make_exc st cls [ Int (Z.of_int 34); Str "Result too large" ]
              in
              Error (exc, st)
            else Ok (Float r, st))
  | Mat_mul -> binop_type_error st op a b

(* ref: 6.7 Binary arithmetic operations / 3.2.4.3 Complex — complex +,-,*,/,**
   (integer exponent via CPython's c_powu); other operators are unsupported for
   complex (TypeError). Real operands are coerced to complex. *)
and complex_binop st op a b : value r =
  let ar, ai = Option.get (as_complex a)
  and br, bi = Option.get (as_complex b) in
  let prod (xr, xi) (yr, yi) =
    ((xr *. yr) -. (xi *. yi), (xr *. yi) +. (xi *. yr))
  in
  match op with
  | Add -> Ok (Complex (ar +. br, ai +. bi), st)
  | Sub -> Ok (Complex (ar -. br, ai -. bi), st)
  | Mul ->
      let r, i = prod (ar, ai) (br, bi) in
      Ok (Complex (r, i), st)
  | Div ->
      if br = 0. && bi = 0. then
        raise_py st "ZeroDivisionError" "complex division by zero"
      else
        (* Smith's algorithm, matching CPython's _Py_c_quot *)
        let r, i =
          if Float.abs br >= Float.abs bi then
            let ratio = bi /. br in
            let denom = br +. (bi *. ratio) in
            ((ar +. (ai *. ratio)) /. denom, (ai -. (ar *. ratio)) /. denom)
          else
            let ratio = br /. bi in
            let denom = (br *. ratio) +. bi in
            (((ar *. ratio) +. ai) /. denom, ((ai *. ratio) -. ar) /. denom)
        in
        Ok (Complex (r, i), st)
  | Pow -> (
      (* integer (integral real) exponents use exact repeated multiplication
         (CPython's c_powu); other exponents are out of scope. *)
      match as_complex b with
      | Some (n, im) when im = 0. && Float.is_integer n && Float.abs n <= 100.
        ->
          let powu x m =
            let rec go r p mask =
              if mask = 0 || mask > m then r
              else
                let r = if m land mask <> 0 then prod r p else r in
                go r (prod p p) (mask lsl 1)
            in
            go (1., 0.) x 1
          in
          let k = int_of_float n in
          let re, im =
            if k >= 0 then powu (ar, ai) k
            else
              let pr, pi = powu (ar, ai) (-k) in
              let d = (pr *. pr) +. (pi *. pi) in
              (pr /. d, -.pi /. d)
          in
          Ok (Complex (re, im), st)
      | _ ->
          (* ref: 3.2.4.3 — general complex power z**w = exp(w * log z) *)
          if ar = 0. && ai = 0. then
            if br = 0. && bi = 0. then Ok (Complex (1., 0.), st)
            else Ok (Complex (0., 0.), st)
          else
            let lr = Float.log (Float.hypot ar ai) and li = Float.atan2 ai ar in
            let wr, wi = prod (br, bi) (lr, li) in
            let m = Float.exp wr in
            Ok (Complex (m *. Float.cos wi, m *. Float.sin wi), st))
  | _ -> binop_type_error st op a b

and list_binop st op ~inplace x_addr b : value r =
  match (op, heap_get st x_addr, deref st b) with
  | Add, List xs, Some (List ys) ->
      if inplace then Ok (Ref x_addr, heap_set st x_addr (List (xs @ ys)))
      else
        let l, st = alloc st (List (xs @ ys)) in
        Ok (l, st)
  | Mul, List xs, _ when as_z b <> None ->
      let n = max 0 (Z.to_int (Option.get (as_z b))) in
      let repeated = List.concat (List.init n (fun _ -> xs)) in
      if inplace then Ok (Ref x_addr, heap_set st x_addr (List repeated))
      else
        let l, st = alloc st (List repeated) in
        Ok (l, st)
  | Add, List _, _ when not inplace -> concat_type_error st "list" b
  | _, List _, _ -> binop_type_error st op (Ref x_addr) b
  | _ -> assert false

and set_binop st op xs ys ~frozen : value r =
  let* result, st =
    match (op : Phir.binop) with
    | Or ->
        let* extra, st =
          fold_m st
            (fun st acc y ->
              let* m, st = set_mem st xs y in
              Ok ((if m then acc else y :: acc), st))
            [] ys
        in
        Ok (xs @ List.rev extra, st)
    | And ->
        fold_m st
          (fun st acc x ->
            let* m, st = set_mem st ys x in
            Ok ((if m then acc @ [ x ] else acc), st))
          [] xs
    | Sub ->
        fold_m st
          (fun st acc x ->
            let* m, st = set_mem st ys x in
            Ok ((if m then acc else acc @ [ x ]), st))
          [] xs
    | Xor ->
        let* left, st =
          fold_m st
            (fun st acc x ->
              let* m, st = set_mem st ys x in
              Ok ((if m then acc else acc @ [ x ]), st))
            [] xs
        in
        let* right, st =
          fold_m st
            (fun st acc y ->
              let* m, st = set_mem st xs y in
              Ok ((if m then acc else acc @ [ y ]), st))
            [] ys
        in
        Ok (left @ right, st)
    | _ ->
        raise_py st "TypeError" ("unsupported set operation " ^ binop_symbol op)
  in
  let s, st = alloc st (if frozen then Frozenset result else Set result) in
  Ok (s, st)

(* ref: 3.3.8 Emulating numeric types — the binary-operator protocol: try the
   in-place dunder (__iadd__ ...) when inplace, then the left operand's __op__,
   then the right operand's reflected __rop__; NotImplemented falls through. *)
and instance_binop st op ~inplace a b : value r =
  let base = binop_dunder op in
  let try_dunder st v name args =
    let* m, st = find_dunder st v name in
    match m with
    | Some f -> (
        let* r, st = call st f args [] in
        (* a dunder returning NotImplemented means "not applicable here",
           so the operator protocol falls through to the next candidate. *)
        match r with
        | Not_implemented -> Ok (None, st)
        | _ -> Ok (Some r, st))
    | None -> Ok (None, st)
  in
  let* r, st =
    if inplace then try_dunder st a ("__i" ^ base ^ "__") [ b ]
    else Ok (None, st)
  in
  match r with
  | Some v -> Ok (v, st)
  | None -> (
      (* forward a.__op__(b), reflected b.__rop__(a); subclass priority may swap *)
      let fwd = "__" ^ base ^ "__"
      and rev = "__r" ^ base ^ "__" in
      let* prio, st = reflected_priority st a b rev in
      let t1, n1, a1, t2, n2, a2 =
        if prio then (b, rev, a, a, fwd, b) else (a, fwd, b, b, rev, a)
      in
      let* r, st = try_dunder st t1 n1 [ a1 ] in
      match r with
      | Some v -> Ok (v, st)
      | None -> (
          let* r, st = try_dunder st t2 n2 [ a2 ] in
          match r with
          | Some v -> Ok (v, st)
          | None -> (
              (* ref: 3.2 — a built-in-type subclass without an operator
                 override operates on its underlying payload (result is the
                 built-in type) *)
              match (native_of st a, native_of st b) with
              | None, None -> binop_type_error st op a b
              | na, nb ->
                  binary st op ~inplace
                    (Option.value na ~default:a)
                    (Option.value nb ~default:b))))

and unary st (op : Phir.unop) v : value r =
  match (op, v) with
  | Negative, Int z -> Ok (Int (Z.neg z), st)
  | Negative, Bool b -> Ok (Int (if b then Z.minus_one else Z.zero), st)
  | Negative, Float f -> Ok (Float (-.f), st)
  | Not, v ->
      let* t, st = py_truth st v in
      Ok (Bool (not t), st)
  | Invert, v when as_z v <> None ->
      Ok (Int (Z.lognot (Option.get (as_z v))), st)
  | To_bool, v ->
      let* t, st = py_truth st v in
      Ok (Bool t, st)
  | (Negative | Invert), v when is_instance_value st v -> (
      let name = if op = Negative then "__neg__" else "__invert__" in
      let* m, st = find_dunder st v name in
      match m with
      | Some f -> call st f [] []
      | None ->
          raise_py st "TypeError"
            ("bad operand type for unary operator: " ^ type_name st v))
  | _ ->
      raise_py st "TypeError"
        ("bad operand type for unary operator: " ^ type_name st v)

and contains st item seq : bool r =
  match seq with
  | Str hay -> (
      match item with
      | Str needle ->
          let lh = String.length hay and ln = String.length needle in
          let rec go i =
            i + ln <= lh && (String.sub hay i ln = needle || go (i + 1))
          in
          Ok (ln = 0 || go 0, st)
      | _ ->
          raise_py st "TypeError"
            "'in <string>' requires string as left operand")
  | Bytes hay -> (
      (* ref: 3.2.5.1 — `int in bytes` tests a byte value; `bytes in bytes`
         tests a subsequence *)
      match item with
      | Bytes needle ->
          let lh = String.length hay and ln = String.length needle in
          let rec go i =
            i + ln <= lh && (String.sub hay i ln = needle || go (i + 1))
          in
          Ok (ln = 0 || go 0, st)
      | _ when as_z item <> None ->
          let n = Z.to_int (Option.get (as_z item)) in
          if n < 0 || n > 255 then
            raise_py st "ValueError" "byte must be in range(0, 256)"
          else Ok (String.contains hay (Char.chr n), st)
      | _ -> raise_py st "TypeError" "a bytes-like object is required, not '%s'"
      )
  | Tuple xs -> set_mem st xs item
  | Ref a -> (
      match heap_get st a with
      | List xs -> set_mem st xs item
      | Set xs | Frozenset xs -> set_mem st xs item
      | Bytearray s -> contains st item (Bytes s) (* same rules as bytes *)
      | Dict ps -> set_mem st (List.map fst ps) item
      | Instance _ -> (
          let* m, st = find_dunder st seq "__contains__" in
          match m with
          | Some f ->
              let* r, st = call st f [ item ] [] in
              py_truth st r
          | None -> (
              (* a built-in subclass uses the payload's membership (so str
                 subclasses do substring tests); else fall back to iteration *)
              match native_of st seq with
              | Some p -> contains st item p
              | None ->
                  let* xs, st = to_list st seq in
                  set_mem st xs item))
      | _ -> not_iterable st seq)
  | _ -> not_iterable st seq

(* ---------- subscripts and slices ----------------------------------- *)

and norm_index st ~len ~what z : int r =
  let i = Z.to_int z in
  let i = if i < 0 then i + len else i in
  if i < 0 || i >= len then
    raise_py st "IndexError" (what ^ " index out of range")
  else Ok (i, st)

and slice_args st = function
  | Slice (a, b, c) -> (
      let cv = function
        | None_ -> Ok None
        | v -> (
            match as_z v with
            | Some z -> Ok (Some z)
            | None -> Error "slice indices must be integers")
      in
      match (cv a, cv b, cv c) with
      | Ok _, Ok _, Ok (Some z) when Z.equal z Z.zero ->
          raise_py st "ValueError" "slice step cannot be zero"
      | Ok a, Ok b, Ok c -> Ok ((a, b, c), st)
      | _ -> raise_py st "TypeError" "slice indices must be integers or None")
  | _ -> raise_py st "TypeError" "expected a slice"

and subscript st obj index : value r =
  (* ref: 3.3.8 __index__ — for builtin sequence indexing, a non-integer index
     that is an instance is losslessly converted to an int via __index__. (A
     user container's __getitem__ receives the original key, so this only
     applies to str/tuple/list, not instances or dict keys.) *)
  let* index, st =
    let resolve () =
      if is_instance_value st index then
        let* m, st = find_dunder st index "__index__" in
        match m with Some f -> call st f [] [] | None -> Ok (index, st)
      else Ok (index, st)
    in
    match obj with
    | Str _ | Tuple _ -> resolve ()
    | Ref a -> (
        match heap_get st a with List _ -> resolve () | _ -> Ok (index, st))
    | _ -> Ok (index, st)
  in
  match (obj, index) with
  | Str s, Slice _ ->
      let* (a, b, c), st = slice_args st index in
      let chars = utf8_chars s in
      let idxs = slice_indices ~len:(List.length chars) a b c in
      Ok (Str (String.concat "" (List.map (List.nth chars) idxs)), st)
  | Str s, _ when as_z index <> None ->
      let* i, st =
        norm_index st ~len:(utf8_length s) ~what:"string"
          (Option.get (as_z index))
      in
      Ok (Str (utf8_sub s ~pos:i ~len:1), st)
  | Str _, _ ->
      (* ref: 6.3.2 Subscriptions — sequence key must be int/slice *)
      raise_py st "TypeError"
        (Printf.sprintf "string indices must be integers, not '%s'"
           (type_name st index))
  (* ref: 3.2.5.1 — bytes: integer index yields the byte value (an int); a slice
     yields bytes *)
  | Bytes s, Slice _ ->
      let* (a, b, c), st = slice_args st index in
      let idxs = slice_indices ~len:(String.length s) a b c in
      Ok
        ( Bytes (String.concat "" (List.map (fun i -> String.make 1 s.[i]) idxs)),
          st )
  | Bytes s, _ when as_z index <> None ->
      let* i, st =
        norm_index st ~len:(String.length s) ~what:"bytes"
          (Option.get (as_z index))
      in
      Ok (Int (Z.of_int (Char.code s.[i])), st)
  | Bytes _, _ ->
      raise_py st "TypeError"
        (Printf.sprintf "byte indices must be integers or slices, not %s"
           (type_name st index))
  | Tuple xs, Slice _ ->
      let* (a, b, c), st = slice_args st index in
      let idxs = slice_indices ~len:(List.length xs) a b c in
      Ok (Tuple (List.map (List.nth xs) idxs), st)
  | Tuple xs, _ when as_z index <> None ->
      let* i, st =
        norm_index st ~len:(List.length xs) ~what:"tuple"
          (Option.get (as_z index))
      in
      Ok (List.nth xs i, st)
  | Tuple _, _ ->
      raise_py st "TypeError"
        (Printf.sprintf "tuple indices must be integers or slices, not %s"
           (type_name st index))
  (* ref: 3.2.5 — range subscription: an int yields the element, a slice yields
     a new range *)
  | Range (rs, _, rstep), Slice _ ->
      let* (a, b, c), st = slice_args st index in
      let* n, st = py_len st obj in
      let lo, hi, stp = slice_bounds ~len:n a b c in
      let new_start = Z.add rs (Z.mul (Z.of_int lo) rstep) in
      let new_step = Z.mul rstep (Z.of_int stp) in
      let new_stop = Z.add rs (Z.mul (Z.of_int hi) rstep) in
      Ok (Range (new_start, new_stop, new_step), st)
  | Range (rs, _, rstep), _ when as_z index <> None ->
      let* n, st = py_len st obj in
      let* i, st =
        norm_index st ~len:n ~what:"range object" (Option.get (as_z index))
      in
      Ok (Int (Z.add rs (Z.mul (Z.of_int i) rstep)), st)
  | Range _, _ ->
      raise_py st "TypeError"
        (Printf.sprintf "range indices must be integers or slices, not %s"
           (type_name st index))
  | Ref a, _ -> (
      match (heap_get st a, index) with
      | List xs, Slice _ ->
          let* (s0, s1, s2), st = slice_args st index in
          let idxs = slice_indices ~len:(List.length xs) s0 s1 s2 in
          let l, st = alloc st (List (List.map (List.nth xs) idxs)) in
          Ok (l, st)
      | List xs, _ when as_z index <> None ->
          let* i, st =
            norm_index st ~len:(List.length xs) ~what:"list"
              (Option.get (as_z index))
          in
          Ok (List.nth xs i, st)
      | List _, _ ->
          raise_py st "TypeError"
            (Printf.sprintf "list indices must be integers or slices, not %s"
               (type_name st index))
      (* ref: 3.2.5.2 — bytearray: integer index yields a byte (int); a slice
         yields a new bytearray *)
      | Bytearray s, Slice _ ->
          let* (s0, s1, s2), st = slice_args st index in
          let idxs = slice_indices ~len:(String.length s) s0 s1 s2 in
          let sub =
            String.concat "" (List.map (fun i -> String.make 1 s.[i]) idxs)
          in
          Ok (alloc st (Bytearray sub))
      | Bytearray s, _ when as_z index <> None ->
          let* i, st =
            norm_index st ~len:(String.length s) ~what:"bytearray"
              (Option.get (as_z index))
          in
          Ok (Int (Z.of_int (Char.code s.[i])), st)
      | Dict ps, key -> (
          let* found, st = dict_find st ps key in
          match found with Some v -> Ok (v, st) | None -> raise_key st key)
      | Instance _, _ -> (
          let* m, st = find_dunder st obj "__getitem__" in
          match m with
          | Some f -> call st f [ index ] []
          | None -> (
              (* ref: 3.2 — a built-in-type subclass delegates to its payload; a
                 dict subclass consults __missing__ on a missing key (3.3.7) *)
              match native_of st obj with
              | Some p -> (
                  match subscript st p index with
                  | Ok _ as ok -> ok
                  | Error (exc, st) -> (
                      let* is_key, st = exc_is st exc "KeyError" in
                      if not is_key then Error (exc, st)
                      else
                        let* miss, st = find_dunder st obj "__missing__" in
                        match miss with
                        | Some f -> call st f [ index ] []
                        | None -> Error (exc, st)))
              | None ->
                  raise_py st "TypeError"
                    (Printf.sprintf "'%s' object is not subscriptable"
                       (type_name st obj))))
      | Class c, _ -> (
          (* ref: 3.3.5 Emulating generic types — Class[key] calls the class's
             __class_getitem__ (implicitly a classmethod). *)
          let* m, st = type_lookup st a "__class_getitem__" in
          match m with
          | Some f ->
              let callee =
                match deref st f with
                | Some (Classmethod m) -> Bound (m, Ref a)
                | _ -> Bound (f, Ref a)
              in
              call st callee [ index ] []
          | None -> (
              (* ref: 3.3.5 — built-in containers' __class_getitem__ produces a
                 types.GenericAlias (e.g. list[int], dict[str, int]) *)
              match c.builtin with
              | Some ("list" | "dict" | "tuple" | "set" | "frozenset" | "type")
                ->
                  let args =
                    match index with Tuple xs -> xs | _ -> [ index ]
                  in
                  Ok
                    (alloc st
                       (Generic_alias { ga_origin = obj; ga_args = args }))
              | _ ->
                  raise_py st "TypeError"
                    (Printf.sprintf "type '%s' is not subscriptable" c.cname)))
      | _ ->
          raise_py st "TypeError"
            (Printf.sprintf "'%s' object is not subscriptable"
               (type_name st obj)))
  | _ ->
      raise_py st "TypeError"
        (Printf.sprintf "'%s' object is not subscriptable" (type_name st obj))

and store_subscript st obj index v : unit r =
  match obj with
  | Ref a -> (
      match (heap_get st a, index) with
      | List xs, Slice _ ->
          let* (s0, s1, s2), st = slice_args st index in
          let idxs = slice_indices ~len:(List.length xs) s0 s1 s2 in
          let* items, st = to_list st v in
          if s2 = None || s2 = Some Z.one then
            (* contiguous splice: xs[i:j] = iterable (any length) *)
            let lo = match idxs with [] -> slice_lo xs s0 | i :: _ -> i in
            let hi = match List.rev idxs with [] -> lo | i :: _ -> i + 1 in
            let prefix = List.filteri (fun j _ -> j < lo) xs in
            let suffix = List.filteri (fun j _ -> j >= hi) xs in
            Ok ((), heap_set st a (List (prefix @ items @ suffix)))
          else if List.length items <> List.length idxs then
            (* extended slice: RHS length must match the slice exactly *)
            raise_py st "ValueError"
              (Printf.sprintf
                 "attempt to assign sequence of size %d to extended slice of \
                  size %d"
                 (List.length items) (List.length idxs))
          else
            let assoc = List.combine idxs items in
            let xs' =
              List.mapi
                (fun j x ->
                  match List.assoc_opt j assoc with Some y -> y | None -> x)
                xs
            in
            Ok ((), heap_set st a (List xs'))
      | List xs, _ when as_z index <> None ->
          let* i, st =
            norm_index st ~len:(List.length xs) ~what:"list assignment"
              (Option.get (as_z index))
          in
          Ok ((), heap_set st a (List (list_set_nth xs i v)))
      (* ref: 3.2.5.2 — bytearray is mutable: item and (contiguous) slice
         assignment, replacing the slice with the assigned bytes-like object *)
      | Bytearray s, Slice _ -> (
          let* (s0, s1, s2), st = slice_args st index in
          match as_bytes st v with
          | None -> raise_py st "TypeError" "can assign only bytes-like objects"
          | Some repl ->
              let len = String.length s in
              let idxs = slice_indices ~len s0 s1 s2 in
              let lo =
                match idxs with
                | i :: _ -> i
                | [] -> (
                    match s0 with
                    | Some z ->
                        let i = Z.to_int z in
                        let i = if i < 0 then i + len else i in
                        if i < 0 then 0 else if i > len then len else i
                    | None -> 0)
              in
              let hi = match List.rev idxs with [] -> lo | i :: _ -> i + 1 in
              let prefix = String.sub s 0 lo in
              let suffix = String.sub s hi (len - hi) in
              Ok ((), heap_set st a (Bytearray (prefix ^ repl ^ suffix))))
      | Bytearray s, _ when as_z index <> None ->
          let* i, st =
            norm_index st ~len:(String.length s) ~what:"bytearray"
              (Option.get (as_z index))
          in
          let* n, st =
            match as_z v with
            | Some z when Z.geq z Z.zero && Z.lt z (Z.of_int 256) ->
                Ok (Z.to_int z, st)
            | _ -> raise_py st "ValueError" "byte must be in range(0, 256)"
          in
          let s' = String.mapi (fun j c -> if j = i then Char.chr n else c) s in
          Ok ((), heap_set st a (Bytearray s'))
      | Dict _, key -> dict_set st a key v
      | Instance _, _ -> (
          let* m, st = find_dunder st obj "__setitem__" in
          match m with
          | Some f ->
              let* _, st = call st f [ index; v ] [] in
              Ok ((), st)
          | None -> (
              match native_of st obj with
              | Some p -> store_subscript st p index v
              | None ->
                  raise_py st "TypeError"
                    (Printf.sprintf
                       "'%s' object does not support item assignment"
                       (type_name st obj))))
      | _ ->
          raise_py st "TypeError"
            (Printf.sprintf "'%s' object does not support item assignment"
               (type_name st obj)))
  | _ ->
      raise_py st "TypeError"
        (Printf.sprintf "'%s' object does not support item assignment"
           (type_name st obj))

and slice_lo xs = function
  | Some z ->
      let len = List.length xs in
      let i = Z.to_int z in
      let i = if i < 0 then i + len else i in
      if i < 0 then 0 else if i > len then len else i
  | None -> 0

and del_subscript st obj index : unit r =
  match obj with
  | Ref a -> (
      match (heap_get st a, index) with
      | List xs, Slice _ ->
          let* (s0, s1, s2), st = slice_args st index in
          let idxs = slice_indices ~len:(List.length xs) s0 s1 s2 in
          Ok
            ( (),
              heap_set st a
                (List (List.filteri (fun j _ -> not (List.mem j idxs)) xs)) )
      | List xs, _ when as_z index <> None ->
          let* i, st =
            norm_index st ~len:(List.length xs) ~what:"list assignment"
              (Option.get (as_z index))
          in
          Ok ((), heap_set st a (List (list_del_nth xs i)))
      (* ref: 3.2.5.2 — del on a bytearray index removes that byte *)
      | Bytearray s, _ when as_z index <> None ->
          let* i, st =
            norm_index st ~len:(String.length s) ~what:"bytearray"
              (Option.get (as_z index))
          in
          let s' =
            String.sub s 0 i ^ String.sub s (i + 1) (String.length s - i - 1)
          in
          Ok ((), heap_set st a (Bytearray s'))
      | Dict _, key ->
          let* removed, st = dict_del st a key in
          if removed then Ok ((), st) else raise_key st key
      | Instance _, _ -> (
          let* m, st = find_dunder st obj "__delitem__" in
          match m with
          | Some f ->
              let* _, st = call st f [ index ] [] in
              Ok ((), st)
          | None -> (
              match native_of st obj with
              | Some p -> del_subscript st p index
              | None -> del_unsupported st obj))
      | _ -> del_unsupported st obj)
  | _ -> del_unsupported st obj

and del_unsupported : 'a. state -> value -> 'a r =
 fun st obj ->
  raise_py st "TypeError"
    (Printf.sprintf "'%s' object doesn't support item deletion"
       (type_name st obj))

(* ---------- format specs (the subset the tests exercise) ------------ *)

(* ref: printf-style string formatting (str % args). Each %-spec is translated
   to the equivalent format() mini-language spec and rendered via format_value,
   with the conversions r/a/c/i/u and mapping keys handled here. *)
and printf_format st fmt arg : value r =
  (* a tuple supplies positional args (and is checked for leftovers); any other
     value is a single positional arg that %(key) specifiers also index into *)
  let is_tuple = match arg with Tuple _ -> true | _ -> false in
  let pos_args = match arg with Tuple xs -> xs | single -> [ single ] in
  let n = String.length fmt in
  let span pred i =
    let rec go j = if j < n && pred fmt.[j] then go (j + 1) else j in
    go i
  in
  (* translate printf (flags,width,prec,conv) to a format-spec string *)
  let to_spec flags width prec conv =
    let has c = String.contains flags c in
    let numeric = String.contains "diuoxXeEfFgG" conv in
    let conv =
      match conv with 'i' | 'u' -> "d" | 'F' -> "f" | c -> String.make 1 c
    in
    let align = if has '-' then "<" else if numeric then "" else ">" in
    let sign = if has '+' then "+" else if has ' ' then " " else "" in
    let alt = if has '#' then "#" else "" in
    let zero = if has '0' && (not (has '-')) && numeric then "0" else "" in
    String.concat ""
      [
        align;
        sign;
        alt;
        zero;
        width;
        (if prec = "" then "" else "." ^ prec);
        conv;
      ]
  in
  let rec scan st i args acc =
    if i >= n then
      if is_tuple && args <> [] then
        raise_py st "TypeError"
          "not all arguments converted during string formatting"
      else Ok (String.concat "" (List.rev acc), st)
    else if fmt.[i] <> '%' then
      scan st (i + 1) args (String.make 1 fmt.[i] :: acc)
    else begin
      (* %[(key)][flags][width][.prec][len]conv *)
      let key, j =
        if i + 1 < n && fmt.[i + 1] = '(' then
          let close = span (fun c -> c <> ')') (i + 2) in
          (Some (String.sub fmt (i + 2) (close - i - 2)), close + 1)
        else (None, i + 1)
      in
      let fl_end = span (fun c -> String.contains "-+ #0" c) j in
      let flags = String.sub fmt j (fl_end - j) in
      let w_end = span (fun c -> c >= '0' && c <= '9') fl_end in
      let width = String.sub fmt fl_end (w_end - fl_end) in
      let prec, p_end =
        if w_end < n && fmt.[w_end] = '.' then
          let pe = span (fun c -> c >= '0' && c <= '9') (w_end + 1) in
          (String.sub fmt (w_end + 1) (pe - w_end - 1), pe)
        else ("", w_end)
      in
      let p_end = span (fun c -> String.contains "hlL" c) p_end in
      if p_end >= n then raise_py st "ValueError" "incomplete format"
      else
        let conv = fmt.[p_end] in
        let next = p_end + 1 in
        if conv = '%' then scan st next args ("%" :: acc)
        else
          let* (value, args'), st =
            match key with
            | Some k -> (
                (* %(key) indexes the mapping without consuming positional args *)
                match deref st arg with
                | Some (Dict _) -> (
                    let* v, st = dget st (addr arg) (Str k) in
                    match v with
                    | Some v -> Ok ((v, args), st)
                    | None -> raise_key st (Str k))
                | _ -> raise_py st "TypeError" "format requires a mapping")
            | None -> (
                match args with
                | v :: rest -> Ok ((v, rest), st)
                | [] ->
                    raise_py st "TypeError"
                      "not enough arguments for format string")
          in
          (* conversions needing pre-processing into a string value *)
          let* value, conv, st =
            match conv with
            | 'r' ->
                let* s, st = py_repr st value in
                Ok (Str s, 's', st)
            | 'a' ->
                let* s, st = py_repr st value in
                Ok (Str (ascii_escape s), 's', st)
            | 's' ->
                let* s, st = py_str st value in
                Ok (Str s, 's', st)
            | 'c' -> (
                match value with
                | Str s when utf8_length s = 1 -> Ok (Str s, 's', st)
                | _ ->
                    let* cp, st = as_int st value "%c" in
                    Ok (Str (utf8_encode cp), 's', st))
            | ('d' | 'i' | 'u')
              when match value with Float _ -> true | _ -> false ->
                (* %d truncates a float toward zero *)
                let x = Option.get (as_float value) in
                Ok (Int (Z.of_float x), conv, st)
            | _ -> Ok (value, conv, st)
          in
          let* piece, st =
            format_value st value (to_spec flags width prec conv)
          in
          scan st next args' (piece :: acc)
    end
  in
  let* s, st = scan st 0 pos_args [] in
  Ok (Str s, st)

and format_value st v spec : string r =
  if is_instance_value st v then
    (* ref: 3.3.1 __format__ — format()/f-strings/str.format delegate here;
       object's default delegates to __str__ for an empty spec and rejects a
       non-empty one *)
    let* m, st = find_dunder st v "__format__" in
    match m with
    | Some f -> (
        let* r, st = call st f [ Str spec ] [] in
        match r with
        | Str s -> Ok (s, st)
        | _ ->
            raise_py st "TypeError"
              (Printf.sprintf "__format__ must return a str, not %s"
                 (type_name st r)))
    | None -> (
        (* ref: 3.2 — a built-in subclass formats via its payload *)
        match native_of st v with
        | Some p -> format_value st p spec
        | None ->
            if spec = "" then py_str st v
            else
              raise_py st "TypeError"
                (Printf.sprintf
                   "unsupported format string passed to %s.__format__"
                   (type_name st v)))
  else if spec = "" then py_str st v
  else
    let fill, align, rest =
      let n = String.length spec in
      if n >= 2 && String.contains "<>^=" spec.[1] then
        (spec.[0], Some spec.[1], String.sub spec 2 (n - 2))
      else if n >= 1 && String.contains "<>^=" spec.[0] then
        (' ', Some spec.[0], String.sub spec 1 (n - 1))
      else (' ', None, spec)
    in
    (* ref: format spec — [[fill]align][sign][#][0][width][grouping][.prec][type] *)
    let sign, rest =
      if rest <> "" && (rest.[0] = '+' || rest.[0] = '-' || rest.[0] = ' ') then
        (rest.[0], String.sub rest 1 (String.length rest - 1))
      else ('-', rest)
    in
    let alt, rest =
      if rest <> "" && rest.[0] = '#' then
        (true, String.sub rest 1 (String.length rest - 1))
      else (false, rest)
    in
    let zero, rest =
      if rest <> "" && rest.[0] = '0' then
        (true, String.sub rest 1 (String.length rest - 1))
      else (false, rest)
    in
    let digits s =
      let rec go i =
        if i < String.length s && s.[i] >= '0' && s.[i] <= '9' then go (i + 1)
        else i
      in
      let n = go 0 in
      ( (if n = 0 then None else Some (int_of_string (String.sub s 0 n))),
        String.sub s n (String.length s - n) )
    in
    let width, rest = digits rest in
    let grouping, rest =
      if rest <> "" && (rest.[0] = ',' || rest.[0] = '_') then
        (Some rest.[0], String.sub rest 1 (String.length rest - 1))
      else (None, rest)
    in
    let precision, rest =
      if rest <> "" && rest.[0] = '.' then
        let p, r = digits (String.sub rest 1 (String.length rest - 1)) in
        (p, r)
      else (None, rest)
    in
    let conv = rest in
    (* group an unsigned digit string from the right with [sep] every [size] *)
    let group_digits sep size s =
      let rec go acc s =
        let n = String.length s in
        if n <= size then s :: acc
        else
          go (String.sub s (n - size) size :: acc) (String.sub s 0 (n - size))
      in
      String.concat (String.make 1 sep) (go [] s)
    in
    (* apply grouping to a signed (possibly fractional) numeric string *)
    let group_number size s =
      match grouping with
      | None -> s
      | Some sep ->
          let intpart, frac =
            match String.index_opt s '.' with
            | Some i -> (String.sub s 0 i, String.sub s i (String.length s - i))
            | None -> (s, "")
          in
          group_digits sep size intpart ^ frac
    in
    (* the sign prefix for a numeric body, given whether the value is negative *)
    let sign_prefix neg =
      if neg then "-" else match sign with '+' -> "+" | ' ' -> " " | _ -> ""
    in
    (* build a numeric result: (sign+altprefix, grouped magnitude) *)
    let* (prefix, mag, numeric), st =
      match (v, conv) with
      | _, ("" | "d" | "n") when as_z v <> None ->
          let z = Option.get (as_z v) in
          let s = Z.to_string (Z.abs z) in
          Ok ((sign_prefix (Z.sign z < 0), group_number 3 s, true), st)
      | _, ("x" | "X" | "o" | "b") when as_z v <> None ->
          let z = Option.get (as_z v) in
          let conv_fmt = "%" ^ conv in
          let s = Z.format conv_fmt (Z.abs z) in
          let altp =
            if not alt then ""
            else
              match conv with
              | "x" -> "0x"
              | "X" -> "0X"
              | "o" -> "0o"
              | _ -> "0b"
          in
          let size = match conv with "" | "d" | "n" -> 3 | _ -> 4 in
          Ok ((sign_prefix (Z.sign z < 0) ^ altp, group_number size s, true), st)
      | _, "f" when is_number v ->
          let x = Option.get (as_float v) in
          let p = Option.value precision ~default:6 in
          let s = Printf.sprintf "%.*f" p (Float.abs x) in
          Ok ((sign_prefix (1. /. x < 0. || x < 0.), group_number 3 s, true), st)
      | _, ("e" | "E") when is_number v ->
          let x = Option.get (as_float v) in
          let p = Option.value precision ~default:6 in
          let s = Printf.sprintf "%.*e" p (Float.abs x) in
          let s = if conv = "E" then String.uppercase_ascii s else s in
          Ok ((sign_prefix (x < 0.), s, true), st)
      | _, ("g" | "G") when is_number v ->
          let x = Option.get (as_float v) in
          let p = max 1 (Option.value precision ~default:6) in
          let s = Printf.sprintf "%.*g" p (Float.abs x) in
          let s = if conv = "G" then String.uppercase_ascii s else s in
          Ok ((sign_prefix (x < 0.), s, true), st)
      | _, "%" when is_number v ->
          let x = Option.get (as_float v) in
          let p = Option.value precision ~default:6 in
          let s = Printf.sprintf "%.*f%%" p (Float.abs x *. 100.) in
          Ok ((sign_prefix (x < 0.), s, true), st)
      | _, ("" | "s") ->
          let* s, st = py_str st v in
          let s =
            match precision with
            | Some p when utf8_length s > p -> utf8_sub s ~pos:0 ~len:p
            | _ -> s
          in
          Ok (("", s, is_number v), st)
      | _ ->
          raise_py st "ValueError"
            (Printf.sprintf "unsupported format spec '%s'" spec)
    in
    let body = prefix ^ mag in
    let result =
      match width with
      | None -> body
      | Some w -> (
          let len = utf8_length body in
          if len >= w then body
          else
            let pad = w - len in
            let fill = if zero && align = None && numeric then '0' else fill in
            let align =
              match align with
              | Some a -> a
              | None ->
                  if zero && numeric then '=' else if numeric then '>' else '<'
            in
            let mk n = String.make n fill in
            match align with
            | '<' -> body ^ mk pad
            | '>' -> mk pad ^ body
            | '^' -> mk (pad / 2) ^ body ^ mk (pad - (pad / 2))
            | '=' -> prefix ^ mk pad ^ mag
            | _ -> body)
    in
    Ok (result, st)

(* ---------- sorting (monadic merge sort: comparisons run user code) -- *)

and sorted_values st items ~key ~reverse : value list r =
  let* keyed, st =
    map_m st
      (fun st v ->
        match key with
        | None_ -> Ok ((v, v), st)
        | f ->
            let* k, st = call st f [ v ] [] in
            Ok ((k, v), st))
      items
  in
  (* [le a b]: a may stay before b (stability). *)
  let le st (ka, _) (kb, _) =
    if reverse then
      let* lt, st = py_lt st ka kb in
      Ok (not lt, st)
    else
      let* lt, st = py_lt st kb ka in
      Ok (not lt, st)
  in
  let rec msort st = function
    | ([] | [ _ ]) as xs -> Ok (xs, st)
    | xs ->
        let mid = List.length xs / 2 in
        let left, right = take mid xs in
        let* left, st = msort st left in
        let* right, st = msort st right in
        let rec merge st acc l r =
          match (l, r) with
          | [], r -> Ok (List.rev_append acc r, st)
          | l, [] -> Ok (List.rev_append acc l, st)
          | x :: ls, y :: rs ->
              let* ok, st = le st x y in
              if ok then merge st (x :: acc) ls r else merge st (y :: acc) l rs
        in
        merge st [] left right
  in
  let* sorted, st = msort st keyed in
  Ok (List.map snd sorted, st)

and extremum st items ~key ~want_max : value r =
  match items with
  | [] -> raise_py st "ValueError" "min()/max() of empty iterable"
  | first :: rest ->
      let keyof st v =
        match key with None_ -> Ok (v, st) | f -> call st f [ v ] []
      in
      let* k0, st = keyof st first in
      let* (_, best), st =
        fold_m st
          (fun st (bk, bv) v ->
            let* k, st = keyof st v in
            let* better, st =
              if want_max then py_lt st bk k else py_lt st k bk
            in
            Ok ((if better then (k, v) else (bk, bv)), st))
          (k0, first) rest
      in
      Ok (best, st)

(* ---------- variables ---------------------------------------------- *)

and name_chain_lookup st (f : frame) chain s : value r =
  let rec go st = function
    | [] ->
        raise_py st "NameError" (Printf.sprintf "name '%s' is not defined" s)
    | d :: rest -> (
        let* found, st = dget st d (Str s) in
        match found with Some v -> Ok (v, st) | None -> go st rest)
  in
  go st (chain f)

and load_var st (f : frame) (v : Phir.var) : value r =
  match v with
  | Fast i -> (
      match Int_map.find_opt i f.slots with
      | Some v -> Ok (v, st)
      | None ->
          raise_py st "UnboundLocalError"
            (Printf.sprintf
               "cannot access local variable '%s' where it is not associated \
                with a value"
               (fst f.code.localsplus.(i))))
  | Deref i -> (
      match Int_map.find_opt i f.slots with
      | Some (Ref ca) -> (
          match heap_get st ca with
          | Cell (Some v) -> Ok (v, st)
          | Cell None ->
              raise_py st "NameError"
                (Printf.sprintf
                   "free variable '%s' referenced before assignment"
                   (fst f.code.localsplus.(i)))
          | _ -> raise_py st "RuntimeError" "deref of non-cell")
      | _ -> raise_py st "RuntimeError" "deref of unbound slot")
  | Name s ->
      name_chain_lookup st f (fun f -> [ f.ns; f.globals; st.builtins ]) s
  | Global s -> name_chain_lookup st f (fun f -> [ f.globals; st.builtins ]) s

and store_var st (f : frame) (x : Phir.var) v : frame r =
  match x with
  | Fast i ->
      let slots =
        if v = Null then Int_map.remove i f.slots else Int_map.add i v f.slots
      in
      Ok ({ f with slots }, st)
  | Deref i -> (
      match Int_map.find_opt i f.slots with
      | Some (Ref ca) -> Ok (f, heap_set st ca (Cell (Some v)))
      | _ -> raise_py st "RuntimeError" "store to non-cell slot")
  | Name s ->
      let* (), st = dict_set st f.ns (Str s) v in
      Ok (f, st)
  | Global s ->
      let* (), st = dict_set st f.globals (Str s) v in
      Ok (f, st)

and del_var st (f : frame) (x : Phir.var) : frame r =
  match x with
  | Fast i ->
      if Int_map.mem i f.slots then
        Ok ({ f with slots = Int_map.remove i f.slots }, st)
      else
        raise_py st "UnboundLocalError"
          (Printf.sprintf
             "cannot access local variable '%s' where it is not associated \
              with a value"
             (fst f.code.localsplus.(i)))
  | Deref i -> (
      match Int_map.find_opt i f.slots with
      | Some (Ref ca) -> Ok (f, heap_set st ca (Cell None))
      | _ -> raise_py st "RuntimeError" "delete of non-cell slot")
  | Name s ->
      let* removed, st = dict_del st f.ns (Str s) in
      if removed then Ok (f, st)
      else raise_py st "NameError" (Printf.sprintf "name '%s' is not defined" s)
  | Global s ->
      let* removed, st = dict_del st f.globals (Str s) in
      if removed then Ok (f, st)
      else raise_py st "NameError" (Printf.sprintf "name '%s' is not defined" s)

(* ---------- operand evaluation -------------------------------------- *)

and const_value st (c : Ast.const) : value r =
  match c with
  | Ast.None_ -> Ok (None_, st)
  | Ast.Bool b -> Ok (Bool b, st)
  | Ast.Int z -> Ok (Int z, st)
  | Ast.Float f -> Ok (Float f, st)
  | Ast.Str s -> Ok (Str s, st)
  | Ast.Tuple xs ->
      let* vs, st = map_m st const_value (Array.to_list xs) in
      Ok (Tuple vs, st)
  | Ast.Frozenset xs ->
      (* ref: 3.2.6 Set types — a frozenset constant *)
      let* vs, st = map_m st const_value (Array.to_list xs) in
      let* elems, st = dedup_set st vs in
      let s, st = alloc st (Frozenset elems) in
      Ok (s, st)
  | Ast.Ellipsis ->
      Ok (Ellipsis, st) (* ref: 3.2.3 Ellipsis (the ... literal) *)
  | Ast.Complex { re; im } ->
      Ok (Complex (re, im), st) (* ref: 3.2.4.3 Complex literals *)
  | Ast.Bytes s -> Ok (Bytes s, st) (* ref: 3.2.5.1 — bytes literal *)
  | Ast.Code _ -> unsupported st "constant kind"

(* Stack operands form a prefix (popped here, rightmost = TOS); folded
   operands evaluate left to right. *)
and eval_operands st (f : frame) (ops : Phir.value list) :
    (value list * frame) r =
  let n_stack =
    List.length (List.filter (function Phir.Stack -> true | _ -> false) ops)
  in
  let popped, rest = take n_stack f.stack in
  let f = { f with stack = rest } in
  let rec go st stacked acc = function
    | [] -> Ok (List.rev acc, st)
    | (op : Phir.value) :: more -> (
        match op with
        | Stack -> (
            match stacked with
            | v :: tl -> go st tl (v :: acc) more
            | [] -> assert false)
        | Null -> go st stacked (Null :: acc) more
        | Const c ->
            let* v, st = const_value st c in
            go st stacked (v :: acc) more
        | Code c -> go st stacked (Code_obj c :: acc) more
        | Var v ->
            let* x, st = load_var st f v in
            go st stacked (x :: acc) more)
  in
  let* vals, st = go st (List.rev popped) [] ops in
  Ok ((vals, f), st)

(* ---------- calls ---------------------------------------------------- *)

and call st (callee : value) (args : value list) kwargs : value r =
  match callee with
  | Builtin name -> call_builtin st name args kwargs
  | Bound (g, self) -> call st g (self :: args) kwargs
  | Ref a -> (
      match heap_get st a with
      | Func fn -> call_func st fn args kwargs
      | Class { builtin = Some tag; _ } -> builtin_class_call st tag args kwargs
      (* ref: 3.3.3 — calling a class invokes its metaclass's __call__; the
         default (type.__call__) instantiates. A user metaclass __call__
         overrides instantiation. *)
      | Class { meta = Some meta; _ } -> (
          let* mcall, st = type_lookup st meta "__call__" in
          match mcall with
          | Some (Builtin "type.__call__") | None ->
              instantiate st a args kwargs
          | Some f ->
              call st
                (bind_class_value st f ~inst:(Ref a) ~cls_addr:meta)
                args kwargs)
      | Class _ -> instantiate st a args kwargs
      | Instance _ -> (
          let* m, st = find_dunder st callee "__call__" in
          match m with
          | Some f -> call st f args kwargs
          | None -> not_callable st callee)
      | _ -> not_callable st callee)
  | _ -> not_callable st callee

and not_callable : 'a. state -> value -> 'a r =
 fun st v ->
  raise_py st "TypeError"
    (Printf.sprintf "'%s' object is not callable" (type_name st v))

and call_func st (fn : func) args kwargs : value r =
  let* slots, st = bind_args st fn args kwargs in
  let frame =
    {
      code = fn.code;
      globals = fn.globals;
      ns = fn.globals;
      slots;
      stack = [];
      idx = 0;
      closure = fn.closure;
    }
  in
  let* out, st = run_frame st frame in
  match out with
  | Returned v -> Ok (v, st)
  | Yielded _ -> raise_py st "RuntimeError" "yield escaped its generator"

and bind_args st (fn : func) args kwargs : value Int_map.t r =
  let code = fn.code in
  let pname i = fst code.localsplus.(i) in
  let argc = code.argcount and kwonly = code.kwonlyargcount in
  let posonly = code.posonlyargcount in
  let has_va = Ast.has_varargs code and has_kw = Ast.has_varkw code in
  let va_slot = argc + kwonly in
  let kw_slot = va_slot + if has_va then 1 else 0 in
  let err msg = Printf.sprintf "%s() %s" code.name msg in
  (* positional *)
  let n_pos = min (List.length args) argc in
  let pos, extra = take n_pos args in
  let slots =
    List.fold_left
      (fun (slots, i) v -> (Int_map.add i v slots, i + 1))
      (Int_map.empty, 0) pos
    |> fst
  in
  let* slots, st =
    if extra = [] then Ok (slots, st)
    else if has_va then Ok (Int_map.add va_slot (Tuple extra) slots, st)
    else
      (* ref: 6.3.4 Calls — too many positional arguments. CPython phrases the
         accepted count as "exactly N" or "from MIN to MAX". *)
      let given = List.length args in
      let maxp = argc in
      let minp = argc - List.length fn.defaults in
      let accepts =
        if minp = maxp then
          Printf.sprintf "%d positional argument%s" maxp
            (if maxp = 1 then "" else "s")
        else Printf.sprintf "from %d to %d positional arguments" minp maxp
      in
      raise_py st "TypeError"
        (err
           (Printf.sprintf "takes %s but %d %s given" accepts given
              (if given = 1 then "was" else "were")))
  in
  let slots =
    if has_va && not (Int_map.mem va_slot slots) then
      Int_map.add va_slot (Tuple []) slots
    else slots
  in
  (* keyword arguments *)
  let param_index name =
    let rec go i =
      if i >= argc + kwonly then None
      else if pname i = name then Some i
      else go (i + 1)
    in
    go 0
  in
  let* (slots, kw_extra), st =
    fold_m st
      (fun st (slots, kw_extra) (name, v) ->
        match param_index name with
        | Some i when i >= posonly ->
            if Int_map.mem i slots then
              raise_py st "TypeError"
                (err
                   (Printf.sprintf "got multiple values for argument '%s'" name))
            else Ok ((Int_map.add i v slots, kw_extra), st)
        | _ -> Ok ((slots, (name, v) :: kw_extra), st))
      (slots, []) kwargs
  in
  let kw_extra = List.rev kw_extra in
  let* slots, st =
    if has_kw then
      let d, st =
        alloc st (Dict (List.map (fun (n, v) -> (Str n, v)) kw_extra))
      in
      Ok (Int_map.add kw_slot d slots, st)
    else if kw_extra = [] then Ok (slots, st)
    else
      (* ref: 6.3.4 Calls / 8.7 — keywords with no matching fillable slot and no
         **kwargs: a positional-only parameter name passed by keyword gets its
         own message, else an unexpected-keyword error. *)
      let names = List.map fst kw_extra in
      let posonly_names =
        List.filter
          (fun n ->
            match param_index n with Some i -> i < posonly | None -> false)
          names
      in
      if posonly_names <> [] then
        raise_py st "TypeError"
          (err
             (Printf.sprintf
                "got some positional-only arguments passed as keyword \
                 arguments: '%s'"
                (String.concat ", " posonly_names)))
      else
        raise_py st "TypeError"
          (err
             (Printf.sprintf "got an unexpected keyword argument '%s'"
                (List.hd names)))
  in
  (* defaults *)
  let n_def = List.length fn.defaults in
  let slots =
    List.fold_left
      (fun (slots, i) v ->
        ((if Int_map.mem i slots then slots else Int_map.add i v slots), i + 1))
      (slots, argc - n_def)
      fn.defaults
    |> fst
  in
  let* slots, st =
    fold_m st
      (fun st slots i ->
        if Int_map.mem i slots then Ok (slots, st)
        else
          let* d, st = dict_find st fn.kwdefaults (Str (pname i)) in
          match d with
          | Some v -> Ok (Int_map.add i v slots, st)
          | None -> Ok (slots, st))
      slots
      (List.init kwonly (fun k -> argc + k))
  in
  (* completeness — ref: 6.3.4 Calls: unfilled slots with no default raise
     TypeError. CPython reports all missing positional names first (then any
     missing keyword-only names), formatting the list as "'a'", "'a' and 'b'",
     or "'a', 'b', and 'c'". *)
  let names_of range =
    List.filter_map
      (fun i -> if Int_map.mem i slots then None else Some (pname i))
      range
  in
  let format_names ns =
    match ns with
    | [ a ] -> Printf.sprintf "'%s'" a
    | [ a; b ] -> Printf.sprintf "'%s' and '%s'" a b
    | _ ->
        let rec go = function
          | [ x ] -> Printf.sprintf "and '%s'" x
          | x :: rest -> Printf.sprintf "'%s', %s" x (go rest)
          | [] -> ""
        in
        go ns
  in
  let missing_pos = names_of (List.init argc (fun i -> i)) in
  let missing_kw = names_of (List.init kwonly (fun k -> argc + k)) in
  if missing_pos <> [] then
    raise_py st "TypeError"
      (err
         (Printf.sprintf "missing %d required positional argument%s: %s"
            (List.length missing_pos)
            (if List.length missing_pos = 1 then "" else "s")
            (format_names missing_pos)))
  else if missing_kw <> [] then
    raise_py st "TypeError"
      (err
         (Printf.sprintf "missing %d required keyword-only argument%s: %s"
            (List.length missing_kw)
            (if List.length missing_kw = 1 then "" else "s")
            (format_names missing_kw)))
  else Ok (slots, st)

(* ---------- frame execution ----------------------------------------- *)

and find_handler (table : Ast.exn_entry array) idx : Ast.exn_entry option =
  Array.find_opt
    (fun (e : Ast.exn_entry) -> e.start_idx <= idx && idx < e.end_idx)
    table

and drop n xs =
  if n <= 0 then xs else match xs with [] -> [] | _ :: t -> drop (n - 1) t

and run_frame st (f : frame) : frame_outcome r =
  match exec_instr st f f.code.instrs.(f.idx) with
  | Ok (Next f', st) -> run_frame st (advance f')
  | Ok (Goto (f', t), st) -> run_frame st { f' with idx = t }
  | Ok (Fin out, st) -> Ok (out, st)
  | Error (exc, st) -> (
      match find_handler f.code.exn_table f.idx with
      | None -> Error (exc, st)
      | Some e ->
          let stack = drop (List.length f.stack - e.depth) f.stack in
          let stack =
            if e.push_lasti then Int (Z.of_int f.idx) :: stack else stack
          in
          run_frame st { f with stack = exc :: stack; idx = e.target_idx })

and exec_instr st (f : frame) (ins : Phir.instr) : istep r =
  let op1 st f v =
    let* (vals, f), st = eval_operands st f [ v ] in
    Ok ((List.hd vals, f), st)
  in
  match ins with
  | Assign (x, v) ->
      let* (v, f), st = op1 st f v in
      let* f, st = store_var st f x v in
      Ok (Next f, st)
  | Delete x ->
      let* f, st = del_var st f x in
      Ok (Next f, st)
  | Push v ->
      let* (v, f), st = op1 st f v in
      Ok (Next (push f v), st)
  | Pop_top v ->
      let* (_, f), st = op1 st f v in
      Ok (Next f, st)
  | Copy n -> Ok (Next (push f (List.nth f.stack (n - 1))), st)
  | Swap n ->
      let top = List.hd f.stack and deep = List.nth f.stack (n - 1) in
      let stack =
        List.mapi
          (fun i v -> if i = 0 then deep else if i = n - 1 then top else v)
          f.stack
      in
      Ok (Next { f with stack }, st)
  | Unary (op, v) ->
      let* (v, f), st = op1 st f v in
      let* r, st = unary st op v in
      Ok (Next (push f r), st)
  | Binary_op { op; inplace; l; r } -> (
      let* (vals, f), st = eval_operands st f [ l; r ] in
      match vals with
      | [ a; b ] ->
          let* v, st = binary st op ~inplace a b in
          Ok (Next (push f v), st)
      | _ -> assert false)
  | Compare { op; coerce_bool; l; r } -> (
      (* ref: 6.10.1 — keep the raw comparison result unless the bytecode asks
         for bool coercion (boolean context / chained comparison). *)
      let* (vals, f), st = eval_operands st f [ l; r ] in
      match vals with
      | [ a; b ] ->
          let* v, st = py_compare_value st op a b in
          let* v, st =
            if coerce_bool then
              let* t, st = py_truth st v in
              Ok (Bool t, st)
            else Ok (v, st)
          in
          Ok (Next (push f v), st)
      | _ -> assert false)
  | Is_op { invert; l; r } -> (
      let* (vals, f), st = eval_operands st f [ l; r ] in
      match vals with
      | [ a; b ] ->
          let same =
            match (a, b) with
            | Ref x, Ref y -> x = y
            | Null, Null -> true
            | Ref _, _ | _, Ref _ -> false
            | x, y -> x = y (* immutables: identity unspecified; use equality *)
          in
          Ok (Next (push f (Bool (if invert then not same else same))), st)
      | _ -> assert false)
  | Contains_op { invert; item; seq } -> (
      let* (vals, f), st = eval_operands st f [ item; seq ] in
      match vals with
      | [ item; seq ] ->
          let* m, st = contains st item seq in
          Ok (Next (push f (Bool (if invert then not m else m))), st)
      | _ -> assert false)
  | Subscript (obj, index) -> (
      let* (vals, f), st = eval_operands st f [ obj; index ] in
      match vals with
      | [ obj; index ] ->
          let* v, st = subscript st obj index in
          Ok (Next (push f v), st)
      | _ -> assert false)
  | Store_subscr { value; obj; index } -> (
      let* (vals, f), st = eval_operands st f [ value; obj; index ] in
      match vals with
      | [ v; obj; index ] ->
          let* (), st = store_subscript st obj index v in
          Ok (Next f, st)
      | _ -> assert false)
  | Delete_subscr { obj; index } -> (
      let* (vals, f), st = eval_operands st f [ obj; index ] in
      match vals with
      | [ obj; index ] ->
          let* (), st = del_subscript st obj index in
          Ok (Next f, st)
      | _ -> assert false)
  | Binary_slice { obj; start; stop } -> (
      let* (vals, f), st = eval_operands st f [ obj; start; stop ] in
      match vals with
      | [ obj; start; stop ] ->
          let* v, st = subscript st obj (Slice (start, stop, None_)) in
          Ok (Next (push f v), st)
      | _ -> assert false)
  | Store_slice { value; obj; start; stop } -> (
      let* (vals, f), st = eval_operands st f [ value; obj; start; stop ] in
      match vals with
      | [ v; obj; start; stop ] ->
          let* (), st = store_subscript st obj (Slice (start, stop, None_)) v in
          Ok (Next f, st)
      | _ -> assert false)
  | Load_attr { obj; name; meth } ->
      let* (obj, f), st = op1 st f obj in
      let* v, st = getattr_value st obj name in
      let f = push f v in
      Ok (Next (if meth then push f Null else f), st)
  | Load_super_attr { super = sv; cls; self; name; meth; two_arg = _ } -> (
      let* (vals, f), st = eval_operands st f [ sv; cls; self ] in
      match vals with
      | [ _; Ref cls_addr; self ] ->
          let* v, st = super_getattr st ~cls:cls_addr ~self name in
          let f = push f v in
          Ok (Next (if meth then push f Null else f), st)
      | _ -> raise_py st "RuntimeError" "malformed super()")
  | Store_attr { value; obj; name } -> (
      let* (vals, f), st = eval_operands st f [ value; obj ] in
      match vals with
      | [ v; obj ] ->
          let* (), st = setattr_value st obj name v in
          Ok (Next f, st)
      | _ -> assert false)
  | Delete_attr { obj; name } ->
      let* (obj, f), st = op1 st f obj in
      let* (), st = delattr_value st obj name in
      Ok (Next f, st)
  | Call { f = fv; self; args } -> (
      let* (vals, f), st =
        eval_operands st f (fv :: self :: Array.to_list args)
      in
      match vals with
      | callee :: selfv :: argv ->
          let argv = if selfv = Null then argv else selfv :: argv in
          let* v, st = call st callee argv [] in
          Ok (Next (push f v), st)
      | _ -> assert false)
  | Call_kw { f = fv; self; args; kw_names } -> (
      let* (vals, f), st =
        eval_operands st f ((fv :: self :: Array.to_list args) @ [ kw_names ])
      in
      match vals with
      | callee :: selfv :: rest ->
          let argv, kw_names =
            match List.rev rest with
            | Tuple names :: rargs -> (List.rev rargs, names)
            | _ -> assert false
          in
          let n_kw = List.length kw_names in
          let pos, kwvals = take (List.length argv - n_kw) argv in
          let kwargs =
            List.map2
              (fun k v -> match k with Str s -> (s, v) | _ -> assert false)
              kw_names kwvals
          in
          let pos = if selfv = Null then pos else selfv :: pos in
          let* v, st = call st callee pos kwargs in
          Ok (Next (push f v), st)
      | _ -> assert false)
  | Call_ex { f = fv; null; args; kwargs } -> (
      let ops = [ fv; null; args ] @ Option.to_list kwargs in
      let* (vals, f), st = eval_operands st f ops in
      match vals with
      | callee :: _ :: argv :: rest ->
          let* args, st = to_list st argv in
          let* kwargs, st =
            match rest with
            | [] -> Ok ([], st)
            | [ kw ] -> (
                match deref st kw with
                | Some (Dict ps) ->
                    Ok
                      ( List.filter_map
                          (function Str k, v -> Some (k, v) | _ -> None)
                          ps,
                        st )
                | _ -> raise_py st "TypeError" "** argument must be a dict")
            | _ -> assert false
          in
          let* v, st = call st callee args kwargs in
          Ok (Next (push f v), st)
      | _ -> assert false)
  | Intrinsic_1 (id, v) -> (
      let* (v, f), st = op1 st f v in
      match id with
      | Unary_positive ->
          if is_number v then Ok (Next (push f v), st)
          else raise_py st "TypeError" "bad operand for unary +"
      | List_to_tuple -> (
          match deref st v with
          | Some (List xs) -> Ok (Next (push f (Tuple xs)), st)
          | _ -> raise_py st "TypeError" "expected a list")
      | Stopiteration_error ->
          let* is_stop, st = exc_is st v "StopIteration" in
          if not is_stop then Ok (Next (push f v), st)
          else
            let cls = builtin_class_addr st "RuntimeError" in
            let exc, st =
              make_exc st cls [ Str "generator raised StopIteration" ]
            in
            Ok (Next (push f exc), st)
      (* ref: 7.14 (PEP 695) — `type X = ...` builds a typing.TypeAliasType from
         (name, type_params, value-computing-function); the value is evaluated
         lazily on __value__ access. *)
      | Typealias -> (
          match v with
          | Tuple [ Str name; type_params; value_fn ] ->
              let ta, st =
                alloc st
                  (Type_alias
                     {
                       ta_name = name;
                       ta_value = value_fn;
                       ta_type_params =
                         (match type_params with None_ -> Tuple [] | t -> t);
                     })
              in
              Ok (Next (push f ta), st)
          | _ -> unsupported st "typealias")
      (* ref: 8.10 (PEP 695) — a `[T]` type-parameter list creates a TypeVar *)
      | Typevar -> (
          match v with
          | Str name ->
              let tv, st =
                alloc st
                  (Typevar
                     {
                       tv_name = name;
                       tv_bound = None_;
                       tv_constraints = None_;
                     })
              in
              Ok (Next (push f tv), st)
          | _ -> unsupported st "typevar")
      | _ -> unsupported st "intrinsic")
  | Intrinsic_2 (Prep_reraise_star, orig, excs) -> (
      (* ref: 8.4 — at the end of a try/except*, combine the leftover/handler
         exceptions collected per clause into one to reraise: none -> None, one
         -> itself, several -> a new ExceptionGroup("", excs). *)
      let* (vals, f), st = eval_operands st f [ orig; excs ] in
      match vals with
      | [ _orig; excs ] -> (
          let items =
            match deref st excs with Some (List xs) -> xs | _ -> []
          in
          let raised = List.filter (fun e -> e <> None_) items in
          match raised with
          | [] -> Ok (Next (push f None_), st)
          | [ e ] -> Ok (Next (push f e), st)
          | _ ->
              let lst, st = alloc st (List raised) in
              let* g, st =
                instantiate st
                  (builtin_class_addr st "ExceptionGroup")
                  [ Str ""; lst ] []
              in
              Ok (Next (push f g), st))
      | _ -> assert false)
  | Intrinsic_2 (Set_function_type_params, fv, pv) -> (
      (* ref: 8.10 — attach __type_params__ to a function (returns the func) *)
      let* (vals, f), st = eval_operands st f [ fv; pv ] in
      match vals with
      | [ (Ref fa as func); params ] -> (
          match heap_get st fa with
          | Func fn ->
              let* (), st =
                dict_set st fn.fdict (Str "__type_params__") params
              in
              Ok (Next (push f func), st)
          | _ -> Ok (Next (push f func), st))
      | _ -> assert false)
  | Intrinsic_2 (Typevar_with_bound, nv, bv) -> (
      (* ref: 8.10 — `[T: bound]`: (name, lazy-bound-function) -> TypeVar *)
      let* (vals, f), st = eval_operands st f [ nv; bv ] in
      match vals with
      | [ Str name; bound ] ->
          let tv, st =
            alloc st
              (Typevar
                 { tv_name = name; tv_bound = bound; tv_constraints = None_ })
          in
          Ok (Next (push f tv), st)
      | _ -> assert false)
  | Intrinsic_2 (Typevar_with_constraints, nv, cv) -> (
      (* ref: 8.10 — `[T: (int, str)]`: (name, lazy-constraints-function) *)
      let* (vals, f), st = eval_operands st f [ nv; cv ] in
      match vals with
      | [ Str name; constraints ] ->
          let tv, st =
            alloc st
              (Typevar
                 {
                   tv_name = name;
                   tv_bound = None_;
                   tv_constraints = constraints;
                 })
          in
          Ok (Next (push f tv), st)
      | _ -> assert false)
  | Intrinsic_2 _ -> unsupported st "intrinsic_2"
  | Jump t -> Ok (Goto (f, t), st)
  | Cond_jump { cond; v; target } ->
      let* (v, f), st = op1 st f v in
      let* b, st =
        match cond with
        | If_true -> py_truth st v
        | If_false ->
            let* t, st = py_truth st v in
            Ok (not t, st)
        | If_none -> Ok (v = None_, st)
        | If_not_none -> Ok (v <> None_, st)
      in
      if b then Ok (Goto (f, target), st) else Ok (Next f, st)
  | Get_iter v ->
      let* (v, f), st = op1 st f v in
      let* it, st = py_iter st v in
      Ok (Next (push f it), st)
  | For_iter target -> (
      let it = List.hd f.stack in
      let* nx, st = py_next st it in
      match nx with
      | Some v -> Ok (Next (push f v), st)
      | None -> Ok (Goto (push f None_, target), st))
  | End_for ->
      let _, f = pop f in
      Ok (Next f, st)
  | Get_yield_from_iter v -> (
      let* (v, f), st = op1 st f v in
      match deref st v with
      | Some (Gen _) -> Ok (Next (push f v), st)
      | _ ->
          let* it, st = py_iter st v in
          Ok (Next (push f it), st))
  | Return v ->
      let* (v, f), st = op1 st f v in
      ignore f;
      Ok (Fin (Returned v), st)
  | Return_generator ->
      let kind =
        if Ast.is_async_generator f.code then `Async_gen
        else if Ast.is_coroutine f.code then `Coroutine
        else `Gen
      in
      let g, st =
        alloc st
          (Gen { gframe = Some (advance f); gstarted = false; gkind = kind })
      in
      Ok (Fin (Returned g), st)
  | Yield { v; arg = _ } ->
      let* (v, f), st = op1 st f v in
      Ok (Fin (Yielded (v, advance f)), st)
  | Send { v; on_stop } -> (
      let* (v, f), st = op1 st f v in
      let receiver = List.hd f.stack in
      match deref st receiver with
      | Some (Gen _) -> (
          let* step, st = gen_resume st (addr receiver) v in
          match step with
          | `Yield y -> Ok (Next (push f y), st)
          | `Return rv -> Ok (Goto (push f rv, on_stop), st))
      | _ -> (
          let* nx, st = py_next st receiver in
          match nx with
          | Some y -> Ok (Next (push f y), st)
          | None -> Ok (Goto (push f None_, on_stop), st)))
  | End_send ->
      let v, f = pop f in
      let _, f = pop f in
      Ok (Next (push f v), st)
  | Cleanup_throw -> unsupported st "Cleanup_throw"
  | Raise { exc; cause } -> (
      let* (vals, f), st =
        eval_operands st f (Option.to_list exc @ Option.to_list cause)
      in
      ignore f;
      match (exc, vals) with
      | None, _ ->
          (* ref: 7.8 — a bare raise re-raises the active exception, else errors *)
          if st.cur_exc = None_ then
            raise_py st "RuntimeError" "No active exception to reraise"
          else Error (st.cur_exc, st)
      | Some _, [ v ] ->
          (* ref: 7.8 — implicit chaining: the active exception (if any) becomes
             the new exception's __context__ *)
          let* excv, st = exception_instance st v in
          let* (), st = set_exc_chain st excv ~cause:None_ ~suppress:false in
          Error (excv, st)
      | Some _, [ v; c ] ->
          (* ref: 7.8 — `from` sets __cause__ (None is allowed) and suppresses
             the context display *)
          let* excv, st = exception_instance st v in
          let* cv, st =
            if c = None_ then Ok (None_, st) else exception_instance st c
          in
          let* (), st = set_exc_chain st excv ~cause:cv ~suppress:true in
          Error (excv, st)
      | _ -> assert false)
  | Reraise _ ->
      let exc, f = pop f in
      ignore f;
      Error (exc, st)
  | Push_exc_info ->
      let v, f = pop f in
      let f = push (push f st.cur_exc) v in
      Ok (Next f, { st with cur_exc = v })
  | Pop_except ->
      let v, f = pop f in
      Ok (Next f, { st with cur_exc = v })
  | Check_exc_match pat ->
      let* (pat, f), st = op1 st f pat in
      let exc = List.hd f.stack in
      let* m, st = isinstance_value st exc pat in
      Ok (Next (push f (Bool m)), st)
  | Check_eg_match { exc; pattern } -> (
      (* ref: 8.4 — except* matching: split the (group) exception into the part
         matching [pattern] and the rest, pushing rest then match (match on
         top). A bare exception that matches is wrapped in a one-element group;
         a non-match leaves match=None and rest=the exception. *)
      let* (vals, f), st = eval_operands st f [ exc; pattern ] in
      match vals with
      | [ exc; pattern ] ->
          let* (m, rest), st = eg_match st exc pattern in
          Ok (Next (push (push f rest) m), st)
      | _ -> assert false)
  | With_except_start ->
      let exit_func = List.nth f.stack 3 in
      let exc = List.hd f.stack in
      let* cls, st = class_of_value st exc in
      let* res, st = call st exit_func [ cls; exc; None_ ] [] in
      Ok (Next (push f res), st)
  | Build_tuple vs ->
      let* (vals, f), st = eval_operands st f (Array.to_list vs) in
      Ok (Next (push f (Tuple vals)), st)
  | Build_list vs ->
      let* (vals, f), st = eval_operands st f (Array.to_list vs) in
      let l, st = alloc st (List vals) in
      Ok (Next (push f l), st)
  | Build_set vs ->
      let* (vals, f), st = eval_operands st f (Array.to_list vs) in
      let* elems, st = dedup_set st vals in
      let s, st = alloc st (Set elems) in
      Ok (Next (push f s), st)
  | Build_map pairs ->
      let ops =
        List.concat_map (fun (k, v) -> [ k; v ]) (Array.to_list pairs)
      in
      let* (vals, f), st = eval_operands st f ops in
      let d, st = alloc st (Dict []) in
      let rec fill st = function
        | [] -> Ok ((), st)
        | k :: v :: rest ->
            let* (), st = dict_set st (addr d) k v in
            fill st rest
        | _ -> assert false
      in
      let* (), st = fill st vals in
      Ok (Next (push f d), st)
  | Build_string vs ->
      let* (vals, f), st = eval_operands st f (Array.to_list vs) in
      let parts = List.map (function Str s -> s | _ -> "") vals in
      Ok (Next (push f (Str (String.concat "" parts))), st)
  | Build_slice vs -> (
      let* (vals, f), st = eval_operands st f (Array.to_list vs) in
      match vals with
      | [ a; b ] -> Ok (Next (push f (Slice (a, b, None_))), st)
      | [ a; b; c ] -> Ok (Next (push f (Slice (a, b, c))), st)
      | _ -> assert false)
  | Build_const_key_map { keys; values } -> (
      let* (vals, f), st =
        eval_operands st f (Array.to_list values @ [ keys ])
      in
      match List.rev vals with
      | Tuple keys :: rvals ->
          let vals = List.rev rvals in
          let d, st = alloc st (Dict []) in
          let* (), st =
            fold_m st
              (fun st () (k, v) -> dict_set st (addr d) k v)
              () (List.combine keys vals)
          in
          Ok (Next (push f d), st)
      | _ -> assert false)
  | List_append (depth, v) -> (
      let* (v, f), st = op1 st f v in
      match List.nth f.stack (depth - 1) with
      | Ref a -> (
          match heap_get st a with
          | List xs -> Ok (Next f, heap_set st a (List (xs @ [ v ])))
          | _ -> assert false)
      | _ -> assert false)
  | Set_add (depth, v) -> (
      let* (v, f), st = op1 st f v in
      match List.nth f.stack (depth - 1) with
      | Ref a -> (
          match heap_get st a with
          | Set xs ->
              let* m, st = set_mem st xs v in
              let st = if m then st else heap_set st a (Set (xs @ [ v ])) in
              Ok (Next f, st)
          | _ -> assert false)
      | _ -> assert false)
  | Map_add (depth, k, v) -> (
      let* (vals, f), st = eval_operands st f [ k; v ] in
      match (vals, List.nth f.stack (depth - 1)) with
      | [ k; v ], Ref a ->
          let* (), st = dict_set st a k v in
          Ok (Next f, st)
      | _ -> assert false)
  | List_extend (depth, v) -> (
      let* (v, f), st = op1 st f v in
      let* items, st = to_list st v in
      match List.nth f.stack (depth - 1) with
      | Ref a -> (
          match heap_get st a with
          | List xs -> Ok (Next f, heap_set st a (List (xs @ items)))
          | _ -> assert false)
      | _ -> assert false)
  | Set_update (depth, v) -> (
      let* (v, f), st = op1 st f v in
      let* items, st = to_list st v in
      match List.nth f.stack (depth - 1) with
      | Ref a -> (
          match heap_get st a with
          | Set xs ->
              let* merged, st = dedup_set st (xs @ items) in
              Ok (Next f, heap_set st a (Set merged))
          | _ -> assert false)
      | _ -> assert false)
  | Dict_update (depth, v) | Dict_merge (depth, v) -> (
      let* (v, f), st = op1 st f v in
      match (deref st v, List.nth f.stack (depth - 1)) with
      | Some (Dict ps), Ref a ->
          let* (), st =
            fold_m st (fun st () (k, x) -> dict_set st a k x) () ps
          in
          Ok (Next f, st)
      | _ -> raise_py st "TypeError" "argument must be a mapping")
  | Unpack_sequence (n, v) ->
      (* ref: 7.2 Assignment statements — the iterable must have exactly n items;
         CPython distinguishes too-many from not-enough. *)
      let* (v, f), st = op1 st f v in
      let* items, st = to_list st v in
      let m = List.length items in
      if m > n then
        raise_py st "ValueError"
          (Printf.sprintf "too many values to unpack (expected %d)" n)
      else if m < n then
        raise_py st "ValueError"
          (Printf.sprintf "not enough values to unpack (expected %d, got %d)" n
             m)
      else Ok (Next { f with stack = items @ f.stack }, st)
  | Unpack_ex { before; after; v } ->
      (* ref: 7.2 — a starred target needs at least before+after items *)
      let* (v, f), st = op1 st f v in
      let* items, st = to_list st v in
      if List.length items < before + after then
        raise_py st "ValueError"
          (Printf.sprintf
             "not enough values to unpack (expected at least %d, got %d)"
             (before + after) (List.length items))
      else
        let bs, rest = take before items in
        let mid, asx = take (List.length rest - after) rest in
        let star, st = alloc st (List mid) in
        Ok (Next { f with stack = bs @ (star :: asx) @ f.stack }, st)
  | Format_simple v ->
      (* f"{x}" is format(x, "") — for an instance this calls __format__(""),
         which only coincides with str(x) for the object default. *)
      let* (v, f), st = op1 st f v in
      let* s, st = format_value st v "" in
      Ok (Next (push f (Str s)), st)
  | Format_with_spec (v, spec) -> (
      let* (vals, f), st = eval_operands st f [ v; spec ] in
      match vals with
      | [ v; Str spec ] ->
          let* s, st = format_value st v spec in
          Ok (Next (push f (Str s)), st)
      | _ -> assert false)
  | Convert_value (conv, v) ->
      let* (v, f), st = op1 st f v in
      let* s, st =
        match conv with
        | Str_conv -> py_str st v
        | Repr_conv | Ascii_conv -> py_repr st v
      in
      Ok (Next (push f (Str s)), st)
  | Make_function v -> (
      let* (v, f), st = op1 st f v in
      match v with
      | Code_obj code ->
          let fdict, st = alloc st (Dict []) in
          let fn, st =
            alloc st
              (Func
                 {
                   code;
                   globals = f.globals;
                   defaults = [];
                   kwdefaults = [];
                   closure = [];
                   fdict = addr fdict;
                 })
          in
          Ok (Next (push f fn), st)
      | _ -> raise_py st "TypeError" "MAKE_FUNCTION expects a code object")
  | Set_function_attribute { attr; v; f = fv } -> (
      let* (vals, f), st = eval_operands st f [ v; fv ] in
      match vals with
      | [ v; (Ref fa as fref) ] -> (
          match heap_get st fa with
          | Func fn ->
              let* fn, st =
                match attr with
                | Defaults -> (
                    match v with
                    | Tuple ds -> Ok ({ fn with defaults = ds }, st)
                    | _ -> raise_py st "TypeError" "defaults must be a tuple")
                | Kw_defaults -> (
                    match deref st v with
                    | Some (Dict ps) -> Ok ({ fn with kwdefaults = ps }, st)
                    | _ -> raise_py st "TypeError" "kwdefaults must be a dict")
                | Closure -> (
                    match v with
                    | Tuple cells -> Ok ({ fn with closure = cells }, st)
                    | _ -> raise_py st "TypeError" "closure must be a tuple")
                | Annotations -> (
                    (* ref: 8.7 — annotations arrive as a flat [k1;v1;k2;v2;…]
                       tuple; store them as the function's __annotations__ dict *)
                    match v with
                    | Tuple kvs ->
                        let rec pairs = function
                          | a :: b :: rest -> (a, b) :: pairs rest
                          | _ -> []
                        in
                        let d, st = alloc st (Dict (pairs kvs)) in
                        let* (), st =
                          dict_set st fn.fdict (Str "__annotations__") d
                        in
                        Ok (fn, st)
                    | _ -> Ok (fn, st))
              in
              Ok (Next (push f fref), heap_set st fa (Func fn))
          | _ -> assert false)
      | _ -> assert false)
  | Make_cell i ->
      let existing = Int_map.find_opt i f.slots in
      let cell, st = alloc st (Cell existing) in
      Ok (Next { f with slots = Int_map.add i cell f.slots }, st)
  | Copy_free_vars _ ->
      let free_slots =
        Array.to_list f.code.localsplus
        |> List.mapi (fun i (_, k) -> (i, k))
        |> List.filter_map (fun (i, k) -> if k = Ast.Free then Some i else None)
      in
      let slots =
        List.fold_left2
          (fun slots i cell -> Int_map.add i cell slots)
          f.slots free_slots f.closure
      in
      Ok (Next { f with slots }, st)
  | Load_build_class -> Ok (Next (push f (Builtin "__build_class__")), st)
  | Load_assertion_error ->
      Ok (Next (push f (Ref (builtin_class_addr st "AssertionError"))), st)
  | Setup_annotations -> (
      let* found, st = dget st f.ns (Str "__annotations__") in
      match found with
      | Some _ -> Ok (Next f, st)
      | None ->
          let d, st = alloc st (Dict []) in
          let* (), st = dict_set st f.ns (Str "__annotations__") d in
          Ok (Next f, st))
  | Load_locals -> Ok (Next (push f (Ref f.ns)), st)
  | Load_from_dict_or_globals (v, name) -> (
      let* (d, f), st = op1 st f v in
      let* found, st = dget st (addr d) (Str name) in
      match found with
      | Some v -> Ok (Next (push f v), st)
      | None ->
          let* v, st =
            name_chain_lookup st f (fun f -> [ f.globals; st.builtins ]) name
          in
          Ok (Next (push f v), st))
  | Load_from_dict_or_deref (v, slot) -> (
      let* (d, f), st = op1 st f v in
      let name = fst f.code.localsplus.(slot) in
      let* found, st = dget st (addr d) (Str name) in
      match found with
      | Some v -> Ok (Next (push f v), st)
      | None ->
          let* v, st = load_var st f (Deref slot) in
          Ok (Next (push f v), st))
  | Load_fast_and_clear i ->
      let v = Option.value (Int_map.find_opt i f.slots) ~default:Null in
      Ok (Next (push { f with slots = Int_map.remove i f.slots } v), st)
  | Import_name _ | Import_from _ ->
      raise_py st "ImportError" "imports are not supported"
  | Get_awaitable _ | Get_aiter _ | Get_anext | End_async_for
  | Before_async_with _ ->
      unsupported st "async"
  | Before_with v -> (
      let* (mgr, f), st = op1 st f v in
      let* exit_m, st = find_dunder st mgr "__exit__" in
      let* enter_m, st = find_dunder st mgr "__enter__" in
      match (exit_m, enter_m) with
      | Some exit_f, Some enter_f ->
          let* entered, st = call st enter_f [] [] in
          Ok (Next (push (push f exit_f) entered), st)
      | _ ->
          raise_py st "TypeError"
            (Printf.sprintf
               "'%s' object does not support the context manager protocol"
               (type_name st mgr)))
  | Match_class { count; subject; cls; names } -> (
      let* (vals, f), st = eval_operands st f [ subject; cls; names ] in
      match vals with
      | [ subject; cls; Tuple kw_names ] -> (
          let* ok, st = isinstance_value st subject cls in
          if not ok then Ok (Next (push f None_), st)
          else
            let* pos_names, st =
              if count = 0 then Ok ([], st)
              else
                match cls with
                | Ref ca -> (
                    let* ma, st = type_lookup st ca "__match_args__" in
                    match ma with
                    | Some (Tuple names) when List.length names >= count ->
                        let firsts, _ = take count names in
                        Ok
                          (List.map (function Str s -> s | _ -> "") firsts, st)
                    | _ ->
                        (* ref: 3.3.10 — more positional sub-patterns than
                           __match_args__ provides (absent ≡ ()) is a TypeError *)
                        let n =
                          match ma with
                          | Some (Tuple names) -> List.length names
                          | _ -> 0
                        in
                        raise_py st "TypeError"
                          (Printf.sprintf
                             "%s() accepts %d positional sub-pattern%s (%d \
                              given)"
                             (cls_of st ca).cname n
                             (if n = 1 then "" else "s")
                             count))
                | _ -> Ok ([], st)
            in
            let all_names =
              pos_names @ List.map (function Str s -> s | _ -> "") kw_names
            in
            let rec gather st acc = function
              | [] -> Ok (Some (List.rev acc), st)
              | n :: rest -> (
                  match getattr_value st subject n with
                  | Ok (v, st) -> gather st (v :: acc) rest
                  | Error (exc, st) ->
                      let* is_attr, st = exc_is st exc "AttributeError" in
                      if is_attr then Ok (None, st) else Error (exc, st))
            in
            let* gathered, st = gather st [] all_names in
            match gathered with
            | Some vs -> Ok (Next (push f (Tuple vs)), st)
            | None -> Ok (Next (push f None_), st))
      | _ -> assert false)
  | Match_mapping ->
      let is_map =
        match deref st (List.hd f.stack) with
        | Some (Dict _) -> true
        | _ -> false
      in
      Ok (Next (push f (Bool is_map)), st)
  | Match_sequence ->
      let is_seq =
        match List.hd f.stack with
        | Tuple _ -> true
        | v -> ( match deref st v with Some (List _) -> true | _ -> false)
      in
      Ok (Next (push f (Bool is_seq)), st)
  | Match_keys -> (
      let keys = List.hd f.stack and subject = List.nth f.stack 1 in
      match (keys, deref st subject) with
      | Tuple ks, Some (Dict ps) ->
          let rec gather st acc = function
            | [] -> Ok (Some (List.rev acc), st)
            | k :: rest -> (
                let* found, st = dict_find st ps k in
                match found with
                | Some v -> gather st (v :: acc) rest
                | None -> Ok (None, st))
          in
          let* gathered, st = gather st [] ks in
          Ok
            ( Next
                (push f
                   (match gathered with Some vs -> Tuple vs | None -> None_)),
              st )
      | _ -> Ok (Next (push f None_), st))
  | Get_len ->
      let* n, st = py_len st (List.hd f.stack) in
      Ok (Next (push f (Int (Z.of_int n))), st)

and dedup_set st vals : value list r =
  fold_m st
    (fun st acc v ->
      let* (), st = check_hashable st v in
      let* m, st = set_mem st acc v in
      Ok ((if m then acc else acc @ [ v ]), st))
    [] vals

and exception_instance st (v : value) : value r =
  match deref st v with
  | Some (Class _) -> call st v [] []
  | Some (Instance _) -> Ok (v, st)
  | _ -> raise_py st "TypeError" "exceptions must derive from BaseException"

and set_exc_attr st exc name v : unit r =
  match deref st exc with
  | Some (Instance { dict; _ }) -> dict_set st dict (Str name) v
  | _ -> Ok ((), st)

(* ref: 7.8 The raise statement — set the chaining attributes on a freshly
   raised exception: __context__ is the active exception (implicit chaining),
   __cause__ is the explicit `from` value, and __suppress_context__ records
   whether `from` was used. *)
and set_exc_chain st excv ~cause ~suppress : unit r =
  let ctx = if st.cur_exc = excv then None_ else st.cur_exc in
  let* (), st = set_exc_attr st excv "__context__" ctx in
  let* (), st = set_exc_attr st excv "__cause__" cause in
  set_exc_attr st excv "__suppress_context__" (Bool suppress)

and class_of_value st (v : value) : value r =
  match deref st v with
  | Some (Instance { cls; _ }) -> Ok (Ref cls, st)
  (* ref: 3.3.3 — type(C) is C's metaclass (the default [type] unless set) *)
  | Some (Class _) -> Ok (Ref (metaclass_addr st (addr v)), st)
  (* ref: 3.3.5/6.7/7.14 — these report a short __name__ ("GenericAlias", ...)
     distinct from the qualified name used in error messages *)
  | Some (Generic_alias _) -> Ok (Ref (builtin_class_addr st "GenericAlias"), st)
  | Some (Union_type _) -> Ok (Ref (builtin_class_addr st "UnionType"), st)
  | Some (Type_alias _) -> Ok (Ref (builtin_class_addr st "TypeAliasType"), st)
  | Some (Typevar _) -> Ok (Ref (builtin_class_addr st "TypeVar"), st)
  | _ -> Ok (Ref (builtin_class_addr st (type_name st v)), st)

(* ---------- string helpers ----------------------------------------- *)

and is_space c =
  c = ' ' || c = '\t' || c = '\n' || c = '\r' || c = '\x0b' || c = '\x0c'

(* Byte index of the first occurrence of [needle] at or after [from]. *)
and find_substring ?(from = 0) hay needle : int option =
  let lh = String.length hay and ln = String.length needle in
  let rec go i =
    if i + ln > lh then None
    else if String.sub hay i ln = needle then Some i
    else go (i + 1)
  in
  go from

and split_on_sep s sep =
  let ls = String.length sep in
  let rec go i acc =
    match find_substring ~from:i s sep with
    | None -> List.rev (String.sub s i (String.length s - i) :: acc)
    | Some j -> go (j + ls) (String.sub s i (j - i) :: acc)
  in
  go 0 []

(* split on [sep] at most [maxsplit] times from the left (maxsplit < 0 = all) *)
and split_on_sep_max s sep maxsplit =
  if maxsplit < 0 then split_on_sep s sep
  else
    let ls = String.length sep in
    let rec go i n acc =
      if n >= maxsplit then
        List.rev (String.sub s i (String.length s - i) :: acc)
      else
        match find_substring ~from:i s sep with
        | None -> List.rev (String.sub s i (String.length s - i) :: acc)
        | Some j -> go (j + ls) (n + 1) (String.sub s i (j - i) :: acc)
    in
    go 0 0 []

(* whitespace split, at most [maxsplit] times from the left *)
and split_whitespace_max s maxsplit =
  if maxsplit < 0 then split_whitespace s
  else
    let n = String.length s in
    let rec skip i = if i < n && is_space s.[i] then skip (i + 1) else i in
    let rec word i =
      if i < n && not (is_space s.[i]) then word (i + 1) else i
    in
    let rec go i cnt acc =
      let i = skip i in
      if i >= n then List.rev acc
      else if cnt >= maxsplit then List.rev (String.sub s i (n - i) :: acc)
      else
        let e = word i in
        go e (cnt + 1) (String.sub s i (e - i) :: acc)
    in
    go 0 0 []

(* byte offset of the last occurrence of [sub] in [s] *)
and rfind_substring s sub =
  if sub = "" then Some (String.length s)
  else
    let rec go i last =
      match find_substring ~from:i s sub with
      | Some j -> go (j + 1) (Some j)
      | None -> last
    in
    go 0 None

(* str.istitle: at least one cased char, each word's first cased char upper and
   the rest lower (ASCII) *)
and is_titlecased s =
  let upper c = c >= 'A' && c <= 'Z' in
  let lower c = c >= 'a' && c <= 'z' in
  let rec go i prev_cased any_cased ok =
    if i >= String.length s then ok && any_cased
    else
      let c = s.[i] in
      if upper c then
        if prev_cased then go (i + 1) true any_cased false
        else go (i + 1) true true ok
      else if lower c then
        if prev_cased then go (i + 1) true true ok
        else go (i + 1) true true false
      else go (i + 1) false any_cased ok
  in
  go 0 false false true

(* str.expandtabs: replace tabs by spaces to the next multiple of [w] *)
and expand_tabs s w =
  let rec go i col acc =
    if i >= String.length s then String.concat "" (List.rev acc)
    else
      match s.[i] with
      | '\t' ->
          let pad = if w <= 0 then 0 else w - (col mod w) in
          go (i + 1) (col + pad) (String.make pad ' ' :: acc)
      | ('\n' | '\r') as c -> go (i + 1) 0 (String.make 1 c :: acc)
      | c -> go (i + 1) (col + 1) (String.make 1 c :: acc)
  in
  go 0 0 []

(* str.translate: map each codepoint through a table (dict of int->str|int|None) *)
and str_translate st s table =
  let pairs = match deref st table with Some (Dict ps) -> ps | _ -> [] in
  let lookup cp =
    List.find_map
      (fun (k, v) ->
        match k with Int z when Z.to_int z = cp -> Some v | _ -> None)
      pairs
  in
  let rec go i acc st =
    if i >= String.length s then Ok (String.concat "" (List.rev acc), st)
    else
      let cp, n = utf8_decode_at s i in
      let piece, st =
        match lookup cp with
        | Some (Str r) -> (r, st)
        | Some (Int z) -> (utf8_encode (Z.to_int z), st)
        | Some None_ -> ("", st)
        | _ -> (String.sub s i n, st)
      in
      go (i + n) (piece :: acc) st
  in
  go 0 [] st

and split_whitespace s =
  let n = String.length s in
  let rec go i acc cur =
    if i >= n then List.rev (if cur = "" then acc else cur :: acc)
    else if is_space s.[i] then
      go (i + 1) (if cur = "" then acc else cur :: acc) ""
    else go (i + 1) acc (cur ^ String.make 1 s.[i])
  in
  go 0 [] ""

and string_trim ~left ~right s =
  let n = String.length s in
  let rec lo i = if i < n && left && is_space s.[i] then lo (i + 1) else i in
  let rec hi i =
    if i > 0 && right && is_space s.[i - 1] then hi (i - 1) else i
  in
  let a = lo 0 in
  let b = hi n in
  if b <= a then "" else String.sub s a (b - a)

and count_nonoverlap s sub =
  if sub = "" then utf8_length s + 1
  else
    let rec go i acc =
      match find_substring ~from:i s sub with
      | None -> acc
      | Some j -> go (j + String.length sub) (acc + 1)
    in
    go 0 0

and replace_substring s old_s new_s limit =
  if old_s = "" then s
  else
    let rec go i n =
      if n = 0 then String.sub s i (String.length s - i)
      else
        match find_substring ~from:i s old_s with
        | None -> String.sub s i (String.length s - i)
        | Some j ->
            String.sub s i (j - i) ^ new_s ^ go (j + String.length old_s) (n - 1)
    in
    go 0 limit

and title_case s =
  let chars = List.of_seq (String.to_seq s) in
  let is_alpha c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') in
  let _, out =
    List.fold_left
      (fun (prev_alpha, acc) c ->
        let c' =
          if is_alpha c then
            if prev_alpha then Char.lowercase_ascii c
            else Char.uppercase_ascii c
          else c
        in
        (is_alpha c, acc ^ String.make 1 c'))
      (false, "") chars
  in
  out

(* "{}-{}", "{1}{0}" style templates (str.format). *)
and str_format st template args : string r =
  let n = String.length template in
  let rec go st i auto acc =
    if i >= n then Ok (acc, st)
    else
      match template.[i] with
      | '{' when i + 1 < n && template.[i + 1] = '{' ->
          go st (i + 2) auto (acc ^ "{")
      | '}' when i + 1 < n && template.[i + 1] = '}' ->
          go st (i + 2) auto (acc ^ "}")
      | '{' -> (
          match String.index_from_opt template i '}' with
          | None -> raise_py st "ValueError" "unmatched '{' in format string"
          | Some j ->
              let field = String.sub template (i + 1) (j - i - 1) in
              let idx, auto =
                if field = "" then (auto, auto + 1)
                else (int_of_string field, auto)
              in
              if idx >= List.length args then
                raise_py st "IndexError" "format index out of range"
              else
                (* "{}" is format(arg, "") — delegate to __format__ for instances *)
                let* s, st = format_value st (List.nth args idx) "" in
                go st (j + 1) auto (acc ^ s))
      | c -> go st (i + 1) auto (acc ^ String.make 1 c)
  in
  go st 0 0 ""

(* ---------- builtin call dispatch ----------------------------------- *)

and as_str st v what : string r =
  match v with
  | Str s -> Ok (s, st)
  | _ -> raise_py st "TypeError" (what ^ " expects a string")

and as_int st v what : int r =
  match as_z v with
  | Some z -> Ok (Z.to_int z, st)
  | None -> raise_py st "TypeError" (what ^ " expects an integer")

(* ref: 3.3.8 __index__ — losslessly interpret a value as an integer (used by
   hex/bin/oct and the like); a non-int instance is converted via __index__. *)
and to_index st v : Z.t r =
  match as_z v with
  | Some z -> Ok (z, st)
  | None -> (
      let* m, st = find_dunder st v "__index__" in
      match m with
      | Some f -> (
          let* r, st = call st f [] [] in
          match as_z r with
          | Some z -> Ok (z, st)
          | None ->
              raise_py st "TypeError"
                (Printf.sprintf "__index__ returned non-int (type %s)"
                   (type_name st r)))
      | None ->
          raise_py st "TypeError"
            (Printf.sprintf "'%s' object cannot be interpreted as an integer"
               (type_name st v)))

(* escape non-ASCII codepoints in a (UTF-8) repr string, as ascii() does *)
and ascii_escape s =
  let rec go i acc =
    if i >= String.length s then List.rev acc
    else
      let cp, n = utf8_decode_at s i in
      let piece =
        if cp < 0x80 then String.sub s i n
        else if cp <= 0xff then Printf.sprintf "\\x%02x" cp
        else if cp <= 0xffff then Printf.sprintf "\\u%04x" cp
        else Printf.sprintf "\\U%08x" cp
      in
      go (i + n) (piece :: acc)
  in
  String.concat "" (go 0 [])

(* round-half-to-even of a float (banker's rounding), as round()/__round__ *)
and round_half_even x =
  let fl = Float.floor x in
  let frac = x -. fl in
  if frac < 0.5 then fl
  else if frac > 0.5 then fl +. 1.
  else if Float.rem fl 2. = 0. then fl
  else fl +. 1.

(* round an integer to a multiple of 10^k (k > 0), round-half-to-even *)
and round_int_pow10 z k =
  let s = Z.pow (Z.of_int 10) k in
  let q = Z.div z s and r = Z.rem z s in
  let half2 = Z.mul (Z.of_int 2) (Z.abs r) in
  let bump =
    match Z.compare half2 s with
    | c when c > 0 -> true
    | c when c < 0 -> false
    | _ -> not (Z.equal (Z.rem q (Z.of_int 2)) Z.zero)
  in
  let q =
    if not bump then q
    else if Z.sign z >= 0 then Z.add q Z.one
    else Z.sub q Z.one
  in
  Z.mul q s

(* radix repr with CPython's prefixes: hex/bin/oct of an int, sign outside *)
and radix_repr prefix base z =
  let sign = if Z.sign z < 0 then "-" else "" in
  let digits =
    if Z.equal z Z.zero then "0"
    else
      let rec go acc z =
        if Z.equal z Z.zero then acc
        else
          let d = Z.to_int (Z.rem z (Z.of_int base)) in
          let c =
            if d < 10 then Char.chr (d + Char.code '0')
            else Char.chr (d - 10 + Char.code 'a')
          in
          go (String.make 1 c ^ acc) (Z.div z (Z.of_int base))
      in
      go "" (Z.abs z)
  in
  sign ^ prefix ^ digits

and pad_str s width fill ~left ~right =
  let len = utf8_length s in
  if len >= width then s
  else
    let pad = width - len in
    if left && right then
      let l = pad / 2 in
      String.make l fill ^ s ^ String.make (pad - l) fill
    else if left then String.make pad fill ^ s
    else s ^ String.make pad fill

and call_builtin st name (args : value list) (kwargs : (string * value) list) :
    value r =
  let kw k = List.assoc_opt k kwargs in
  let arity_error () =
    raise_py st "TypeError" (name ^ "(): wrong number of arguments")
  in
  match (name, args) with
  (* ---- functions ---- *)
  | "print", args ->
      let* parts, st = map_m st py_str args in
      let* sep, st =
        match kw "sep" with Some s -> py_str st s | None -> Ok (" ", st)
      in
      let* finish, st =
        match kw "end" with Some s -> py_str st s | None -> Ok ("\n", st)
      in
      Ok (None_, output st (String.concat sep parts ^ finish))
  | "len", [ v ] ->
      let* n, st = py_len st v in
      Ok (Int (Z.of_int n), st)
  | "repr", [ v ] ->
      let* s, st = py_repr st v in
      Ok (Str s, st)
  | "ascii", [ v ] ->
      (* ref: 6.x — ascii(x) is repr(x) with non-ASCII codepoints escaped *)
      let* s, st = py_repr st v in
      Ok (Str (ascii_escape s), st)
  | "abs", [ v ] -> (
      match v with
      | Int z -> Ok (Int (Z.abs z), st)
      | Bool b -> Ok (Int (if b then Z.one else Z.zero), st)
      | Float f -> Ok (Float (Float.abs f), st)
      | Complex (re, im) -> Ok (Float (Float.hypot re im), st)
      | Ref _ when is_instance_value st v -> (
          let* m, st = find_dunder st v "__abs__" in
          match m with
          | Some f -> call st f [] []
          | None -> raise_py st "TypeError" "bad operand type for abs()")
      | _ -> raise_py st "TypeError" "bad operand type for abs()")
  | "hash", [ v ] -> py_hash st v
  | "divmod", [ a; b ] -> (
      match (as_z a, as_z b) with
      | Some x, Some y ->
          if Z.equal y Z.zero then
            raise_py st "ZeroDivisionError" "integer division or modulo by zero"
          else Ok (Tuple [ Int (z_floordiv x y); Int (z_mod x y) ], st)
      | _ when is_number a && is_number b ->
          (* ref: 6.7 — divmod on floats: (a // b, a % b) *)
          let* q, st = num_binop st Floor_div a b in
          let* r, st = num_binop st Mod a b in
          Ok (Tuple [ q; r ], st)
      | _ -> (
          (* ref: 3.3.8 — divmod(a, b) uses a.__divmod__, then b.__rdivmod__ *)
          let try_d st v name other =
            let* m, st = find_dunder st v name in
            match m with
            | Some f -> (
                let* r, st = call st f [ other ] [] in
                match r with
                | Not_implemented -> Ok (None, st)
                | _ -> Ok (Some r, st))
            | None -> Ok (None, st)
          in
          let* r, st = try_d st a "__divmod__" b in
          match r with
          | Some v -> Ok (v, st)
          | None -> (
              let* r, st = try_d st b "__rdivmod__" a in
              match r with
              | Some v -> Ok (v, st)
              | None ->
                  raise_py st "TypeError"
                    (Printf.sprintf
                       "unsupported operand type(s) for divmod(): '%s' and '%s'"
                       (type_name st a) (type_name st b)))))
  | "pow", [ a; b ] -> num_binop st Pow a b
  | "pow", [ a; b; m ] -> (
      (* ref: 3.3.8 — three-argument pow(a, b, m) is modular exponentiation
         (integers only; a negative exponent uses the modular inverse) *)
      match (as_z a, as_z b, as_z m) with
      | Some _, Some _, Some z when Z.equal z Z.zero ->
          raise_py st "ValueError" "pow() 3rd argument cannot be 0"
      | Some x, Some y, Some z -> Ok (Int (Z.powm x y z), st)
      | _ ->
          raise_py st "TypeError"
            "pow() 3rd argument not allowed unless all arguments are integers")
  (* ref: 3.3.8 — hex/bin/oct render an integer (via __index__) with a prefix *)
  | "hex", [ v ] ->
      let* z, st = to_index st v in
      Ok (Str (radix_repr "0x" 16 z), st)
  | "bin", [ v ] ->
      let* z, st = to_index st v in
      Ok (Str (radix_repr "0b" 2 z), st)
  | "oct", [ v ] ->
      let* z, st = to_index st v in
      Ok (Str (radix_repr "0o" 8 z), st)
  | "round", [ v ] when is_instance_value st v -> (
      (* ref: 3.3.8 — round(x) calls x.__round__() *)
      let* m, st = find_dunder st v "__round__" in
      match m with
      | Some f -> call st f [] []
      | None -> raise_py st "TypeError" "type has no __round__ method")
  | "round", [ v; n ] when is_instance_value st v -> (
      (* ref: 3.3.8 — round(x, n) calls x.__round__(n) *)
      let* m, st = find_dunder st v "__round__" in
      match m with
      | Some f -> call st f [ n ] []
      | None -> raise_py st "TypeError" "type has no __round__ method")
  | "round", [ v ] -> (
      match v with
      | Int _ -> Ok (v, st)
      | Bool b -> Ok (Int (if b then Z.one else Z.zero), st)
      | Float x -> Ok (Int (Z.of_float (round_half_even x)), st)
      | _ -> raise_py st "TypeError" "round expects a number")
  | "round", [ v; n ] when n = None_ -> call_builtin st "round" [ v ] []
  | "round", [ v; n ] -> (
      (* ref: 3.3.8 — round(x, ndigits): ints are unchanged for ndigits >= 0,
         otherwise rounded to a power of ten; floats round to ndigits decimal
         places (round-half-to-even, like CPython). *)
      let* nd, st = as_int st n "round" in
      match v with
      | Int z ->
          if nd >= 0 then Ok (Int z, st)
          else Ok (Int (round_int_pow10 z (-nd)), st)
      | Bool b ->
          let z = if b then Z.one else Z.zero in
          if nd >= 0 then Ok (Int z, st)
          else Ok (Int (round_int_pow10 z (-nd)), st)
      | Float x ->
          if nd >= 0 then
            Ok (Float (float_of_string (Printf.sprintf "%.*f" nd x)), st)
          else
            let scale = 10. ** float_of_int (-nd) in
            Ok (Float (round_half_even (x /. scale) *. scale), st)
      | _ -> raise_py st "TypeError" "round expects a number")
  | "chr", [ v ] ->
      let* cp, st = as_int st v "chr" in
      Ok (Str (utf8_encode cp), st)
  | "ord", [ Str s ] when utf8_length s = 1 ->
      Ok (Int (Z.of_int (fst (utf8_decode_at s 0))), st)
  | "callable", [ v ] -> (
      match v with
      | Builtin _ | Bound _ -> Ok (Bool true, st)
      | Ref a -> (
          match heap_get st a with
          | Func _ | Class _ -> Ok (Bool true, st)
          | Instance { cls; _ } ->
              let* m, st = type_lookup st cls "__call__" in
              Ok (Bool (m <> None), st)
          | _ -> Ok (Bool false, st))
      | _ -> Ok (Bool false, st))
  | "getattr", [ obj; Str n ] -> getattr_value st obj n
  | "getattr", [ obj; Str n; default ] -> (
      match getattr_value st obj n with
      | Ok _ as ok -> ok
      | Error (exc, st) ->
          let* is_attr, st = exc_is st exc "AttributeError" in
          if is_attr then Ok (default, st) else Error (exc, st))
  | "setattr", [ obj; Str n; v ] ->
      let* (), st = setattr_value st obj n v in
      Ok (None_, st)
  | "delattr", [ obj; Str n ] ->
      let* (), st = delattr_value st obj n in
      Ok (None_, st)
  | "hasattr", [ obj; Str n ] -> (
      match getattr_value st obj n with
      | Ok (_, st) -> Ok (Bool true, st)
      | Error (exc, st) ->
          let* is_attr, st = exc_is st exc "AttributeError" in
          if is_attr then Ok (Bool false, st) else Error (exc, st))
  | "isinstance", [ v; cls ] -> (
      (* ref: 3.3.3 — a metaclass __instancecheck__ overrides the default
         isinstance() test *)
      let* hook, st = metaclass_hook st cls "__instancecheck__" in
      match hook with
      | Some f ->
          let* r, st = call st f [ v ] [] in
          let* b, st = py_truth st r in
          Ok (Bool b, st)
      | None ->
          let* b, st = isinstance_value st v cls in
          Ok (Bool b, st))
  | "issubclass", [ c; parent ] -> (
      (* ref: 3.3.3 — a metaclass __subclasscheck__ overrides issubclass() *)
      let* hook, st = metaclass_hook st parent "__subclasscheck__" in
      match hook with
      | Some f ->
          let* r, st = call st f [ c ] [] in
          let* b, st = py_truth st r in
          Ok (Bool b, st)
      | None ->
          let* b, st = issubclass_value st c parent in
          Ok (Bool b, st))
  | "iter", [ v ] -> py_iter st v
  | "next", [ it ] -> (
      (* a generator carries its return value on StopIteration, so route it
         through gen_send (py_next would discard it) *)
      match deref st it with
      | Some (Gen _) -> gen_send st it None_
      | _ -> (
          let* nx, st = py_next st it in
          match nx with
          | Some v -> Ok (v, st)
          | None -> raise_py st "StopIteration" ""))
  | "next", [ it; default ] -> (
      let* nx, st = py_next st it in
      match nx with Some v -> Ok (v, st) | None -> Ok (default, st))
  | "enumerate", it_args -> (
      let start = match kw "start" with Some s -> Some s | None -> None in
      match (it_args, start) with
      | [ v ], _ | [ v; _ ], None ->
          let start =
            match (it_args, start) with
            | [ _; s ], _ -> s
            | _, Some s -> s
            | _ -> Int Z.zero
          in
          let* z, st = as_int st start "enumerate" in
          let* inner, st = py_iter st v in
          let e, st = alloc st (Iter (It_enum (Z.of_int z, inner))) in
          Ok (e, st)
      | _ -> arity_error ())
  | "zip", iterables ->
      let* iters, st = map_m st py_iter iterables in
      let z, st = alloc st (Iter (It_zip iters)) in
      Ok (z, st)
  | "map", fn :: iterables when iterables <> [] ->
      let* iters, st = map_m st py_iter iterables in
      let m, st = alloc st (Iter (It_map (fn, iters))) in
      Ok (m, st)
  | "filter", [ pred; v ] ->
      let* it, st = py_iter st v in
      let m, st = alloc st (Iter (It_filter (pred, it))) in
      Ok (m, st)
  | "sum", v :: rest ->
      let* items, st = to_list st v in
      let start = match rest with [ s ] -> s | _ -> Int Z.zero in
      fold_m st (fun st acc x -> binary st Add ~inplace:false acc x) start items
  | "min", args | "max", args -> (
      let want_max = name = "max" in
      let key = Option.value (kw "key") ~default:None_ in
      match args with
      | [ v ] ->
          let* items, st = to_list st v in
          extremum st items ~key ~want_max
      | [] -> arity_error ()
      | several -> extremum st several ~key ~want_max)
  | "sorted", [ v ] ->
      let* items, st = to_list st v in
      let key = Option.value (kw "key") ~default:None_ in
      let* rev, st =
        match kw "reverse" with
        | Some r -> py_truth st r
        | None -> Ok (false, st)
      in
      let* sorted, st = sorted_values st items ~key ~reverse:rev in
      let l, st = alloc st (List sorted) in
      Ok (l, st)
  | "any", [ v ] | "all", [ v ] ->
      let want_all = name = "all" in
      let* itv, st = py_iter st v in
      let rec scan st =
        let* nx, st = py_next st itv in
        match nx with
        | None -> Ok (Bool want_all, st)
        | Some x ->
            let* t, st = py_truth st x in
            if t <> want_all then Ok (Bool (not want_all), st) else scan st
      in
      scan st
  | "vars", [ v ] -> (
      match deref st v with
      | Some (Instance { dict; _ }) -> Ok (Ref dict, st)
      | Some (Class c) -> Ok (Ref c.cdict, st)
      | _ -> raise_py st "TypeError" "vars() argument must have __dict__")
  | "reversed", [ v ] -> (
      (* ref: 3.3.7 __reversed__ — reversed() calls __reversed__ if present, else
         falls back to the sequence protocol (__len__ + __getitem__). *)
      let rev_iter xs st =
        let it, st = alloc st (Iter (It_seq (List.rev xs))) in
        Ok (it, st)
      in
      let not_reversible st =
        raise_py st "TypeError"
          (Printf.sprintf "'%s' object is not reversible" (type_name st v))
      in
      match deref st v with
      | Some (Instance _) -> (
          let* m, st = find_dunder st v "__reversed__" in
          match m with
          | Some f -> call st f [] []
          | None -> (
              let* g, st = find_dunder st v "__getitem__" in
              match g with
              | None -> not_reversible st
              | Some g ->
                  let* n, st = py_len st v in
                  let rec go st i acc =
                    if i >= n then Ok (acc, st)
                    else
                      let* x, st = call st g [ Int (Z.of_int i) ] [] in
                      go st (i + 1) (x :: acc)
                  in
                  let* items, st = go st 0 [] in
                  let it, st = alloc st (Iter (It_seq items)) in
                  Ok (it, st)))
      | Some (List xs) -> rev_iter xs st
      | Some (Dict ps) -> rev_iter (List.map fst ps) st
      | Some _ -> not_reversible st
      | None -> (
          match v with
          | Tuple xs -> rev_iter xs st
          | Str _ | Range _ ->
              let* xs, st = to_list st v in
              rev_iter xs st
          | _ -> not_reversible st))
  | "dir", [ v ] -> (
      (* ref: 3.3.2 __dir__ — dir() calls __dir__ (if defined), then converts the
         result to a list and sorts it (without removing duplicates). The default
         __dir__ yields the unique names from the instance dict and the class
         MRO. *)
      let strs_of items =
        List.filter_map (function Str s -> Some s | _ -> None) items
      in
      match deref st v with
      | Some (Instance { cls; dict; _ }) -> (
          let* m, st = type_lookup st cls "__dir__" in
          match m with
          | Some f ->
              let* res, st =
                call st (bind_class_value st f ~inst:v ~cls_addr:cls) [] []
              in
              let* items, st = to_list st res in
              let sorted = List.sort String.compare (strs_of items) in
              let l, st = alloc st (List (List.map (fun s -> Str s) sorted)) in
              Ok (l, st)
          | None ->
              let keys d =
                List.filter_map
                  (function Str s, _ -> Some s | _ -> None)
                  (dict_pairs st d)
              in
              let class_keys =
                List.concat_map
                  (fun a -> keys (cls_of st a).cdict)
                  (cls_of st cls).mro
              in
              let sorted =
                List.sort_uniq String.compare (keys dict @ class_keys)
              in
              let l, st = alloc st (List (List.map (fun s -> Str s) sorted)) in
              Ok (l, st))
      | _ ->
          let l, st = alloc st (List []) in
          Ok (l, st))
  | "format", [ v ] -> call_builtin st "format" [ v; Str "" ] []
  | "format", [ v; Str spec ] ->
      let* s, st = format_value st v spec in
      Ok (Str s, st)
  | "super", [ (Ref ca as _c); obj ] -> (
      match heap_get st ca with
      | Class _ ->
          let s, st = alloc st (Super { cls = ca; self = obj }) in
          Ok (s, st)
      | _ -> raise_py st "TypeError" "super() argument 1 must be a type")
  | "__build_class__", body :: Str cls_name :: bases -> (
      match deref st body with
      | Some (Func fn) ->
          (* ref: 3.3.3.1 — Resolving MRO entries: a base that is not a class is
             replaced by its __mro_entries__(bases) result; the unresolved bases
             are remembered as __orig_bases__. *)
          let orig_tuple = Tuple bases in
          let* (resolved, used), st =
            fold_m st
              (fun st (acc, used) b ->
                match deref st b with
                | Some (Class _) -> Ok ((acc @ [ b ], used), st)
                | _ -> (
                    let* m, st = find_dunder st b "__mro_entries__" in
                    match m with
                    | Some f ->
                        let* r, st = call st f [ orig_tuple ] [] in
                        let entries =
                          match r with Tuple xs -> xs | _ -> [ r ]
                        in
                        Ok ((acc @ entries, true), st)
                    | None -> Ok ((acc @ [ b ], used), st)))
              ([], false) bases
          in
          let bases = resolved in
          (* ref: 3.3.3.2/3.3.3.3 — the metaclass is determined before the body
             runs (raising a conflict early), then its __prepare__ (if any)
             produces the namespace the class body populates. *)
          let base_addrs = List.map addr bases in
          let explicit =
            match List.assoc_opt "metaclass" kwargs with
            | Some (Ref m)
              when match heap_get st m with Class _ -> true | _ -> false ->
                Some m
            | _ -> None
          in
          let* mcs, st = determine_metaclass st ~explicit ~bases:base_addrs in
          let body_kwds = List.filter (fun (k, _) -> k <> "metaclass") kwargs in
          let* prep, st = type_lookup st mcs "__prepare__" in
          let* ns, st =
            match prep with
            | Some f -> (
                let* r, st =
                  call st
                    (bind_class_value st f ~inst:(Ref mcs) ~cls_addr:mcs)
                    [ Str cls_name; Tuple bases ]
                    body_kwds
                in
                match deref st r with
                | Some (Dict _) -> Ok (r, st)
                | _ ->
                    raise_py st "TypeError"
                      (Printf.sprintf
                         "%s.__prepare__() must return a mapping, not %s"
                         (cls_of st mcs).cname (type_name st r)))
            | None -> Ok (alloc st (Dict []))
          in
          let* slots, st = bind_args st fn [] [] in
          let frame =
            {
              code = fn.code;
              globals = fn.globals;
              ns = addr ns;
              slots;
              stack = [];
              idx = 0;
              closure = fn.closure;
            }
          in
          let* out, st = run_frame st frame in
          ignore out;
          (* ref: 3.3.3.1 — record __orig_bases__ when __mro_entries__ rewrote
             the bases list *)
          let* (), st =
            if used then dict_set st (addr ns) (Str "__orig_bases__") orig_tuple
            else Ok ((), st)
          in
          (* class-definition keyword args (including the metaclass= hint) are
             forwarded to make_class, which consumes metaclass= and passes the
             rest to __init_subclass__ (ref: 3.3.3) *)
          make_class st ~name:cls_name ~bases:(List.map addr bases)
            ~ns_addr:(addr ns) ~kwds:kwargs
      | _ -> raise_py st "TypeError" "__build_class__ expects a function")
  (* ---- methods installed on boot classes (ref: 3.3.1 __init__/__new__,
     3.3.2 __getattribute__/__setattr__/__delattr__ — object's defaults) ---- *)
  (* ref: 3.3.3 — the default metaclass machinery. type.__new__ builds the
     class; bound-self prepended by super() leaves name/bases/ns as the trailing
     three positional arguments. *)
  | "type.__new__", (Ref mcs :: _ as args) -> (
      match List.rev args with
      | ns :: bases_v :: Str name :: _ ->
          let bases =
            match bases_v with Tuple xs -> List.map addr xs | _ -> []
          in
          type_new st ~mcs ~name ~bases ~ns_addr:(addr ns) ~kwds:kwargs
      | _ -> raise_py st "TypeError" "type.__new__() takes 3 arguments")
  (* type.__init__ is a no-op: the class was fully built by type.__new__ *)
  | "type.__init__", _ -> Ok (None_, st)
  (* type.__call__(cls, *args) instantiates the class (ref: 3.3.1) *)
  | "type.__call__", cls :: rest -> instantiate st (addr cls) rest kwargs
  | "object.__init__", _ -> Ok (None_, st)
  (* ref: 3.2 — container __init__ fills a built-in subclass instance's payload
     (reached via super().__init__(iterable) from the subclass) *)
  | "list.__init__", self :: rest -> (
      match native_of st self with
      | Some (Ref la) ->
          let* items, st =
            match rest with [] -> Ok ([], st) | it :: _ -> to_list st it
          in
          Ok (None_, heap_set st la (List items))
      | _ -> Ok (None_, st))
  | "set.__init__", self :: rest -> (
      match native_of st self with
      | Some (Ref la) -> (
          let* s, st = builtin_class_call st "set" rest [] in
          match deref st s with
          | Some (Set xs) -> Ok (None_, heap_set st la (Set xs))
          | _ -> Ok (None_, st))
      | _ -> Ok (None_, st))
  | "dict.__init__", self :: rest -> (
      match native_of st self with
      | Some (Ref da) -> (
          let* d, st = builtin_class_call st "dict" rest kwargs in
          match deref st d with
          | Some (Dict ps) -> Ok (None_, heap_set st da (Dict ps))
          | _ -> Ok (None_, st))
      | _ -> Ok (None_, st))
  | "object.__new__", cls :: _ -> (
      (* create a fresh, empty instance of the given class; extra args are
         ignored (as CPython does when __init__ is overridden) *)
      match deref st cls with
      | Some (Class _) ->
          let d, st = alloc st (Dict []) in
          Ok
            (alloc st
               (Instance { cls = addr cls; dict = addr d; native = None_ }))
      | _ -> raise_py st "TypeError" "object.__new__(X): X is not a type object"
      )
  | "object.__getattribute__", [ self; Str name ] -> (
      match deref st self with
      | Some (Instance { cls; dict; _ }) ->
          object_getattribute st self cls dict name
      | _ -> getattr_value st self name)
  | "object.__setattr__", [ self; Str name; value ] ->
      let* (), st = object_setattr st self name value in
      Ok (None_, st)
  | "object.__delattr__", [ self; Str name ] ->
      let* (), st = object_delattr st self name in
      Ok (None_, st)
  | "object.__init_subclass__", _ ->
      (* ref: 3.3.3 — the default does nothing, but rejects any arguments *)
      if kwargs = [] then Ok (None_, st)
      else
        let name =
          match args with
          | cls :: _ -> (
              match deref st cls with
              | Some (Class { cname; _ }) -> cname
              | _ -> "object")
          | [] -> "object"
        in
        raise_py st "TypeError"
          (Printf.sprintf "%s.__init_subclass__() takes no keyword arguments"
             name)
  | "BaseException.__init__", self :: rest ->
      let* (), st = set_exc_attr st self "args" (Tuple rest) in
      Ok (None_, st)
  | "BaseException.add_note", [ self; note ] -> (
      (* ref: 3.x — add_note appends a string to the exception's __notes__ list *)
      match note with
      | Str _ -> (
          let* cur, st = exc_attr st self "__notes__" in
          match deref st cur with
          | Some (List xs) ->
              Ok (None_, heap_set st (addr cur) (List (xs @ [ note ])))
          | _ ->
              let l, st = alloc st (List [ note ]) in
              let* (), st = set_exc_attr st self "__notes__" l in
              Ok (None_, st))
      | _ ->
          raise_py st "TypeError"
            (Printf.sprintf "note must be a str, not '%s'" (type_name st note)))
  | "BaseException.with_traceback", [ self; tb ] ->
      (* ref: 3.x — sets __traceback__ and returns self *)
      let* (), st = set_exc_attr st self "__traceback__" tb in
      Ok (self, st)
  | "BaseException.__str__", [ self ] -> (
      let* args, st = exc_args st self in
      match args with
      | [] -> Ok (Str "", st)
      | [ a ] ->
          let* s, st = py_str st a in
          Ok (Str s, st)
      | several ->
          let* s, st = py_repr st (Tuple several) in
          Ok (Str s, st))
  | "KeyError.__str__", [ self ] -> (
      let* args, st = exc_args st self in
      match args with
      | [ a ] ->
          let* s, st = py_repr st a in
          Ok (Str s, st)
      | _ -> call_builtin st "BaseException.__str__" [ self ] [])
  (* ref: 8.4 — (Base)ExceptionGroup(message, exceptions): store args, and the
     derived .message (a str) and .exceptions (a tuple). *)
  | "BaseExceptionGroup.__init__", self :: rest -> (
      match rest with
      | [ msg; excs_v ] -> (
          if match msg with Str _ -> false | _ -> true then
            raise_py st "TypeError"
              (Printf.sprintf
                 "BaseExceptionGroup.__new__() argument 1 must be str, not %s"
                 (type_name st msg))
          else
            let items =
              match (deref st excs_v, excs_v) with
              | Some (List xs), _ -> Some xs
              | _, Tuple xs -> Some xs
              | _ -> None
            in
            match items with
            | None ->
                raise_py st "TypeError"
                  "second argument (exceptions) must be a sequence"
            | Some [] ->
                raise_py st "ValueError"
                  "second argument (exceptions) must be a non-empty sequence"
            | Some items ->
                let* (), st =
                  fold_m st
                    (fun st () (i, it) ->
                      if is_exception_instance st it then Ok ((), st)
                      else
                        raise_py st "ValueError"
                          (Printf.sprintf
                             "Item %d of second argument (exceptions) is not \
                              an exception"
                             i))
                    ()
                    (List.mapi (fun i x -> (i, x)) items)
                in
                let* (), st = set_exc_attr st self "args" (Tuple rest) in
                let* (), st = set_exc_attr st self "message" msg in
                let* (), st = set_exc_attr st self "exceptions" (Tuple items) in
                Ok (None_, st))
      | [ _ ] ->
          raise_py st "TypeError"
            "BaseExceptionGroup.__new__() takes exactly 2 arguments (1 given)"
      | _ ->
          raise_py st "TypeError"
            "BaseExceptionGroup.__new__() takes exactly 2 arguments")
  | "BaseExceptionGroup.__str__", [ self ] ->
      let* msg, st = exc_attr st self "message" in
      let* excs, st = exc_attr st self "exceptions" in
      let n = match excs with Tuple xs -> List.length xs | _ -> 0 in
      let* ms, st = py_str st msg in
      Ok
        ( Str
            (Printf.sprintf "%s (%d sub-exception%s)" ms n
               (if n = 1 then "" else "s")),
          st )
  | "BaseExceptionGroup.split", [ self; condition ] ->
      let* m, st = eg_split_pair st self condition in
      let m, r = m in
      Ok (Tuple [ m; r ], st)
  | "BaseExceptionGroup.subgroup", [ self; condition ] ->
      let* (m, _r), st = eg_split_pair st self condition in
      Ok (m, st)
  | "BaseExceptionGroup.derive", [ self; excs ] ->
      let* items, st = to_list st excs in
      eg_derive st self items
  | "generator.send", [ g; v ] -> gen_send st g v
  | "generator.__next__", [ g ] -> gen_send st g None_
  | "generator.close", [ g ] -> (
      match deref st g with
      | Some (Gen gen) ->
          Ok (None_, heap_set st (addr g) (Gen { gen with gframe = None }))
      | _ -> raise_py st "TypeError" "close() expects a generator")
  | "generator.throw", g :: rest -> (
      (* ref: 6.2.9 — gen.throw(exc) raises exc at the current yield; a handled
         exception yields again (or returns -> StopIteration) *)
      match deref st g with
      | Some (Gen _) -> (
          let exc_arg = match rest with e :: _ -> e | [] -> None_ in
          let* exc, st = exception_instance st exc_arg in
          let* step, st = gen_throw st (addr g) exc in
          match step with
          | `Yield y -> Ok (y, st)
          | `Return rv ->
              let cls = builtin_class_addr st "StopIteration" in
              let exc, st =
                make_exc st cls (if rv = None_ then [] else [ rv ])
              in
              Error (exc, st))
      | _ -> raise_py st "TypeError" "throw() expects a generator")
  | "property.setter", [ p; f ] -> (
      match deref st p with
      | Some (Property { fget; _ }) ->
          let np, st = alloc st (Property { fget; fset = Some f }) in
          Ok (np, st)
      | _ -> raise_py st "TypeError" "setter() expects a property")
  | "property.getter", [ p; f ] -> (
      match deref st p with
      | Some (Property { fset; _ }) ->
          let np, st = alloc st (Property { fget = f; fset }) in
          Ok (np, st)
      | _ -> raise_py st "TypeError" "getter() expects a property")
  | _ -> (
      (* type-qualified methods: "str.upper", "list.append", ... *)
      match String.index_opt name '.' with
      | Some i ->
          let tag = String.sub name 0 i in
          let meth = String.sub name (i + 1) (String.length name - i - 1) in
          type_method st tag meth args kwargs
      | None -> raise_py st "RuntimeError" ("unknown builtin: " ^ name))

and exc_args st self : value list r =
  match deref st self with
  | Some (Instance { dict; _ }) -> (
      let* found, st = dget st dict (Str "args") in
      match found with Some (Tuple args) -> Ok (args, st) | _ -> Ok ([], st))
  | _ -> Ok ([], st)

(* ref: 8.4 — exception-group helpers. *)
and exc_attr st self name : value r =
  match deref st self with
  | Some (Instance { dict; _ }) ->
      let* v, st = dget st dict (Str name) in
      Ok (Option.value v ~default:None_, st)
  | _ -> Ok (None_, st)

and is_exception_instance st v =
  match deref st v with
  | Some (Instance { cls; _ }) ->
      List.mem (builtin_class_addr st "BaseException") (cls_of st cls).mro
  | _ -> false

(* ref: 5.x — the default exception repr: Type(arg, ...) over its args *)
and exc_repr st self : string r =
  let* args, st = exc_args st self in
  let* parts, st = map_m st py_repr args in
  Ok (Printf.sprintf "%s(%s)" (type_name st self) (String.concat ", " parts), st)

and is_exception_group st v =
  match deref st v with
  | Some (Instance { cls; _ }) ->
      List.mem (builtin_class_addr st "BaseExceptionGroup") (cls_of st cls).mro
  | _ -> false

(* the except*/split condition: an exception type, tuple of types, or a
   predicate callable applied to each leaf exception *)
and eg_condition_matches st condition x : bool r =
  let is_type =
    match (deref st condition, condition) with
    | Some (Class _ | Union_type _), _ -> true
    | _, Tuple _ -> true
    | _ -> false
  in
  if is_type then isinstance_value st x condition
  else
    let* r, st = call st condition [ x ] [] in
    py_truth st r

(* derive a new group of the same type, message, with the given sub-exceptions *)
and eg_derive st self excs : value r =
  let* msg, st = exc_attr st self "message" in
  let cls =
    match deref st self with
    | Some (Instance { cls; _ }) -> cls
    | _ -> builtin_class_addr st "ExceptionGroup"
  in
  let lst, st = alloc st (List excs) in
  instantiate st cls [ msg; lst ] []

(* recursively partition self.exceptions into (match, rest) groups, preserving
   nested structure; either side is None when empty *)
and eg_split_pair st self condition : (value * value) r =
  let* excs, st = exc_attr st self "exceptions" in
  let items = match excs with Tuple xs -> xs | _ -> [] in
  let rec go st matched unmatched = function
    | [] -> Ok ((List.rev matched, List.rev unmatched), st)
    | x :: rest ->
        if is_exception_group st x then
          let* (m, r), st = eg_split_pair st x condition in
          let matched = if m = None_ then matched else m :: matched in
          let unmatched = if r = None_ then unmatched else r :: unmatched in
          go st matched unmatched rest
        else
          let* ok, st = eg_condition_matches st condition x in
          if ok then go st (x :: matched) unmatched rest
          else go st matched (x :: unmatched) rest
  in
  let* (matched, unmatched), st = go st [] [] items in
  let* m, st =
    if matched = [] then Ok (None_, st) else eg_derive st self matched
  in
  let* r, st =
    if unmatched = [] then Ok (None_, st) else eg_derive st self unmatched
  in
  Ok ((m, r), st)

(* ref: 8.4 (CPython _PyEval_ExceptionGroupMatch) — match an exception against
   an except* pattern, returning (match, rest). *)
and eg_match st exc_value match_type : (value * value) r =
  if exc_value = None_ then Ok ((None_, None_), st)
  else
    let* full, st = eg_condition_matches st match_type exc_value in
    if full then
      if is_exception_group st exc_value then Ok ((exc_value, None_), st)
      else
        (* a naked exception that matches is wrapped in a one-element group *)
        let lst, st = alloc st (List [ exc_value ]) in
        let* wrapped, st =
          instantiate st
            (builtin_class_addr st "ExceptionGroup")
            [ Str ""; lst ] []
        in
        Ok ((wrapped, None_), st)
    else if is_exception_group st exc_value then
      eg_split_pair st exc_value match_type
    else Ok ((None_, exc_value), st)

and gen_send st g v : value r =
  match deref st g with
  | Some (Gen _) -> (
      let* step, st = gen_resume st (addr g) v in
      match step with
      | `Yield y -> Ok (y, st)
      | `Return rv ->
          let cls = builtin_class_addr st "StopIteration" in
          let exc, st = make_exc st cls (if rv = None_ then [] else [ rv ]) in
          Error (exc, st))
  | _ -> raise_py st "TypeError" "send() expects a generator"

and type_method st tag meth args kwargs : value r =
  match tag with
  | "str" -> str_method st meth args
  | "list" -> list_method st meth args kwargs
  | "dict" -> dict_method st meth args kwargs
  | "set" -> set_method st meth args
  | "tuple" -> tuple_method st meth args
  | "int" | "bool" -> int_method st meth args
  | "float" -> float_method st meth args
  | "complex" -> complex_method st meth args
  | "bytes" -> bytes_method st meth args
  | "bytearray" -> bytearray_method st meth args
  | _ -> raise_py st "RuntimeError" ("unknown method " ^ tag ^ "." ^ meth)

and bytes_method st meth args : value r =
  (* bytes mirror str's byte-oriented methods; results stay bytes *)
  let byte_trim ~left ~right s =
    let ws c =
      c = ' ' || c = '\t' || c = '\n' || c = '\r' || c = '\011' || c = '\012'
    in
    let n = String.length s in
    let i = ref 0 and j = ref n in
    if left then
      while !i < !j && ws s.[!i] do
        incr i
      done;
    if right then
      while !j > !i && ws s.[!j - 1] do
        decr j
      done;
    String.sub s !i (!j - !i)
  in
  match (meth, args) with
  (* ref: 3.2.5.1 — bytes.decode interprets the bytes as text (UTF-8/ASCII) *)
  | "decode", [ Bytes s ] | "decode", [ Bytes s; Str _ ] -> Ok (Str s, st)
  | "upper", [ Bytes s ] -> Ok (Bytes (String.uppercase_ascii s), st)
  | "lower", [ Bytes s ] -> Ok (Bytes (String.lowercase_ascii s), st)
  | "replace", [ Bytes s; Bytes o; Bytes n ] ->
      Ok (Bytes (replace_substring s o n max_int), st)
  | "replace", [ Bytes s; Bytes o; Bytes n; cnt ] ->
      let* c, st = as_int st cnt "replace" in
      Ok (Bytes (replace_substring s o n c), st)
  | "split", [ Bytes s ] ->
      let l, st =
        alloc st (List (List.map (fun x -> Bytes x) (split_whitespace s)))
      in
      Ok (l, st)
  | "split", [ Bytes s; Bytes sep ] ->
      let l, st =
        alloc st (List (List.map (fun x -> Bytes x) (split_on_sep s sep)))
      in
      Ok (l, st)
  | "startswith", [ Bytes s; Bytes p ] ->
      Ok
        ( Bool
            (String.length p <= String.length s
            && String.sub s 0 (String.length p) = p),
          st )
  | "endswith", [ Bytes s; Bytes p ] ->
      let ls = String.length s and lp = String.length p in
      Ok (Bool (lp <= ls && String.sub s (ls - lp) lp = p), st)
  | "find", [ Bytes s; Bytes sub ] -> (
      match find_substring s sub with
      | Some b -> Ok (Int (Z.of_int b), st)
      | None -> Ok (Int Z.minus_one, st))
  | "rfind", [ Bytes s; Bytes sub ] -> (
      match rfind_substring s sub with
      | Some b -> Ok (Int (Z.of_int b), st)
      | None -> Ok (Int Z.minus_one, st))
  | "index", [ Bytes s; Bytes sub ] -> (
      match find_substring s sub with
      | Some b -> Ok (Int (Z.of_int b), st)
      | None -> raise_py st "ValueError" "subsection not found")
  | "count", [ Bytes s; Bytes sub ] ->
      Ok (Int (Z.of_int (count_nonoverlap s sub)), st)
  | "strip", [ Bytes s ] -> Ok (Bytes (byte_trim ~left:true ~right:true s), st)
  | "lstrip", [ Bytes s ] -> Ok (Bytes (byte_trim ~left:true ~right:false s), st)
  | "rstrip", [ Bytes s ] -> Ok (Bytes (byte_trim ~left:false ~right:true s), st)
  | "hex", [ Bytes s ] ->
      let buf =
        String.concat ""
          (List.map (Printf.sprintf "%02x")
             (List.map Char.code (List.of_seq (String.to_seq s))))
      in
      Ok (Str buf, st)
  | "join", [ Bytes sep; v ] ->
      let* items, st = to_list st v in
      let parts = List.map (function Bytes b -> b | _ -> "") items in
      Ok (Bytes (String.concat sep parts), st)
  | _ -> raise_py st "RuntimeError" ("unknown bytes method " ^ meth)

(* ref: 3.2.5.2 — bytearray methods. decode mirrors bytes; append/extend mutate
   the array in place. *)
and bytearray_method st meth args : value r =
  let self_ba = function
    | Ref a -> (
        match heap_get st a with
        | Bytearray s -> Ok ((a, s), st)
        | _ -> raise_py st "TypeError" "expected a bytearray")
    | _ -> raise_py st "TypeError" "expected a bytearray"
  in
  match (meth, args) with
  | "decode", [ self ] | "decode", [ self; _ ] -> (
      match as_bytes st self with
      | Some s -> Ok (Str s, st)
      | None -> raise_py st "TypeError" "expected a bytearray")
  | "append", [ self; v ] -> (
      let* (a, s), st = self_ba self in
      match as_z v with
      | Some z when Z.geq z Z.zero && Z.lt z (Z.of_int 256) ->
          Ok
            ( None_,
              heap_set st a
                (Bytearray (s ^ String.make 1 (Char.chr (Z.to_int z)))) )
      | _ -> raise_py st "ValueError" "byte must be in range(0, 256)")
  | "extend", [ self; other ] -> (
      let* (a, s), st = self_ba self in
      match as_bytes st other with
      | Some o -> Ok (None_, heap_set st a (Bytearray (s ^ o)))
      | None ->
          raise_py st "TypeError" "can only extend bytearray with bytes-like")
  | _ -> raise_py st "RuntimeError" ("unknown bytearray method " ^ meth)

(* ref: 3.2.5 — build the raw byte string for a bytes()/bytearray() call:
   empty, N zero bytes, copy of a bytes-like, str+encoding, or an iterable of
   ints in range(0,256). *)
and build_bytes st args : string r =
  match args with
  | [] -> Ok ("", st)
  | [ Str _ ] -> raise_py st "TypeError" "string argument without an encoding"
  | [ v ] when as_bytes st v <> None -> Ok (Option.get (as_bytes st v), st)
  | [ v ] when as_z v <> None ->
      Ok (String.make (max 0 (Z.to_int (Option.get (as_z v)))) '\000', st)
  | [ Str s; Str enc ] ->
      if enc = "utf-8" || enc = "utf8" || enc = "UTF-8" || enc = "ascii" then
        Ok (s, st)
      else raise_py st "LookupError" ("unknown encoding: " ^ enc)
  | [ v ] ->
      let* items, st = to_list st v in
      fold_m st
        (fun st acc x ->
          match as_z x with
          | Some z when Z.geq z Z.zero && Z.lt z (Z.of_int 256) ->
              Ok (acc ^ String.make 1 (Char.chr (Z.to_int z)), st)
          | Some _ -> raise_py st "ValueError" "bytes must be in range(0, 256)"
          | None -> raise_py st "TypeError" "cannot convert object to bytes")
        "" items
  | _ -> raise_py st "TypeError" "wrong number of arguments"

and str_method st meth args : value r =
  let no_such () = raise_py st "RuntimeError" ("unknown str method " ^ meth) in
  match (meth, args) with
  | "upper", [ Str s ] -> Ok (Str (String.uppercase_ascii s), st)
  | "lower", [ Str s ] -> Ok (Str (String.lowercase_ascii s), st)
  | "capitalize", [ Str s ] ->
      Ok (Str (String.capitalize_ascii (String.lowercase_ascii s)), st)
  | "swapcase", [ Str s ] ->
      Ok
        ( Str
            (String.map
               (fun c ->
                 if c >= 'a' && c <= 'z' then Char.uppercase_ascii c
                 else if c >= 'A' && c <= 'Z' then Char.lowercase_ascii c
                 else c)
               s),
          st )
  | "title", [ Str s ] -> Ok (Str (title_case s), st)
  | "strip", [ Str s ] -> Ok (Str (string_trim ~left:true ~right:true s), st)
  | "lstrip", [ Str s ] -> Ok (Str (string_trim ~left:true ~right:false s), st)
  | "rstrip", [ Str s ] -> Ok (Str (string_trim ~left:false ~right:true s), st)
  | "split", [ Str s ] ->
      let l, st =
        alloc st (List (List.map (fun x -> Str x) (split_whitespace s)))
      in
      Ok (l, st)
  | "split", [ Str s; Str sep ] ->
      let l, st =
        alloc st (List (List.map (fun x -> Str x) (split_on_sep s sep)))
      in
      Ok (l, st)
  | "split", [ Str s; None_ ] ->
      let l, st =
        alloc st (List (List.map (fun x -> Str x) (split_whitespace s)))
      in
      Ok (l, st)
  | "split", [ Str s; sep; cnt ] ->
      (* ref: str.split(sep, maxsplit) — None sep splits on runs of whitespace *)
      let* m, st = as_int st cnt "split" in
      let parts =
        match sep with
        | Str sep -> split_on_sep_max s sep m
        | _ -> split_whitespace_max s m
      in
      let l, st = alloc st (List (List.map (fun x -> Str x) parts)) in
      Ok (l, st)
  | "removeprefix", [ Str s; Str p ] ->
      (* ref: str.removeprefix/removesuffix (PEP 616) *)
      let lp = String.length p and ls = String.length s in
      Ok
        ( Str
            (if lp <= ls && String.sub s 0 lp = p then String.sub s lp (ls - lp)
             else s),
          st )
  | "removesuffix", [ Str s; Str p ] ->
      let lp = String.length p and ls = String.length s in
      Ok
        ( Str
            (if lp <= ls && lp > 0 && String.sub s (ls - lp) lp = p then
               String.sub s 0 (ls - lp)
             else s),
          st )
  | "join", [ Str sep; v ] ->
      let* items, st = to_list st v in
      let* parts, st = map_m st (fun st x -> as_str st x "join") items in
      Ok (Str (String.concat sep parts), st)
  | "replace", [ Str s; Str o; Str n ] ->
      Ok (Str (replace_substring s o n max_int), st)
  | "replace", [ Str s; Str o; Str n; cnt ] ->
      let* c, st = as_int st cnt "replace" in
      Ok (Str (replace_substring s o n c), st)
  | "startswith", [ Str s; Str p ] ->
      Ok
        ( Bool
            (String.length p <= String.length s
            && String.sub s 0 (String.length p) = p),
          st )
  | "endswith", [ Str s; Str p ] ->
      let ls = String.length s and lp = String.length p in
      Ok (Bool (lp <= ls && String.sub s (ls - lp) lp = p), st)
  | "find", [ Str s; Str sub ] -> (
      match find_substring s sub with
      | Some b -> Ok (Int (Z.of_int (utf8_length (String.sub s 0 b))), st)
      | None -> Ok (Int Z.minus_one, st))
  | "index", [ Str s; Str sub ] -> (
      match find_substring s sub with
      | Some b -> Ok (Int (Z.of_int (utf8_length (String.sub s 0 b))), st)
      | None -> raise_py st "ValueError" "substring not found")
  | "count", [ Str s; Str sub ] ->
      Ok (Int (Z.of_int (count_nonoverlap s sub)), st)
  | "rfind", [ Str s; Str sub ] -> (
      match rfind_substring s sub with
      | Some b -> Ok (Int (Z.of_int (utf8_length (String.sub s 0 b))), st)
      | None -> Ok (Int Z.minus_one, st))
  | "rindex", [ Str s; Str sub ] -> (
      match rfind_substring s sub with
      | Some b -> Ok (Int (Z.of_int (utf8_length (String.sub s 0 b))), st)
      | None -> raise_py st "ValueError" "substring not found")
  | "casefold", [ Str s ] -> Ok (Str (String.lowercase_ascii s), st)
  | "isspace", [ Str s ] -> Ok (Bool (s <> "" && String.for_all is_space s), st)
  | "isalnum", [ Str s ] ->
      Ok
        ( Bool
            (s <> ""
            && String.for_all
                 (fun c ->
                   (c >= 'a' && c <= 'z')
                   || (c >= 'A' && c <= 'Z')
                   || (c >= '0' && c <= '9'))
                 s),
          st )
  | ("isnumeric" | "isdecimal"), [ Str s ] ->
      Ok (Bool (s <> "" && String.for_all (fun c -> c >= '0' && c <= '9') s), st)
  | "isidentifier", [ Str s ] ->
      (* ref: a valid identifier — [A-Za-z_][A-Za-z0-9_]* (ASCII subset) *)
      let id_start c =
        (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c = '_'
      in
      let id_cont c = id_start c || (c >= '0' && c <= '9') in
      Ok (Bool (s <> "" && id_start s.[0] && String.for_all id_cont s), st)
  | "istitle", [ Str s ] -> Ok (Bool (is_titlecased s), st)
  | "expandtabs", [ Str s ] -> Ok (Str (expand_tabs s 8), st)
  | "expandtabs", [ Str s; n ] ->
      let* w, st = as_int st n "expandtabs" in
      Ok (Str (expand_tabs s w), st)
  | "translate", [ Str s; table ] ->
      let* out, st = str_translate st s table in
      Ok (Str out, st)
  | "encode", Str s :: rest ->
      (* ref: str.encode(encoding='utf-8') -> bytes *)
      let enc = match rest with Str e :: _ -> e | _ -> "utf-8" in
      let* b, st = build_bytes st [ Str s; Str enc ] in
      Ok (Bytes b, st)
  | "isdigit", [ Str s ] ->
      Ok (Bool (s <> "" && String.for_all (fun c -> c >= '0' && c <= '9') s), st)
  | "isalpha", [ Str s ] ->
      Ok
        ( Bool
            (s <> ""
            && String.for_all
                 (fun c -> (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z'))
                 s),
          st )
  | "isupper", [ Str s ] ->
      let cased =
        String.exists
          (fun c -> (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z'))
          s
      in
      Ok
        ( Bool (cased && not (String.exists (fun c -> c >= 'a' && c <= 'z') s)),
          st )
  | "islower", [ Str s ] ->
      let cased =
        String.exists
          (fun c -> (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z'))
          s
      in
      Ok
        ( Bool (cased && not (String.exists (fun c -> c >= 'A' && c <= 'Z') s)),
          st )
  | "center", Str s :: rest -> (
      match rest with
      | [ w ] ->
          let* w, st = as_int st w "center" in
          Ok (Str (pad_str s w ' ' ~left:true ~right:true), st)
      | [ w; Str fill ] when String.length fill = 1 ->
          let* w, st = as_int st w "center" in
          Ok (Str (pad_str s w fill.[0] ~left:true ~right:true), st)
      | _ -> no_such ())
  | "zfill", [ Str s; w ] ->
      let* w, st = as_int st w "zfill" in
      Ok (Str (pad_str s w '0' ~left:true ~right:false), st)
  | "ljust", Str s :: rest -> (
      match rest with
      | [ w ] ->
          let* w, st = as_int st w "ljust" in
          Ok (Str (pad_str s w ' ' ~left:false ~right:true), st)
      | [ w; Str fill ] when String.length fill = 1 ->
          let* w, st = as_int st w "ljust" in
          Ok (Str (pad_str s w fill.[0] ~left:false ~right:true), st)
      | _ -> no_such ())
  | "rjust", Str s :: rest -> (
      match rest with
      | [ w ] ->
          let* w, st = as_int st w "rjust" in
          Ok (Str (pad_str s w ' ' ~left:true ~right:false), st)
      | [ w; Str fill ] when String.length fill = 1 ->
          let* w, st = as_int st w "rjust" in
          Ok (Str (pad_str s w fill.[0] ~left:true ~right:false), st)
      | _ -> no_such ())
  | "partition", [ Str s; Str sep ] -> (
      match find_substring s sep with
      | Some i ->
          Ok
            ( Tuple
                [
                  Str (String.sub s 0 i);
                  Str sep;
                  Str
                    (String.sub s
                       (i + String.length sep)
                       (String.length s - i - String.length sep));
                ],
              st )
      | None -> Ok (Tuple [ Str s; Str ""; Str "" ], st))
  | "rpartition", [ Str s; Str sep ] -> (
      let rec last_at i best =
        match find_substring ~from:i s sep with
        | None -> best
        | Some j -> last_at (j + 1) (Some j)
      in
      match last_at 0 None with
      | Some i ->
          Ok
            ( Tuple
                [
                  Str (String.sub s 0 i);
                  Str sep;
                  Str
                    (String.sub s
                       (i + String.length sep)
                       (String.length s - i - String.length sep));
                ],
              st )
      | None -> Ok (Tuple [ Str ""; Str ""; Str s ], st))
  | "splitlines", [ Str s ] ->
      let lines = split_on_sep s "\n" in
      let lines =
        match List.rev lines with "" :: rest -> List.rev rest | _ -> lines
      in
      let l, st = alloc st (List (List.map (fun x -> Str x) lines)) in
      Ok (l, st)
  | "format", Str s :: rest ->
      let* out, st = str_format st s rest in
      Ok (Str out, st)
  | _ -> no_such ()

and list_method st meth args kwargs : value r =
  let self_xs st = function
    | Ref a -> (
        match heap_get st a with
        | List xs -> Ok ((a, xs), st)
        | _ -> raise_py st "TypeError" "expected a list")
    | _ -> raise_py st "TypeError" "expected a list"
  in
  match (meth, args) with
  | "append", [ self; v ] ->
      let* (a, xs), st = self_xs st self in
      Ok (None_, heap_set st a (List (xs @ [ v ])))
  | "extend", [ self; v ] ->
      let* (a, xs), st = self_xs st self in
      let* items, st = to_list st v in
      Ok (None_, heap_set st a (List (xs @ items)))
  | "insert", [ self; i; v ] ->
      let* (a, xs), st = self_xs st self in
      let* i, st = as_int st i "insert" in
      let len = List.length xs in
      let i = if i < 0 then max 0 (i + len) else min i len in
      let before, after = take i xs in
      Ok (None_, heap_set st a (List (before @ (v :: after))))
  | "pop", [ self ] -> (
      let* (a, xs), st = self_xs st self in
      match List.rev xs with
      | [] -> raise_py st "IndexError" "pop from empty list"
      | last :: rest -> Ok (last, heap_set st a (List (List.rev rest))))
  | "pop", [ self; i ] ->
      let* (a, xs), st = self_xs st self in
      let* i, st = as_int st i "pop" in
      let len = List.length xs in
      let i = if i < 0 then i + len else i in
      if i < 0 || i >= len then
        raise_py st "IndexError" "pop index out of range"
      else Ok (List.nth xs i, heap_set st a (List (list_del_nth xs i)))
  | "remove", [ self; v ] ->
      let* (a, xs), st = self_xs st self in
      let rec go st i = function
        | [] -> raise_py st "ValueError" "list.remove(x): x not in list"
        | x :: rest ->
            let* eq, st = py_eq st x v in
            if eq then Ok (i, st) else go st (i + 1) rest
      in
      let* i, st = go st 0 xs in
      Ok (None_, heap_set st a (List (list_del_nth xs i)))
  | "index", [ self; v ] ->
      let* (_, xs), st = self_xs st self in
      let rec go st i = function
        | [] -> raise_py st "ValueError" "value is not in list"
        | x :: rest ->
            let* eq, st = py_eq st x v in
            if eq then Ok (Int (Z.of_int i), st) else go st (i + 1) rest
      in
      go st 0 xs
  | "count", [ self; v ] ->
      let* (_, xs), st = self_xs st self in
      let* n, st =
        fold_m st
          (fun st acc x ->
            let* eq, st = py_eq st x v in
            Ok ((if eq then acc + 1 else acc), st))
          0 xs
      in
      Ok (Int (Z.of_int n), st)
  | "reverse", [ self ] ->
      let* (a, xs), st = self_xs st self in
      Ok (None_, heap_set st a (List (List.rev xs)))
  | "clear", [ self ] ->
      let* (a, _), st = self_xs st self in
      Ok (None_, heap_set st a (List []))
  | "sort", [ self ] ->
      let* (a, xs), st = self_xs st self in
      let key = Option.value (List.assoc_opt "key" kwargs) ~default:None_ in
      let* rev, st =
        match List.assoc_opt "reverse" kwargs with
        | Some r -> py_truth st r
        | None -> Ok (false, st)
      in
      let* sorted, st = sorted_values st xs ~key ~reverse:rev in
      Ok (None_, heap_set st a (List sorted))
  | "copy", [ self ] ->
      let* (_, xs), st = self_xs st self in
      let l, st = alloc st (List xs) in
      Ok (l, st)
  | _ -> raise_py st "RuntimeError" ("unknown list method " ^ meth)

and dict_method st meth args kwargs : value r =
  let self_d st = function
    | Ref a -> (
        match heap_get st a with
        | Dict ps -> Ok ((a, ps), st)
        | _ -> raise_py st "TypeError" "expected a dict")
    | _ -> raise_py st "TypeError" "expected a dict"
  in
  match (meth, args) with
  | "get", [ self; k ] | "get", [ self; k; _ ] -> (
      let* (_, ps), st = self_d st self in
      let* found, st = dict_find st ps k in
      match found with
      | Some v -> Ok (v, st)
      | None -> Ok ((match args with [ _; _; d ] -> d | _ -> None_), st))
  | "keys", [ self ] ->
      let* (_, ps), st = self_d st self in
      let l, st = alloc st (List (List.map fst ps)) in
      Ok (l, st)
  | "values", [ self ] ->
      let* (_, ps), st = self_d st self in
      let l, st = alloc st (List (List.map snd ps)) in
      Ok (l, st)
  | "items", [ self ] ->
      let* (_, ps), st = self_d st self in
      let l, st =
        alloc st (List (List.map (fun (k, v) -> Tuple [ k; v ]) ps))
      in
      Ok (l, st)
  | "pop", [ self; k ] | "pop", [ self; k; _ ] -> (
      let* (a, ps), st = self_d st self in
      let* found, st = dict_find st ps k in
      match found with
      | Some v ->
          let* _, st = dict_del st a k in
          Ok (v, st)
      | None -> (
          match args with [ _; _; d ] -> Ok (d, st) | _ -> raise_key st k))
  | "setdefault", [ self; k ] ->
      dict_method st "setdefault" [ self; k; None_ ] kwargs
  | "setdefault", [ self; k; d ] -> (
      let* (a, ps), st = self_d st self in
      let* found, st = dict_find st ps k in
      match found with
      | Some v -> Ok (v, st)
      | None ->
          let* (), st = dict_set st a k d in
          Ok (d, st))
  | "update", self :: rest ->
      (* ref: 3.2.7.1 — update(other?, **kwargs): merge a dict or an iterable of
         (key, value) pairs, then the keyword arguments *)
      let* (a, _), st = self_d st self in
      let* (), st =
        match rest with
        | [] -> Ok ((), st)
        | other :: _ -> (
            match deref st other with
            | Some (Dict ops) ->
                fold_m st (fun st () (k, v) -> dict_set st a k v) () ops
            | _ ->
                let* items, st = to_list st other in
                fold_m st
                  (fun st () pair ->
                    match pair with
                    | Tuple [ k; v ] -> dict_set st a k v
                    | _ -> (
                        let* kv, st = to_list st pair in
                        match kv with
                        | [ k; v ] -> dict_set st a k v
                        | _ ->
                            raise_py st "ValueError"
                              "dictionary update sequence element has length \
                               != 2"))
                  () items)
      in
      let* (), st =
        fold_m st (fun st () (k, v) -> dict_set st a (Str k) v) () kwargs
      in
      Ok (None_, st)
  | "copy", [ self ] ->
      let* (_, ps), st = self_d st self in
      let d, st = alloc st (Dict ps) in
      Ok (d, st)
  | "clear", [ self ] ->
      let* (a, _), st = self_d st self in
      Ok (None_, heap_set st a (Dict []))
  | "popitem", [ self ] -> (
      (* ref: 3.2.7.1 — popitem removes and returns the last inserted (key,
         value) pair (LIFO); empty dict is a KeyError *)
      let* (a, ps), st = self_d st self in
      match List.rev ps with
      | (k, v) :: _ ->
          let* _, st = dict_del st a k in
          Ok (Tuple [ k; v ], st)
      | [] -> raise_key st (Str "popitem(): dictionary is empty"))
  | _ -> raise_py st "RuntimeError" ("unknown dict method " ^ meth)

and set_method st meth args : value r =
  let self_s st = function
    | Ref a -> (
        match heap_get st a with
        | Set xs -> Ok ((a, xs), st)
        | _ -> raise_py st "TypeError" "expected a set")
    | _ -> raise_py st "TypeError" "expected a set"
  in
  match (meth, args) with
  | "add", [ self; v ] ->
      let* (a, xs), st = self_s st self in
      let* (), st = check_hashable st v in
      let* m, st = set_mem st xs v in
      Ok (None_, if m then st else heap_set st a (Set (xs @ [ v ])))
  | "discard", [ self; v ] | "remove", [ self; v ] -> (
      let* (a, xs), st = self_s st self in
      let rec go st i = function
        | [] -> Ok (None, st)
        | x :: rest ->
            let* eq, st = py_eq st x v in
            if eq then Ok (Some i, st) else go st (i + 1) rest
      in
      let* found, st = go st 0 xs in
      match found with
      | Some i -> Ok (None_, heap_set st a (Set (list_del_nth xs i)))
      | None -> if meth = "remove" then raise_key st v else Ok (None_, st))
  | ( ("union" | "intersection" | "difference" | "symmetric_difference"),
      [ self; other ] ) ->
      (* ref: 3.2.6 — the named set algebra methods accept any iterable *)
      let* (_, xs), st = self_s st self in
      let* ys, st = to_list st other in
      let op =
        match meth with
        | "union" -> Phir.Or
        | "intersection" -> And
        | "difference" -> Sub
        | _ -> Xor
      in
      set_binop st op xs ys ~frozen:false
  | ("issubset" | "issuperset" | "isdisjoint"), [ self; other ] -> (
      let* (_, xs), st = self_s st self in
      let* ys, st = to_list st other in
      match meth with
      | "issubset" ->
          let* b, st = set_subset st xs ys in
          Ok (Bool b, st)
      | "issuperset" ->
          let* b, st = set_subset st ys xs in
          Ok (Bool b, st)
      | _ ->
          (* isdisjoint: no shared element *)
          let* shared, st =
            fold_m st
              (fun st acc y -> if acc then Ok (true, st) else set_mem st xs y)
              false ys
          in
          Ok (Bool (not shared), st))
  | "update", [ self; other ] ->
      let* (a, xs), st = self_s st self in
      let* ys, st = to_list st other in
      let* merged, st =
        fold_m st
          (fun st acc y ->
            let* m, st = set_mem st acc y in
            Ok ((if m then acc else acc @ [ y ]), st))
          xs ys
      in
      Ok (None_, heap_set st a (Set merged))
  | "pop", [ self ] -> (
      let* (a, xs), st = self_s st self in
      match xs with
      | x :: rest -> Ok (x, heap_set st a (Set rest))
      | [] -> raise_key st (Str "pop from an empty set"))
  | "clear", [ self ] ->
      let* (a, _), st = self_s st self in
      Ok (None_, heap_set st a (Set []))
  | "copy", [ self ] ->
      let* (_, xs), st = self_s st self in
      let s, st = alloc st (Set xs) in
      Ok (s, st)
  | _ -> raise_py st "RuntimeError" ("unknown set method " ^ meth)

and tuple_method st meth args : value r =
  match (meth, args) with
  | "count", [ Tuple xs; v ] ->
      let* n, st =
        fold_m st
          (fun st acc x ->
            let* eq, st = py_eq st x v in
            Ok ((if eq then acc + 1 else acc), st))
          0 xs
      in
      Ok (Int (Z.of_int n), st)
  | "index", [ Tuple xs; v ] ->
      let rec go st i = function
        | [] -> raise_py st "ValueError" "tuple.index(x): x not in tuple"
        | x :: rest ->
            let* eq, st = py_eq st x v in
            if eq then Ok (Int (Z.of_int i), st) else go st (i + 1) rest
      in
      go st 0 xs
  | _ -> raise_py st "RuntimeError" ("unknown tuple method " ^ meth)

and int_method st meth args : value r =
  match (meth, args) with
  | "bit_length", [ v ] -> (
      match as_z v with
      | Some z -> Ok (Int (Z.of_int (Z.numbits (Z.abs z))), st)
      | None -> raise_py st "TypeError" "bit_length expects an int")
  | "bit_count", [ v ] -> (
      match as_z v with
      | Some z -> Ok (Int (Z.of_int (Z.popcount (Z.abs z))), st)
      | None -> raise_py st "TypeError" "bit_count expects an int")
  | "conjugate", [ v ] -> Ok (v, st)
  | "to_bytes", v :: rest when as_z v <> None ->
      (* ref: int.to_bytes(length=1, byteorder='big', *, signed=False) *)
      let z = Option.get (as_z v) in
      let length =
        match rest with
        | l :: _ -> ( match as_z l with Some n -> Z.to_int n | None -> 1)
        | [] -> 1
      in
      let order = match rest with _ :: Str o :: _ -> o | _ -> "big" in
      if Z.sign z < 0 then
        raise_py st "OverflowError" "can't convert negative int to unsigned"
      else if Z.numbits z > length * 8 then
        raise_py st "OverflowError" "int too big to convert"
      else
        let byte i =
          Char.chr
            (Z.to_int (Z.logand (Z.shift_right z (8 * i)) (Z.of_int 0xff)))
        in
        let big = List.init length (fun i -> byte (length - 1 - i)) in
        let bytes = if order = "little" then List.rev big else big in
        Ok (Bytes (String.init length (List.nth bytes)), st)
  | "from_bytes", b :: rest -> (
      (* ref: int.from_bytes(bytes, byteorder='big') — a classmethod *)
      match as_bytes st b with
      | Some s ->
          let order = match rest with Str o :: _ -> o | _ -> "big" in
          let chars = List.of_seq (String.to_seq s) in
          let chars = if order = "little" then List.rev chars else chars in
          let z =
            List.fold_left
              (fun acc c -> Z.add (Z.shift_left acc 8) (Z.of_int (Char.code c)))
              Z.zero chars
          in
          Ok (Int z, st)
      | None -> raise_py st "TypeError" "cannot convert object to bytes")
  | "__add__", [ a; b ] -> num_binop st Add a b
  | _ -> raise_py st "RuntimeError" ("unknown int method " ^ meth)

and float_method st meth args : value r =
  match (meth, args) with
  | "is_integer", [ Float f ] -> Ok (Bool (Float.is_integer f), st)
  | "conjugate", [ v ] -> Ok (v, st)
  | "as_integer_ratio", [ Float f ] ->
      (* ref: 3.2.4.2 — exact (numerator, denominator) for a float *)
      let num, den = float_as_integer_ratio f in
      Ok (Tuple [ Int num; Int den ], st)
  | _ -> raise_py st "RuntimeError" ("unknown float method " ^ meth)

(* exact rational (numerator, denominator) of a finite float, like CPython's
   float.as_integer_ratio *)
and float_as_integer_ratio f =
  if not (Float.is_finite f) then (Z.zero, Z.one) (* unreachable in tests *)
  else
    let m, e = Float.frexp f in
    (* m * 2^e, m in [0.5,1); scale m to an integer mantissa *)
    let rec scale m e =
      if Float.is_integer m || e <= -1075 then (m, e)
      else scale (m *. 2.) (e - 1)
    in
    let m, e = scale m e in
    let num = Z.of_float m and den = Z.one in
    if e >= 0 then (Z.mul num (Z.shift_left Z.one e), den)
    else (num, Z.shift_left den (-e))

and complex_method st meth args : value r =
  match (meth, args) with
  | "conjugate", [ Complex (re, im) ] -> Ok (Complex (re, -.im), st)
  | _ -> raise_py st "RuntimeError" ("unknown complex method " ^ meth)

(* ---------- builtin type constructors ------------------------------- *)

and builtin_class_call st tag args kwargs : value r =
  match (tag, args) with
  (* ref: 3.2.13 Internal types — slice objects (slice(stop) / slice(start,stop
     [,step])) *)
  | "slice", [ stop ] -> Ok (Slice (None_, stop, None_), st)
  | "slice", [ start; stop ] -> Ok (Slice (start, stop, None_), st)
  | "slice", [ start; stop; step ] -> Ok (Slice (start, stop, step), st)
  | "int", [] -> Ok (Int Z.zero, st)
  | "int", [ v ] -> (
      match v with
      | Int _ -> Ok (v, st)
      | Bool b -> Ok (Int (if b then Z.one else Z.zero), st)
      | Float f -> Ok (Int (Z.of_float f), st)
      | Str s -> parse_int st s 10
      | _ when is_instance_value st v -> (
          (* ref: 3.3.8 — int() uses __int__, falling back to __index__ *)
          let* m, st = find_dunder st v "__int__" in
          let* m, st =
            match m with
            | Some _ -> Ok (m, st)
            | None -> find_dunder st v "__index__"
          in
          match m with
          | Some f -> call st f [] []
          | None -> (
              match native_of st v with
              | Some p -> builtin_class_call st "int" [ p ] []
              | None ->
                  raise_py st "TypeError"
                    "int() argument must be a number or string"))
      | _ -> raise_py st "TypeError" "int() argument must be a number or string"
      )
  | "int", [ Str s; base ] ->
      let* b, st = as_int st base "int" in
      parse_int st s b
  | "float", [] -> Ok (Float 0., st)
  | "float", [ v ] -> (
      match v with
      | Float _ -> Ok (v, st)
      | Int z -> Ok (Float (Z.to_float z), st)
      | Bool b -> Ok (Float (if b then 1. else 0.), st)
      | Str s -> (
          match float_of_string_opt (String.trim s) with
          | Some f -> Ok (Float f, st)
          | None ->
              raise_py st "ValueError"
                ("could not convert string to float: " ^ str_repr s))
      | _ when is_instance_value st v -> (
          (* ref: 3.3.8 — float() uses __float__, falling back to __index__ *)
          let* m, st = find_dunder st v "__float__" in
          let* m, st =
            match m with
            | Some _ -> Ok (m, st)
            | None -> find_dunder st v "__index__"
          in
          match m with
          | Some f -> call st f [] []
          | None -> (
              match native_of st v with
              | Some p -> builtin_class_call st "float" [ p ] []
              | None ->
                  raise_py st "TypeError"
                    "float() argument must be a number or string"))
      | _ ->
          raise_py st "TypeError" "float() argument must be a number or string")
  (* ref: 3.2.4.3 Complex — complex() constructor *)
  | "complex", [] -> Ok (Complex (0., 0.), st)
  | "complex", [ v ] -> (
      match as_complex v with
      | Some (re, im) -> Ok (Complex (re, im), st)
      | None ->
          raise_py st "TypeError"
            "complex() first argument must be a string or a number")
  | "complex", [ re; im ] -> (
      (* the two-argument form takes real operands: complex(a, b) = a + b*1j *)
      match (as_complex re, as_complex im) with
      | Some (rr, ri), Some (ir, ii) ->
          (* a + b*i where a=(rr,ri), b=(ir,ii): (rr - ii, ri + ir) *)
          Ok (Complex (rr -. ii, ri +. ir), st)
      | _ ->
          raise_py st "TypeError"
            "complex() argument must be a string or a number")
  | "str", [] -> Ok (Str "", st)
  | "str", [ v ] ->
      let* s, st = py_str st v in
      Ok (Str s, st)
  | "bool", [] -> Ok (Bool false, st)
  | "bool", [ v ] ->
      let* t, st = py_truth st v in
      Ok (Bool t, st)
  | "list", [] ->
      let l, st = alloc st (List []) in
      Ok (l, st)
  | "list", [ v ] ->
      let* items, st = to_list st v in
      let l, st = alloc st (List items) in
      Ok (l, st)
  | "tuple", [] -> Ok (Tuple [], st)
  | "tuple", [ v ] ->
      let* items, st = to_list st v in
      Ok (Tuple items, st)
  | "dict", [] when kwargs = [] ->
      let d, st = alloc st (Dict []) in
      Ok (d, st)
  | "dict", [] ->
      let d, st =
        alloc st (Dict (List.map (fun (k, v) -> (Str k, v)) kwargs))
      in
      Ok (d, st)
  | "dict", [ v ] -> (
      match deref st v with
      | Some (Dict ps) ->
          let d, st = alloc st (Dict ps) in
          Ok (d, st)
      | _ ->
          let* items, st = to_list st v in
          let d, st = alloc st (Dict []) in
          let* (), st =
            fold_m st
              (fun st () item ->
                let* pair, st = to_list st item in
                match pair with
                | [ k; v ] -> dict_set st (addr d) k v
                | _ ->
                    raise_py st "ValueError"
                      "dictionary update sequence element is not a pair")
              () items
          in
          Ok (d, st))
  | "set", [] ->
      let s, st = alloc st (Set []) in
      Ok (s, st)
  | "set", [ v ] ->
      let* items, st = to_list st v in
      let* elems, st = dedup_set st items in
      let s, st = alloc st (Set elems) in
      Ok (s, st)
  (* ref: 3.2.6 Set types — frozenset() constructor *)
  | "frozenset", [] -> Ok (alloc st (Frozenset []))
  | "frozenset", [ v ] -> (
      match deref st v with
      | Some (Frozenset _) -> Ok (v, st)
      | _ ->
          let* items, st = to_list st v in
          let* elems, st = dedup_set st items in
          Ok (alloc st (Frozenset elems)))
  (* ref: 3.2.5.1 — bytes(): empty / n zero bytes / from an iterable of ints /
     from a string + encoding *)
  | "bytes", args ->
      let* s, st = build_bytes st args in
      Ok (Bytes s, st)
  | "bytearray", args ->
      let* s, st = build_bytes st args in
      Ok (alloc st (Bytearray s))
  | "range", [ stop ] -> (
      match as_z stop with
      | Some z -> Ok (Range (Z.zero, z, Z.one), st)
      | None -> raise_py st "TypeError" "range() expects integers")
  | "range", [ start; stop ] -> (
      match (as_z start, as_z stop) with
      | Some a, Some b -> Ok (Range (a, b, Z.one), st)
      | _ -> raise_py st "TypeError" "range() expects integers")
  | "range", [ start; stop; step ] -> (
      match (as_z start, as_z stop, as_z step) with
      | Some _, Some _, Some s when Z.equal s Z.zero ->
          raise_py st "ValueError" "range() arg 3 must not be zero"
      | Some a, Some b, Some s -> Ok (Range (a, b, s), st)
      | _ -> raise_py st "TypeError" "range() expects integers")
  | "type", [ v ] -> class_of_value st v
  | "type", [ Str cls_name; Tuple bases; d ] -> (
      match deref st d with
      | Some (Dict ps) ->
          let ns, st = alloc st (Dict ps) in
          make_class st ~name:cls_name ~bases:(List.map addr bases)
            ~ns_addr:(addr ns) ~kwds:[]
      | _ -> raise_py st "TypeError" "type() arg 3 must be a dict")
  | "object", [] -> instantiate st (builtin_class_addr st "object") [] []
  | "property", fget :: _ ->
      let p, st = alloc st (Property { fget; fset = None }) in
      Ok (p, st)
  | "classmethod", [ f ] ->
      let c, st = alloc st (Classmethod f) in
      Ok (c, st)
  | "staticmethod", [ f ] ->
      let c, st = alloc st (Staticmethod f) in
      Ok (c, st)
  | _ ->
      raise_py st "TypeError"
        (Printf.sprintf "cannot call %s() with these arguments" tag)

and parse_int st s base : value r =
  (* ref: int(str, base) — bases 2..36 or 0 (auto-detect from prefix); a literal
     may carry the matching 0x/0o/0b prefix, an optional sign, and single
     underscores between digits *)
  let err () =
    raise_py st "ValueError"
      (Printf.sprintf "invalid literal for int() with base %d: %s" base
         (str_repr s))
  in
  if base <> 0 && (base < 2 || base > 36) then
    raise_py st "ValueError" "int() base must be >= 2 and <= 36, or 0"
  else
    let t = String.trim s in
    let neg, body =
      if t <> "" && (t.[0] = '+' || t.[0] = '-') then
        (t.[0] = '-', String.sub t 1 (String.length t - 1))
      else (false, t)
    in
    let lower = String.lowercase_ascii body in
    let has2 p = String.length lower >= 2 && lower.[0] = '0' && lower.[1] = p in
    let drop2 s = String.sub s 2 (String.length s - 2) in
    let base, digits =
      match base with
      | 0 ->
          if has2 'x' then (16, drop2 body)
          else if has2 'o' then (8, drop2 body)
          else if has2 'b' then (2, drop2 body)
          else (10, body)
      | 16 when has2 'x' -> (16, drop2 body)
      | 8 when has2 'o' -> (8, drop2 body)
      | 2 when has2 'b' -> (2, drop2 body)
      | b -> (b, body)
    in
    let digit c =
      if c >= '0' && c <= '9' then Char.code c - Char.code '0'
      else if c >= 'a' && c <= 'z' then Char.code c - Char.code 'a' + 10
      else if c >= 'A' && c <= 'Z' then Char.code c - Char.code 'A' + 10
      else 99
    in
    let zbase = Z.of_int base in
    let n = String.length digits in
    let rec go acc prev_us i =
      if i >= n then if i = 0 || prev_us then err () else Ok (Int acc, st)
      else
        let c = digits.[i] in
        if c = '_' then if i = 0 || prev_us then err () else go acc true (i + 1)
        else
          let d = digit c in
          if d >= base then err ()
          else go (Z.add (Z.mul acc zbase) (Z.of_int d)) false (i + 1)
    in
    let* v, st = go Z.zero false 0 in
    match v with
    | Int z -> Ok (Int (if neg then Z.neg z else z), st)
    | _ -> err ()

(* ------------------------------------------------------------------ *)
(* Entry point                                                         *)
(* ------------------------------------------------------------------ *)

let run_module (code : Phir.code) : (string, string) result =
  let st = boot () in
  let globals, st = alloc st (Dict [ (Str "__name__", Str "__main__") ]) in
  let frame =
    {
      code;
      globals = addr globals;
      ns = addr globals;
      slots = Int_map.empty;
      stack = [];
      idx = 0;
      closure = [];
    }
  in
  match run_frame st frame with
  | Ok (Returned _, st) -> Ok (collected_output st)
  | Ok (Yielded _, _) -> Error "module-level yield?"
  | Error (exc, st) ->
      let msg =
        match py_str st exc with Ok (s, _) -> s | Error _ -> "<unprintable>"
      in
      Error (Printf.sprintf "Uncaught %s: %s" (type_name st exc) msg)
  | exception Stack_overflow -> Error "OCaml stack overflow"
  | exception e -> Error ("interpreter bug: " ^ Printexc.to_string e)

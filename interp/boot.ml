(* Boot: builtin classes, the builtins namespace, and pure lookups.

   Split out of [Interp]; all definitions here are pure (no recursion into the
   interpreter knot) and are [open]ed by [Interp] and the per-type modules. *)

open Value

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

(* ref: 6.3.3 Slicings / slice.indices(len) — clamp a [start:stop:step] slice to
   a sequence of length [len]: [slice_bounds] gives the normalised (start, stop,
   step), [slice_indices] the concrete list of selected indices (negative
   indices count from the end; out-of-range bounds are clamped). *)
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

(* method names of a builtin type, by tag (used by attribute access) *)
let builtin_method_names = function
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

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
let exception_tree =
  [
    ("BaseException", None);
    ("Exception", Some "BaseException");
    ("ArithmeticError", Some "Exception");
    ("ZeroDivisionError", Some "ArithmeticError");
    ("LookupError", Some "Exception");
    ("KeyError", Some "LookupError");
    ("IndexError", Some "LookupError");
    ("ValueError", Some "Exception");
    ("TypeError", Some "Exception");
    ("NameError", Some "Exception");
    ("UnboundLocalError", Some "NameError");
    ("AttributeError", Some "Exception");
    ("RuntimeError", Some "Exception");
    ("NotImplementedError", Some "RuntimeError");
    ("StopIteration", Some "Exception");
    ("StopAsyncIteration", Some "Exception");
    ("AssertionError", Some "Exception");
    ("OSError", Some "Exception");
    ("ImportError", Some "Exception");
  ]

let builtin_functions =
  [
    "print";
    "len";
    "repr";
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
    "chr";
    "ord";
    "callable";
    "getattr";
    "setattr";
    "hasattr";
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
    ("str", "object", "str");
    ("list", "object", "list");
    ("dict", "object", "dict");
    ("tuple", "object", "tuple");
    ("set", "object", "set");
    ("range", "object", "range");
    ("property", "object", "property");
    ("classmethod", "object", "classmethod");
    ("staticmethod", "object", "staticmethod");
  ]

let new_class st ?builtin ~bases ~dict_pairs cname =
  let dict_ref, st = alloc st (Dict dict_pairs) in
  let mro_tail =
    match bases with
    | [] -> []
    | b :: _ -> ( match heap_get st b with Class c -> c.mro | _ -> [])
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
            [ (Str "__init__", Builtin "object.__init__") ]
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
            ]
          else if name = "KeyError" then
            [ (Str "__str__", Builtin "KeyError.__str__") ]
          else []
        in
        let caddr, st = new_class st ~bases ~dict_pairs name in
        ((name, caddr) :: acc, st))
      ([], st) exception_tree
  in
  let entries =
    List.map (fun (n, a) -> (Str n, Ref a)) (types @ excs)
    @ List.map (fun n -> (Str n, Builtin n)) builtin_functions
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
  ]

let dict_methods =
  [ "get"; "keys"; "values"; "items"; "pop"; "setdefault"; "update"; "copy" ]

let set_methods = [ "add"; "discard"; "remove"; "union"; "copy" ]
let tuple_methods = [ "count"; "index" ]
let int_methods = [ "bit_length"; "__add__" ]
let float_methods = [ "is_integer" ]
let gen_methods = [ "send"; "close"; "__next__" ]

(* ------------------------------------------------------------------ *)
(* The interpreter proper: one big recursive knot                      *)
(* ------------------------------------------------------------------ *)

let rec make_exc st cls_addr (args : value list) : value * state =
  let dict_ref, st = alloc st (Dict [ (Str "args", Tuple args) ]) in
  alloc st (Instance { cls = cls_addr; dict = addr dict_ref })

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

and dict_set st a key v : unit r =
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

(* Numeric three-way comparison; assumes both are numbers. *)
and cmp_num a b =
  match (as_z a, as_z b) with
  | Some x, Some y -> Z.compare x y
  | _ ->
      let x = Option.get (as_float a) and y = Option.get (as_float b) in
      compare x y

(* ---------- equality ----------------------------------------------- *)

and py_eq st a b : bool r =
  match (a, b) with
  | _ when is_number a && is_number b -> Ok (cmp_num a b = 0, st)
  | Str x, Str y -> Ok (x = y, st)
  | None_, None_ -> Ok (true, st)
  | Tuple xs, Tuple ys -> seq_eq st xs ys
  | Ref x, Ref y when x = y -> Ok (true, st)
  | Ref x, Ref y -> (
      match (heap_get st x, heap_get st y) with
      | List xs, List ys -> seq_eq st xs ys
      | Dict xs, Dict ys -> dict_eq st xs ys
      | Set xs, Set ys -> set_eq st xs ys
      | Instance _, _ | _, Instance _ -> instance_eq st a b
      | _ -> Ok (false, st))
  | (Ref _, _ | _, Ref _) when is_instance_value st a || is_instance_value st b
    ->
      instance_eq st a b
  | _ -> Ok (false, st)

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

and set_mem st elems x =
  fold_m st
    (fun st acc e -> if acc then Ok (true, st) else py_eq st e x)
    false elems

and is_instance_value st = function
  | Ref a -> ( match heap_get st a with Instance _ -> true | _ -> false)
  | _ -> false

and instance_eq st a b : bool r =
  (* try a.__eq__(b), then b.__eq__(a), else identity *)
  let* m, st = find_dunder st a "__eq__" in
  match m with
  | Some f ->
      let* v, st = call st f [ b ] [] in
      py_truth st v
  | None -> (
      let* m, st = find_dunder st b "__eq__" in
      match m with
      | Some f ->
          let* v, st = call st f [ a ] [] in
          py_truth st v
      | None -> Ok (a = b, st))

(* ---------- ordering ----------------------------------------------- *)

and py_lt st a b : bool r =
  match (a, b) with
  | _ when is_number a && is_number b -> Ok (cmp_num a b < 0, st)
  | Str x, Str y -> Ok (String.compare x y < 0, st)
  | Tuple xs, Tuple ys -> seq_lt st xs ys
  | Ref x, Ref y -> (
      match (heap_get st x, heap_get st y) with
      | List xs, List ys -> seq_lt st xs ys
      | Set xs, Set ys ->
          (* strict subset *)
          let* sub, st = set_subset st xs ys in
          Ok (sub && List.length xs < List.length ys, st)
      | Instance _, _ | _, Instance _ -> instance_order st a b "__lt__" "__gt__"
      | _ -> order_type_error st a b "<")
  | _ when is_instance_value st a || is_instance_value st b ->
      instance_order st a b "__lt__" "__gt__"
  | _ -> order_type_error st a b "<"

and seq_lt st xs ys =
  match (xs, ys) with
  | [], [] -> Ok (false, st)
  | [], _ -> Ok (true, st)
  | _, [] -> Ok (false, st)
  | x :: xs, y :: ys ->
      let* eq, st = py_eq st x y in
      if eq then seq_lt st xs ys else py_lt st x y

and instance_order st a b dunder rdunder : bool r =
  let* m, st = find_dunder st a dunder in
  match m with
  | Some f ->
      let* v, st = call st f [ b ] [] in
      py_truth st v
  | None -> (
      let* m, st = find_dunder st b rdunder in
      match m with
      | Some f ->
          let* v, st = call st f [ a ] [] in
          py_truth st v
      | None -> order_type_error st a b "<")

and order_type_error : 'a. state -> value -> value -> string -> 'a r =
 fun st a b sym ->
  raise_py st "TypeError"
    (Printf.sprintf "'%s' not supported between instances of '%s' and '%s'" sym
       (type_name st a) (type_name st b))

and py_compare st (op : Phir.cmpop) a b : bool r =
  match op with
  | Eq -> py_eq st a b
  | Ne ->
      let* e, st = py_eq st a b in
      Ok (not e, st)
  | Lt -> py_lt st a b
  | Gt -> py_lt st b a
  | Le -> (
      match (deref st a, deref st b) with
      | Some (Set xs), Some (Set ys) -> set_subset st xs ys
      | _ ->
          let* gt, st = py_lt st b a in
          if gt then Ok (false, st)
          else
            (* a <= b  <=>  a < b or a == b; for totally ordered builtins
               not (b < a) suffices, and instances get __le__ *)
            le_fallback st a b)
  | Ge -> (
      match (deref st a, deref st b) with
      | Some (Set xs), Some (Set ys) -> set_subset st ys xs
      | _ -> le_fallback_ge st a b)

and le_fallback st a b =
  if is_instance_value st a || is_instance_value st b then
    instance_order st a b "__le__" "__ge__"
  else Ok (true, st)
(* not (b < a) was already established *)

and le_fallback_ge st a b =
  if is_instance_value st a || is_instance_value st b then
    instance_order st a b "__ge__" "__le__"
  else
    let* lt, st = py_lt st a b in
    Ok (not lt, st)

(* ---------- truthiness and length ---------------------------------- *)

and py_truth st (v : value) : bool r =
  match v with
  | None_ -> Ok (false, st)
  | Bool b -> Ok (b, st)
  | Int z -> Ok (not (Z.equal z Z.zero), st)
  | Float f -> Ok (f <> 0., st)
  | Str s -> Ok (s <> "", st)
  | Tuple xs -> Ok (xs <> [], st)
  | Range _ ->
      let* n, st = py_len st v in
      Ok (n > 0, st)
  | Ref a -> (
      match heap_get st a with
      | List xs -> Ok (xs <> [], st)
      | Dict ps -> Ok (ps <> [], st)
      | Set xs -> Ok (xs <> [], st)
      | Instance _ -> (
          let* m, st = find_dunder st v "__bool__" in
          match m with
          | Some f -> (
              let* b, st = call st f [] [] in
              match b with
              | Bool b -> Ok (b, st)
              | _ -> raise_py st "TypeError" "__bool__ should return bool")
          | None -> (
              let* m, st = find_dunder st v "__len__" in
              match m with
              | Some f ->
                  let* n, st = call st f [] [] in
                  py_truth st n
              | None -> Ok (true, st)))
      | _ -> Ok (true, st))
  | _ -> Ok (true, st)

and py_len st (v : value) : int r =
  match v with
  | Str s -> Ok (utf8_length s, st)
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
      | Set xs -> Ok (List.length xs, st)
      | Instance _ -> (
          let* m, st = find_dunder st v "__len__" in
          match m with
          | Some f -> (
              let* n, st = call st f [] [] in
              match as_z n with
              | Some z -> Ok (Z.to_int z, st)
              | None -> raise_py st "TypeError" "__len__ should return an int")
          | None -> no_len st v)
      | _ -> no_len st v)
  | _ -> no_len st v

and no_len : 'a. state -> value -> 'a r =
 fun st v ->
  raise_py st "TypeError"
    (Printf.sprintf "object of type '%s' has no len()" (type_name st v))

(* ---------- repr and str ------------------------------------------- *)

and py_repr st (v : value) : string r =
  match v with
  | None_ -> Ok ("None", st)
  | Bool true -> Ok ("True", st)
  | Bool false -> Ok ("False", st)
  | Int z -> Ok (Z.to_string z, st)
  | Float f -> Ok (float_repr f, st)
  | Str s -> Ok (str_repr s, st)
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
      | Func fn -> Ok ("<function " ^ fn.code.qualname ^ ">", st)
      | Class { cname; _ } -> Ok ("<class '" ^ cname ^ "'>", st)
      | Instance _ -> (
          let* m, st = find_dunder st v "__repr__" in
          match m with
          | Some f -> (
              let* r, st = call st f [] [] in
              match r with
              | Str s -> Ok (s, st)
              | _ -> raise_py st "TypeError" "__repr__ returned non-string")
          | None -> Ok (Printf.sprintf "<%s object>" (type_name st v), st))
      | Gen _ -> Ok ("<generator object>", st)
      | Super _ -> Ok ("<super>", st)
      | Property _ -> Ok ("<property object>", st)
      | Classmethod _ -> Ok ("<classmethod object>", st)
      | Staticmethod _ -> Ok ("<staticmethod object>", st)
      | Cell _ -> Ok ("<cell>", st)
      | Iter _ -> Ok ("<iterator>", st))

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
              | _ -> raise_py st "TypeError" "__str__ returned non-string")
          | None -> py_repr st v)
      | _ -> py_repr st v)
  | _ -> py_repr st v

(* ---------- iteration ---------------------------------------------- *)

and py_iter st (v : value) : value r =
  match v with
  | Str s ->
      let it, st = alloc st (Iter (It_str (s, 0))) in
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
      | Set xs ->
          let it, st = alloc st (Iter (It_seq xs)) in
          Ok (it, st)
      | Iter _ | Gen _ -> Ok (v, st)
      | Instance _ -> (
          let* m, st = find_dunder st v "__iter__" in
          match m with Some f -> call st f [] [] | None -> not_iterable st v)
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

and instance_getattr st inst_v cls dict name : value r =
  if name = "__dict__" then Ok (Ref dict, st)
  else if name = "__class__" then Ok (Ref cls, st)
  else
    let* found, st = type_lookup st cls name in
    match found with
    | Some f when is_data_descriptor st f -> (
        match deref st f with
        | Some (Property { fget; _ }) -> call st fget [ inst_v ] []
        | _ -> assert false)
    | _ -> (
        let* own, st = dget st dict (Str name) in
        match own with
        | Some v -> Ok (v, st)
        | None -> (
            match found with
            | Some f -> Ok (bind_class_value st f ~inst:inst_v ~cls_addr:cls, st)
            | None -> (
                let* fallback, st = type_lookup st cls "__getattr__" in
                match fallback with
                | Some f ->
                    call st
                      (bind_class_value st f ~inst:inst_v ~cls_addr:cls)
                      [ Str name ] []
                | None -> attribute_error st inst_v name)))

and is_data_descriptor st f =
  match deref st f with Some (Property _) -> true | _ -> false

and class_getattr st cls_addr name : value r =
  let c = cls_of st cls_addr in
  match name with
  | "__name__" -> Ok (Str c.cname, st)
  | "__mro__" -> Ok (Tuple (List.map (fun a -> Ref a) c.mro), st)
  | "__dict__" -> Ok (Ref c.cdict, st)
  | "__bases__" -> Ok (Tuple (List.map (fun a -> Ref a) c.bases), st)
  | _ -> (
      let* found, st = type_lookup st cls_addr name in
      match found with
      | Some f -> (
          match deref st f with
          | Some (Classmethod m) -> Ok (Bound (m, Ref cls_addr), st)
          | Some (Staticmethod m) -> Ok (m, st)
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
  | Int _ | Bool _ -> bound_builtin "int"
  | Float _ -> bound_builtin "float"
  | Tuple _ -> bound_builtin "tuple"
  | Ref a -> (
      match heap_get st a with
      | List _ -> bound_builtin "list"
      | Dict _ -> bound_builtin "dict"
      | Set _ -> bound_builtin "set"
      | Instance { cls; dict } -> instance_getattr st v cls dict name
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
              | None -> attribute_error st v name))
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
      | _ -> attribute_error st v name)
  | _ -> attribute_error st v name

(* super: look up after [cls] in the MRO of [type(self)], bind to self. *)
and super_getattr st ~cls ~self name : value r =
  let self_cls =
    match deref st self with
    | Some (Instance { cls; _ }) -> cls
    | Some (Class _) -> addr self
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

and setattr_value st (v : value) name x : unit r =
  match deref st v with
  | Some (Instance { cls; dict }) -> (
      let* found, st = type_lookup st cls name in
      match found with
      | Some f when is_data_descriptor st f -> (
          match deref st f with
          | Some (Property { fset = Some setter; _ }) ->
              let* _, st = call st setter [ v; x ] [] in
              Ok ((), st)
          | Some (Property { fset = None; _ }) ->
              raise_py st "AttributeError"
                (Printf.sprintf "property '%s' has no setter" name)
          | _ -> assert false)
      | _ -> dict_set st dict (Str name) x)
  | Some (Class c) -> dict_set st c.cdict (Str name) x
  | Some (Func fn) -> dict_set st fn.fdict (Str name) x
  | _ ->
      raise_py st "AttributeError"
        (Printf.sprintf "'%s' object has no attribute '%s'" (type_name st v)
           name)

and delattr_value st (v : value) name : unit r =
  match deref st v with
  | Some (Instance { dict; _ }) ->
      let* removed, st = dict_del st dict (Str name) in
      if removed then Ok ((), st) else attribute_error st v name
  | Some (Class c) ->
      let* removed, st = dict_del st c.cdict (Str name) in
      if removed then Ok ((), st) else attribute_error st v name
  | _ -> attribute_error st v name

(* ---------- isinstance / issubclass -------------------------------- *)

and value_matches_builtin st tag (v : value) =
  match tag with
  | "object" -> true
  | "int" -> ( match v with Int _ | Bool _ -> true | _ -> false)
  | "bool" -> ( match v with Bool _ -> true | _ -> false)
  | "float" -> ( match v with Float _ -> true | _ -> false)
  | "str" -> ( match v with Str _ -> true | _ -> false)
  | "tuple" -> ( match v with Tuple _ -> true | _ -> false)
  | "range" -> ( match v with Range _ -> true | _ -> false)
  | "list" -> ( match deref st v with Some (List _) -> true | _ -> false)
  | "dict" -> ( match deref st v with Some (Dict _) -> true | _ -> false)
  | "set" -> ( match deref st v with Some (Set _) -> true | _ -> false)
  | "type" -> ( match deref st v with Some (Class _) -> true | _ -> false)
  | "property" -> (
      match deref st v with Some (Property _) -> true | _ -> false)
  | _ -> false

and isinstance_value st (v : value) (cls_v : value) : bool r =
  match cls_v with
  | Tuple cs ->
      fold_m st
        (fun st acc c -> if acc then Ok (true, st) else isinstance_value st v c)
        false cs
  | Ref ca -> (
      match heap_get st ca with
      | Class { builtin = Some tag; _ } when tag <> "object" ->
          Ok (value_matches_builtin st tag v, st)
      | Class { builtin = Some _; _ } -> Ok (true, st) (* object *)
      | Class _ -> (
          match deref st v with
          | Some (Instance { cls; _ }) ->
              Ok (List.mem ca (cls_of st cls).mro, st)
          | _ -> Ok (false, st))
      | _ ->
          raise_py st "TypeError" "isinstance() arg 2 must be a type or tuple")
  | _ -> raise_py st "TypeError" "isinstance() arg 2 must be a type or tuple"

and issubclass_value st (c : value) (parent : value) : bool r =
  match (c, parent) with
  | Ref a, Ref b -> (
      match (heap_get st a, heap_get st b) with
      | Class ca, Class _ -> Ok (List.mem b ca.mro, st)
      | _ -> raise_py st "TypeError" "issubclass() args must be classes")
  | _ -> raise_py st "TypeError" "issubclass() args must be classes"

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

and make_class st ~name ~bases ~ns_addr : value r =
  let bases =
    match bases with [] -> [ builtin_class_addr st "object" ] | bs -> bs
  in
  let caddr = st.next in
  let _, st =
    alloc st
      (Class
         {
           cname = name;
           bases;
           mro = caddr :: c3_linearize st bases;
           cdict = ns_addr;
           builtin = None;
         })
  in
  (* Fill the __class__ cell used by zero-argument super(), then drop the
     __classcell__ entry the class body stored for us. *)
  let* cell, st = dget st ns_addr (Str "__classcell__") in
  match cell with
  | Some (Ref cell_addr) ->
      let st = heap_set st cell_addr (Cell (Some (Ref caddr))) in
      let* _, st = dict_del st ns_addr (Str "__classcell__") in
      Ok (Ref caddr, st)
  | _ -> Ok (Ref caddr, st)

and instantiate st cls_addr args kwargs : value r =
  let d, st = alloc st (Dict []) in
  let inst, st = alloc st (Instance { cls = cls_addr; dict = addr d }) in
  let* init, st = type_lookup st cls_addr "__init__" in
  match init with
  | Some f ->
      let* _, st =
        call st (bind_class_value st f ~inst ~cls_addr) args kwargs
      in
      Ok (inst, st)
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
  | _, a, b when is_number a && is_number b -> num_binop st op a b
  | Add, Str x, Str y -> Ok (Str (x ^ y), st)
  | Mul, Str s, n when as_z n <> None ->
      Ok (Str (String.concat "" (repeat_seq [ s ] (Option.get (as_z n)))), st)
  | Mul, n, Str s when as_z n <> None ->
      Ok (Str (String.concat "" (repeat_seq [ s ] (Option.get (as_z n)))), st)
  | Add, Tuple xs, Tuple ys -> Ok (Tuple (xs @ ys), st)
  | Mul, Tuple xs, n when as_z n <> None ->
      Ok (Tuple (repeat_seq xs (Option.get (as_z n))), st)
  | _, Ref x, _ when heap_is_list st x -> list_binop st op ~inplace x b
  | Mul, n, Ref y when as_z n <> None && heap_is_list st y -> (
      match heap_get st y with
      | List ys ->
          let l, st = alloc st (List (repeat_seq ys (Option.get (as_z n)))) in
          Ok (l, st)
      | _ -> assert false)
  | _, Ref x, Ref y -> (
      match (heap_get st x, heap_get st y) with
      | Set xs, Set ys -> set_binop st op xs ys
      | _ -> binop_type_error st op a b)
  | _ -> binop_type_error st op a b

and heap_is_list st a = match heap_get st a with List _ -> true | _ -> false

and binop_type_error : 'a. state -> Phir.binop -> value -> value -> 'a r =
 fun st op a b ->
  raise_py st "TypeError"
    (Printf.sprintf "unsupported operand type(s) for %s: '%s' and '%s'"
       (binop_symbol op) (type_name st a) (type_name st b))

and num_binop st op a b : value r =
  let both_int =
    match (as_z a, as_z b) with Some x, Some y -> Some (x, y) | _ -> None
  in
  let fa = Option.get (as_float a) and fb = Option.get (as_float b) in
  match op with
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
            raise_py st "ZeroDivisionError" "integer division or modulo by zero"
          else Ok (Int (z_mod x y), st)
      | None ->
          if fb = 0. then raise_py st "ZeroDivisionError" "float modulo"
          else Ok (Float (py_float_mod fa fb), st))
  | Pow -> (
      match both_int with
      | Some (x, y) when Z.geq y Z.zero -> Ok (Int (Z.pow x (Z.to_int y)), st)
      | _ -> Ok (Float (Float.pow fa fb), st))
  | Mat_mul -> binop_type_error st op a b

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
  | _, List _, _ -> binop_type_error st op (Ref x_addr) b
  | _ -> assert false

and set_binop st op xs ys : value r =
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
  let s, st = alloc st (Set result) in
  Ok (s, st)

and instance_binop st op ~inplace a b : value r =
  let base = binop_dunder op in
  let try_dunder st v name args =
    let* m, st = find_dunder st v name in
    match m with
    | Some f ->
        let* r, st = call st f args [] in
        Ok (Some r, st)
    | None -> Ok (None, st)
  in
  let* r, st =
    if inplace then try_dunder st a ("__i" ^ base ^ "__") [ b ]
    else Ok (None, st)
  in
  match r with
  | Some v -> Ok (v, st)
  | None -> (
      let* r, st = try_dunder st a ("__" ^ base ^ "__") [ b ] in
      match r with
      | Some v -> Ok (v, st)
      | None -> (
          let* r, st = try_dunder st b ("__r" ^ base ^ "__") [ a ] in
          match r with
          | Some v -> Ok (v, st)
          | None -> binop_type_error st op a b))

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
  | Tuple xs -> set_mem st xs item
  | Ref a -> (
      match heap_get st a with
      | List xs -> set_mem st xs item
      | Set xs -> set_mem st xs item
      | Dict ps -> set_mem st (List.map fst ps) item
      | Instance _ -> (
          let* m, st = find_dunder st seq "__contains__" in
          match m with
          | Some f ->
              let* r, st = call st f [ item ] [] in
              py_truth st r
          | None ->
              (* fall back to iteration *)
              let* xs, st = to_list st seq in
              set_mem st xs item)
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
      | Ok a, Ok b, Ok c -> Ok ((a, b, c), st)
      | _ -> raise_py st "TypeError" "slice indices must be integers or None")
  | _ -> raise_py st "TypeError" "expected a slice"

and subscript st obj index : value r =
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
      | Dict ps, key -> (
          let* found, st = dict_find st ps key in
          match found with Some v -> Ok (v, st) | None -> raise_key st key)
      | Instance _, _ -> (
          let* m, st = find_dunder st obj "__getitem__" in
          match m with
          | Some f -> call st f [ index ] []
          | None ->
              raise_py st "TypeError"
                (Printf.sprintf "'%s' object is not subscriptable"
                   (type_name st obj)))
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
          (* default-step splice: xs[i:j] = iterable *)
          let* (s0, s1, s2), st = slice_args st index in
          if s2 <> None && s2 <> Some Z.one then
            unsupported st "extended slice assignment"
          else
            let idxs = slice_indices ~len:(List.length xs) s0 s1 s2 in
            let* items, st = to_list st v in
            let lo = match idxs with [] -> slice_lo xs s0 | i :: _ -> i in
            let hi = match List.rev idxs with [] -> lo | i :: _ -> i + 1 in
            let prefix = List.filteri (fun j _ -> j < lo) xs in
            let suffix = List.filteri (fun j _ -> j >= hi) xs in
            Ok ((), heap_set st a (List (prefix @ items @ suffix)))
      | List xs, _ when as_z index <> None ->
          let* i, st =
            norm_index st ~len:(List.length xs) ~what:"list assignment"
              (Option.get (as_z index))
          in
          Ok ((), heap_set st a (List (list_set_nth xs i v)))
      | Dict _, key -> dict_set st a key v
      | Instance _, _ -> (
          let* m, st = find_dunder st obj "__setitem__" in
          match m with
          | Some f ->
              let* _, st = call st f [ index; v ] [] in
              Ok ((), st)
          | None ->
              raise_py st "TypeError"
                (Printf.sprintf "'%s' object does not support item assignment"
                   (type_name st obj)))
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
      | Dict _, key ->
          let* removed, st = dict_del st a key in
          if removed then Ok ((), st) else raise_key st key
      | _ -> raise_py st "TypeError" "object does not support item deletion")
  | _ -> raise_py st "TypeError" "object does not support item deletion"

(* ---------- format specs (the subset the tests exercise) ------------ *)

and format_value st v spec : string r =
  if spec = "" then py_str st v
  else
    let fill, align, rest =
      let n = String.length spec in
      if n >= 2 && String.contains "<>^=" spec.[1] then
        (spec.[0], Some spec.[1], String.sub spec 2 (n - 2))
      else if n >= 1 && String.contains "<>^=" spec.[0] then
        (' ', Some spec.[0], String.sub spec 1 (n - 1))
      else (' ', None, spec)
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
      if rest <> "" && rest.[0] = ',' then
        (true, String.sub rest 1 (String.length rest - 1))
      else (false, rest)
    in
    let precision, rest =
      if rest <> "" && rest.[0] = '.' then
        let p, r = digits (String.sub rest 1 (String.length rest - 1)) in
        (p, r)
      else (None, rest)
    in
    let conv = rest in
    let group_int s =
      (* insert commas every three digits from the right *)
      let neg = s <> "" && s.[0] = '-' in
      let body = if neg then String.sub s 1 (String.length s - 1) else s in
      let rec go acc s =
        let n = String.length s in
        if n <= 3 then s :: acc
        else go (String.sub s (n - 3) 3 :: acc) (String.sub s 0 (n - 3))
      in
      (if neg then "-" else "") ^ String.concat "," (go [] body)
    in
    let* (body, numeric), st =
      match (v, conv) with
      | _, ("" | "d") when as_z v <> None ->
          let s = Z.to_string (Option.get (as_z v)) in
          Ok (((if grouping then group_int s else s), true), st)
      | _, "x" when as_z v <> None ->
          Ok ((Z.format "%x" (Option.get (as_z v)), true), st)
      | _, "X" when as_z v <> None ->
          Ok ((Z.format "%X" (Option.get (as_z v)), true), st)
      | _, "o" when as_z v <> None ->
          Ok ((Z.format "%o" (Option.get (as_z v)), true), st)
      | _, "b" when as_z v <> None ->
          Ok ((Z.format "%b" (Option.get (as_z v)), true), st)
      | _, "f" when is_number v ->
          let p = Option.value precision ~default:6 in
          Ok ((Printf.sprintf "%.*f" p (Option.get (as_float v)), true), st)
      | _, "e" when is_number v ->
          let p = Option.value precision ~default:6 in
          Ok ((Printf.sprintf "%.*e" p (Option.get (as_float v)), true), st)
      | _, "%" when is_number v ->
          let p = Option.value precision ~default:6 in
          Ok
            ( (Printf.sprintf "%.*f%%" p (Option.get (as_float v) *. 100.), true),
              st )
      | _, ("" | "s") ->
          let* s, st = py_str st v in
          let s =
            match precision with
            | Some p when utf8_length s > p -> utf8_sub s ~pos:0 ~len:p
            | _ -> s
          in
          Ok ((s, is_number v), st)
      | _ ->
          raise_py st "ValueError"
            (Printf.sprintf "unsupported format spec '%s'" spec)
    in
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
            | '=' ->
                if String.length body > 0 && body.[0] = '-' then
                  "-" ^ mk pad ^ String.sub body 1 (String.length body - 1)
                else mk pad ^ body
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
      (* only ever iterated (SET_UPDATE) or membership-tested: a tuple works *)
      let* vs, st = map_m st const_value (Array.to_list xs) in
      Ok (Tuple vs, st)
  | Ast.Bytes _ | Ast.Complex _ | Ast.Code _ | Ast.Ellipsis ->
      unsupported st "constant kind"

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
  let has_va = code.flags land 0x4 <> 0 and has_kw = code.flags land 0x8 <> 0 in
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
      raise_py st "TypeError"
        (err
           (Printf.sprintf "takes %d positional arguments but %d were given"
              argc (List.length args)))
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
        | _ ->
            if has_kw then Ok ((slots, (Str name, v) :: kw_extra), st)
            else
              raise_py st "TypeError"
                (err
                   (Printf.sprintf "got an unexpected keyword argument '%s'"
                      name)))
      (slots, []) kwargs
  in
  let* slots, st =
    if has_kw then
      let d, st = alloc st (Dict (List.rev kw_extra)) in
      Ok (Int_map.add kw_slot d slots, st)
    else Ok (slots, st)
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
  (* completeness *)
  let rec check st i =
    if i >= argc + kwonly then Ok (slots, st)
    else if Int_map.mem i slots then check st (i + 1)
    else
      raise_py st "TypeError"
        (err (Printf.sprintf "missing required argument: '%s'" (pname i)))
  in
  check st 0

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
  | Compare { op; coerce_bool = _; l; r } -> (
      let* (vals, f), st = eval_operands st f [ l; r ] in
      match vals with
      | [ a; b ] ->
          let* v, st = py_compare st op a b in
          Ok (Next (push f (Bool v)), st)
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
      | _ -> unsupported st "intrinsic")
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
        if f.code.flags land 0x200 <> 0 then `Async_gen
        else if f.code.flags land 0x80 <> 0 then `Coroutine
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
          if st.cur_exc = None_ then
            raise_py st "RuntimeError" "No active exception to re-raise"
          else Error (st.cur_exc, st)
      | Some _, [ v ] ->
          let* excv, st = exception_instance st v in
          Error (excv, st)
      | Some _, [ v; c ] ->
          let* excv, st = exception_instance st v in
          let* cv, st = exception_instance st c in
          let* (), st = set_exc_attr st excv "__cause__" cv in
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
  | Check_eg_match _ -> unsupported st "except*"
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
      let* (v, f), st = op1 st f v in
      let* items, st = to_list st v in
      if List.length items <> n then
        raise_py st "ValueError"
          (Printf.sprintf "not enough values to unpack (expected %d, got %d)" n
             (List.length items))
      else Ok (Next { f with stack = items @ f.stack }, st)
  | Unpack_ex { before; after; v } ->
      let* (v, f), st = op1 st f v in
      let* items, st = to_list st v in
      if List.length items < before + after then
        raise_py st "ValueError" "not enough values to unpack"
      else
        let bs, rest = take before items in
        let mid, asx = take (List.length rest - after) rest in
        let star, st = alloc st (List mid) in
        Ok (Next { f with stack = bs @ (star :: asx) @ f.stack }, st)
  | Format_simple v ->
      let* (v, f), st = op1 st f v in
      let* s, st = py_str st v in
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
                | Annotations -> Ok (fn, st)
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
                        raise_py st "TypeError"
                          "class does not define enough __match_args__")
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

and class_of_value st (v : value) : value r =
  match deref st v with
  | Some (Instance { cls; _ }) -> Ok (Ref cls, st)
  | Some (Class _) -> Ok (Ref (builtin_class_addr st "type"), st)
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
                let* s, st = py_str st (List.nth args idx) in
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
  | "abs", [ v ] -> (
      match v with
      | Int z -> Ok (Int (Z.abs z), st)
      | Bool b -> Ok (Int (if b then Z.one else Z.zero), st)
      | Float f -> Ok (Float (Float.abs f), st)
      | _ -> raise_py st "TypeError" "bad operand type for abs()")
  | "divmod", [ a; b ] -> (
      match (as_z a, as_z b) with
      | Some x, Some y ->
          if Z.equal y Z.zero then
            raise_py st "ZeroDivisionError" "integer division or modulo by zero"
          else Ok (Tuple [ Int (z_floordiv x y); Int (z_mod x y) ], st)
      | _ -> raise_py st "TypeError" "divmod expects integers")
  | "pow", [ a; b ] -> num_binop st Pow a b
  | "round", [ v ] -> (
      match v with
      | Int _ | Bool _ -> Ok (v, st)
      | Float x ->
          let fl = Float.floor x in
          let frac = x -. fl in
          let r =
            if frac > 0.5 then fl +. 1.
            else if frac < 0.5 then fl
            else if Float.rem fl 2. = 0. then fl
            else fl +. 1.
          in
          Ok (Int (Z.of_float r), st)
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
  | "isinstance", [ v; cls ] ->
      let* b, st = isinstance_value st v cls in
      Ok (Bool b, st)
  | "issubclass", [ c; parent ] ->
      let* b, st = issubclass_value st c parent in
      Ok (Bool b, st)
  | "iter", [ v ] -> py_iter st v
  | "next", [ it ] -> (
      let* nx, st = py_next st it in
      match nx with
      | Some v -> Ok (v, st)
      | None -> raise_py st "StopIteration" "")
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
          let ns, st = alloc st (Dict []) in
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
          make_class st ~name:cls_name ~bases:(List.map addr bases)
            ~ns_addr:(addr ns)
      | _ -> raise_py st "TypeError" "__build_class__ expects a function")
  (* ---- methods installed on boot classes ---- *)
  | "object.__init__", _ -> Ok (None_, st)
  | "BaseException.__init__", self :: rest ->
      let* (), st = set_exc_attr st self "args" (Tuple rest) in
      Ok (None_, st)
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
  | "generator.send", [ g; v ] -> gen_send st g v
  | "generator.__next__", [ g ] -> gen_send st g None_
  | "generator.close", [ g ] -> (
      match deref st g with
      | Some (Gen gen) ->
          Ok (None_, heap_set st (addr g) (Gen { gen with gframe = None }))
      | _ -> raise_py st "TypeError" "close() expects a generator")
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
  | "dict" -> dict_method st meth args
  | "set" -> set_method st meth args
  | "tuple" -> tuple_method st meth args
  | "int" | "bool" -> int_method st meth args
  | "float" -> float_method st meth args
  | _ -> raise_py st "RuntimeError" ("unknown method " ^ tag ^ "." ^ meth)

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

and dict_method st meth args : value r =
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
  | "setdefault", [ self; k ] -> dict_method st "setdefault" [ self; k; None_ ]
  | "setdefault", [ self; k; d ] -> (
      let* (a, ps), st = self_d st self in
      let* found, st = dict_find st ps k in
      match found with
      | Some v -> Ok (v, st)
      | None ->
          let* (), st = dict_set st a k d in
          Ok (d, st))
  | "update", [ self; other ] -> (
      let* (a, _), st = self_d st self in
      match deref st other with
      | Some (Dict ops) ->
          let* (), st =
            fold_m st (fun st () (k, v) -> dict_set st a k v) () ops
          in
          Ok (None_, st)
      | _ -> raise_py st "TypeError" "update expects a dict")
  | "copy", [ self ] ->
      let* (_, ps), st = self_d st self in
      let d, st = alloc st (Dict ps) in
      Ok (d, st)
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
  | "union", [ self; other ] -> (
      let* (_, xs), st = self_s st self in
      match deref st other with
      | Some (Set ys) -> set_binop st Or xs ys
      | _ -> raise_py st "TypeError" "union expects a set")
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
  | "__add__", [ a; b ] -> num_binop st Add a b
  | _ -> raise_py st "RuntimeError" ("unknown int method " ^ meth)

and float_method st meth args : value r =
  match (meth, args) with
  | "is_integer", [ Float f ] -> Ok (Bool (Float.is_integer f), st)
  | _ -> raise_py st "RuntimeError" ("unknown float method " ^ meth)

(* ---------- builtin type constructors ------------------------------- *)

and builtin_class_call st tag args kwargs : value r =
  match (tag, args) with
  | "int", [] -> Ok (Int Z.zero, st)
  | "int", [ v ] -> (
      match v with
      | Int _ -> Ok (v, st)
      | Bool b -> Ok (Int (if b then Z.one else Z.zero), st)
      | Float f -> Ok (Int (Z.of_float f), st)
      | Str s -> parse_int st s 10
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
      | _ ->
          raise_py st "TypeError" "float() argument must be a number or string")
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
            ~ns_addr:(addr ns)
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
  let t = String.trim s in
  let normalized = if base = 10 then t else "0" ^ base_prefix base ^ t in
  match Z.of_string normalized with
  | z -> Ok (Int z, st)
  | exception _ ->
      raise_py st "ValueError"
        (Printf.sprintf "invalid literal for int() with base %d: %s" base
           (str_repr s))

and base_prefix = function 16 -> "x" | 8 -> "o" | 2 -> "b" | _ -> ""

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

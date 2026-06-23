(* Exception construction and the small coercion helpers that only ever raise.

   All pure (no recursion into the interpreter knot): building an exception just
   allocates an [Instance]. [open]ed by [Interp] and the per-type modules so
   [raise_py]/[raise_key]/[as_str]/[as_int] resolve unqualified. *)

open Value
open Boot

let make_exc st cls_addr (args : value list) : value * state =
  let dict_ref, st = alloc st (Dict [ (Str "args", Tuple args) ]) in
  alloc st (Instance { cls = cls_addr; dict = addr dict_ref; native = None_ })

let raise_py : 'a. state -> string -> string -> 'a r =
 fun st clsname msg ->
  let cls = builtin_class_addr st clsname in
  let args = if msg = "" then [] else [ Str msg ] in
  let exc, st = make_exc st cls args in
  Error (exc, st)

let unsupported : 'a. state -> string -> 'a r =
 fun st what -> raise_py st "RuntimeError" ("pytecode unsupported: " ^ what)

(* KeyError carries the missing *key* (its [__str__] reprs it). *)
let raise_key : 'a. state -> value -> 'a r =
 fun st key ->
  let cls = builtin_class_addr st "KeyError" in
  let exc, st = make_exc st cls [ key ] in
  Error (exc, st)

let as_str st v what : string r =
  match v with
  | Str s -> Ok (s, st)
  | _ -> raise_py st "TypeError" (what ^ " expects a string")

let as_int st v what : int r =
  match as_z v with
  | Some z -> Ok (Z.to_int z, st)
  | None -> raise_py st "TypeError" (what ^ " expects an integer")

(* Effects used to tie the recursive knot across module boundaries.

   The interpreter is one big mutually-recursive group; OCaml cannot split a
   [let rec] across files. Rather than thread a record of back-edge functions
   through every per-type module, those modules call the core protocol via these
   effects; [Interp.run_module] installs the single handler that dispatches each
   one to the matching core function.

   Every effect is TAIL-RESUMPTIVE: the handler computes the result with the
   current [state] and resumes the continuation exactly once with it. No
   continuation is stored, dropped, or resumed twice, and no OCaml-level mutable
   state is introduced — so the interpreter remains an observationally pure
   function of its immutable [state] (the property symbolic execution relies on;
   the effects are merely a dispatch mechanism, equivalent to a direct call).

   Each effect carries the current [state] and returns a full ['a Value.r]
   result (so state threading and error short-circuiting work exactly as with a
   direct call). The plainly-named helpers at the bottom wrap each [perform] so
   the per-type modules (which [open Effects]) read as if they were still inside
   the knot. [Interp] does NOT open this module — it defines and uses the real
   core functions of the same names, and references the constructors qualified
   ([Effects.Call], ...) in its handler. *)

open Value

type _ Effect.t +=
  | Call :
      (state * value * value list * (string * value) list)
      -> value r Effect.t
  | Repr : (state * value) -> string r Effect.t
  | Str_of : (state * value) -> string r Effect.t
  | Eq : (state * value * value) -> bool r Effect.t
  | Truth : (state * value) -> bool r Effect.t
  | To_list : (state * value) -> value list r Effect.t
  | Find_dunder : (state * value * string) -> value option r Effect.t
  | Dget : (state * int * value) -> value option r Effect.t
  | Dict_find :
      (state * (value * value) list * value)
      -> value option r Effect.t
  | Dict_set : (state * int * value * value) -> unit r Effect.t
  | Dict_del : (state * int * value) -> bool r Effect.t
  | Check_hashable : (state * value) -> unit r Effect.t
  | Set_mem : (state * value list * value) -> bool r Effect.t
  | Set_subset : (state * value list * value list) -> bool r Effect.t
  | Set_binop :
      (state * Phir.binop * value list * value list * bool)
      -> value r Effect.t
  | Sorted_values : (state * value list * value * bool) -> value list r Effect.t
  | Num_binop : (state * Phir.binop * value * value) -> value r Effect.t

(* Back-edge helpers: each performs the matching effect above. The per-type
   modules [open Effects] so their bodies call [py_repr st v], [call st f args
   kw], [to_list st v], ... exactly as they did when part of the [let rec]. *)

let call st f args kwargs = Effect.perform (Call (st, f, args, kwargs))
let py_repr st v = Effect.perform (Repr (st, v))
let py_str st v = Effect.perform (Str_of (st, v))
let py_eq st a b = Effect.perform (Eq (st, a, b))
let py_truth st v = Effect.perform (Truth (st, v))
let to_list st v = Effect.perform (To_list (st, v))
let find_dunder st v name = Effect.perform (Find_dunder (st, v, name))
let dget st a key = Effect.perform (Dget (st, a, key))
let dict_find st pairs key = Effect.perform (Dict_find (st, pairs, key))
let dict_set st a key v = Effect.perform (Dict_set (st, a, key, v))
let dict_del st a key = Effect.perform (Dict_del (st, a, key))
let check_hashable st v = Effect.perform (Check_hashable (st, v))
let set_mem st xs x = Effect.perform (Set_mem (st, xs, x))
let set_subset st xs ys = Effect.perform (Set_subset (st, xs, ys))

let set_binop st op xs ys ~frozen =
  Effect.perform (Set_binop (st, op, xs, ys, frozen))

let sorted_values st items ~key ~reverse =
  Effect.perform (Sorted_values (st, items, key, reverse))

let num_binop st op a b = Effect.perform (Num_binop (st, op, a, b))

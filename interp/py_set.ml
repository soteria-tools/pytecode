(* set methods.

   ref: 3.2.6 Set types (the type hierarchy); the methods are the Library
   Reference "Set Types — set, frozenset". Note: the named algebra methods
   (union/intersection/difference/...) accept any iterable, whereas the operator
   forms (|, &, -, ^) require a set; remove() is KeyError on a missing element
   while discard() is not; pop() removes an arbitrary element.

   Back-edges go through [Effects]. *)

open Value
open Boot
open Errors
open Effects

let set_method st meth args : value r =
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

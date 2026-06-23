(* list methods. Back-edges go through [Effects]. *)

open Value
open Boot
open Errors
open Effects

let list_method st meth args kwargs : value r =
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

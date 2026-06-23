(* tuple methods. Back-edges go through [Effects]. *)

open Value
open Errors
open Effects

let tuple_method st meth args : value r =
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

(* dict methods.

   ref: 3.2.7 Mappings / 3.2.7.1 Dictionaries (the type hierarchy); the methods
   are the Library Reference "Mapping Types — dict". Note: get()/pop() take an
   optional default (pop() without one is KeyError); setdefault() inserts the
   default if absent; popitem() removes the last-inserted pair (LIFO).

   Back-edges go through [Effects]. *)

open Value
open Errors
open Effects

let rec dict_method st meth args kwargs : value r =
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

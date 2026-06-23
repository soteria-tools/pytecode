(* bytes / bytearray methods and the bytes()/bytearray() payload builder.
   Back-edges go through [Effects]; pure byte work is in [Strutil]. *)

open Value
open Errors
open Strutil
open Effects

let bytes_method st meth args : value r =
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

let bytearray_method st meth args : value r =
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

let build_bytes st args : string r =
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

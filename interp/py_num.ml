(* int / float / complex methods and int() string parsing.

   ref: 3.2.4 Numbers (the numeric tower: 3.2.4.1 Integral, 3.2.4.2 Real,
   3.2.4.3 Complex); the methods are the Library Reference "Numeric Types — int,
   float, complex" (int.to_bytes/bit_length/..., float.is_integer/
   as_integer_ratio, complex.conjugate).

   Back-edges go through [Effects]; pure rounding etc. is in [Numutil]. *)

open Value
open Errors
open Numutil
open Effects

let int_method st meth args : value r =
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

let float_method st meth args : value r =
  match (meth, args) with
  | "is_integer", [ Float f ] -> Ok (Bool (Float.is_integer f), st)
  | "conjugate", [ v ] -> Ok (v, st)
  | "as_integer_ratio", [ Float f ] ->
      (* ref: 3.2.4.2 — exact (numerator, denominator) for a float *)
      let num, den = float_as_integer_ratio f in
      Ok (Tuple [ Int num; Int den ], st)
  | _ -> raise_py st "RuntimeError" ("unknown float method " ^ meth)

let complex_method st meth args : value r =
  match (meth, args) with
  | "conjugate", [ Complex (re, im) ] -> Ok (Complex (re, -.im), st)
  | _ -> raise_py st "RuntimeError" ("unknown complex method " ^ meth)

let parse_int st s base : value r =
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

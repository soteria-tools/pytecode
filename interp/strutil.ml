(* Pure string helpers (no recursion into the interpreter knot).

   These are the byte/character-level mechanics behind specific str/bytes
   methods (each function's comment names the method it serves, e.g. str.title,
   str.istitle, str.expandtabs, str.translate); the Python-observable behaviour
   and its reference pointers live at the method level — see [Py_str] / [Py_bytes]
   and the Library Reference "Text/Binary Sequence Types".

   [open]ed by [Interp], [Py_str] and [Py_bytes]. *)

open Value

let is_space c =
  c = ' ' || c = '\t' || c = '\n' || c = '\r' || c = '\x0b' || c = '\x0c'

let find_substring ?(from = 0) hay needle : int option =
  let lh = String.length hay and ln = String.length needle in
  let rec go i =
    if i + ln > lh then None
    else if String.sub hay i ln = needle then Some i
    else go (i + 1)
  in
  go from

let split_on_sep s sep =
  let ls = String.length sep in
  let rec go i acc =
    match find_substring ~from:i s sep with
    | None -> List.rev (String.sub s i (String.length s - i) :: acc)
    | Some j -> go (j + ls) (String.sub s i (j - i) :: acc)
  in
  go 0 []

let split_whitespace s =
  let n = String.length s in
  let rec go i acc cur =
    if i >= n then List.rev (if cur = "" then acc else cur :: acc)
    else if is_space s.[i] then
      go (i + 1) (if cur = "" then acc else cur :: acc) ""
    else go (i + 1) acc (cur ^ String.make 1 s.[i])
  in
  go 0 [] ""

let split_on_sep_max s sep maxsplit =
  if maxsplit < 0 then split_on_sep s sep
  else
    let ls = String.length sep in
    let rec go i n acc =
      if n >= maxsplit then
        List.rev (String.sub s i (String.length s - i) :: acc)
      else
        match find_substring ~from:i s sep with
        | None -> List.rev (String.sub s i (String.length s - i) :: acc)
        | Some j -> go (j + ls) (n + 1) (String.sub s i (j - i) :: acc)
    in
    go 0 0 []

let split_whitespace_max s maxsplit =
  if maxsplit < 0 then split_whitespace s
  else
    let n = String.length s in
    let rec skip i = if i < n && is_space s.[i] then skip (i + 1) else i in
    let rec word i =
      if i < n && not (is_space s.[i]) then word (i + 1) else i
    in
    let rec go i cnt acc =
      let i = skip i in
      if i >= n then List.rev acc
      else if cnt >= maxsplit then List.rev (String.sub s i (n - i) :: acc)
      else
        let e = word i in
        go e (cnt + 1) (String.sub s i (e - i) :: acc)
    in
    go 0 0 []

let rfind_substring s sub =
  if sub = "" then Some (String.length s)
  else
    let rec go i last =
      match find_substring ~from:i s sub with
      | Some j -> go (j + 1) (Some j)
      | None -> last
    in
    go 0 None

let is_titlecased s =
  let upper c = c >= 'A' && c <= 'Z' in
  let lower c = c >= 'a' && c <= 'z' in
  let rec go i prev_cased any_cased ok =
    if i >= String.length s then ok && any_cased
    else
      let c = s.[i] in
      if upper c then
        if prev_cased then go (i + 1) true any_cased false
        else go (i + 1) true true ok
      else if lower c then
        if prev_cased then go (i + 1) true true ok
        else go (i + 1) true true false
      else go (i + 1) false any_cased ok
  in
  go 0 false false true

let expand_tabs s w =
  let rec go i col acc =
    if i >= String.length s then String.concat "" (List.rev acc)
    else
      match s.[i] with
      | '\t' ->
          let pad = if w <= 0 then 0 else w - (col mod w) in
          go (i + 1) (col + pad) (String.make pad ' ' :: acc)
      | ('\n' | '\r') as c -> go (i + 1) 0 (String.make 1 c :: acc)
      | c -> go (i + 1) (col + 1) (String.make 1 c :: acc)
  in
  go 0 0 []

let string_trim ~left ~right s =
  let n = String.length s in
  let rec lo i = if i < n && left && is_space s.[i] then lo (i + 1) else i in
  let rec hi i =
    if i > 0 && right && is_space s.[i - 1] then hi (i - 1) else i
  in
  let a = lo 0 in
  let b = hi n in
  if b <= a then "" else String.sub s a (b - a)

let count_nonoverlap s sub =
  if sub = "" then utf8_length s + 1
  else
    let rec go i acc =
      match find_substring ~from:i s sub with
      | None -> acc
      | Some j -> go (j + String.length sub) (acc + 1)
    in
    go 0 0

let replace_substring s old_s new_s limit =
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

let title_case s =
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

let str_translate st s table =
  let pairs = match deref st table with Some (Dict ps) -> ps | _ -> [] in
  let lookup cp =
    List.find_map
      (fun (k, v) ->
        match k with Int z when Z.to_int z = cp -> Some v | _ -> None)
      pairs
  in
  let rec go i acc st =
    if i >= String.length s then Ok (String.concat "" (List.rev acc), st)
    else
      let cp, n = utf8_decode_at s i in
      let piece, st =
        match lookup cp with
        | Some (Str r) -> (r, st)
        | Some (Int z) -> (utf8_encode (Z.to_int z), st)
        | Some None_ -> ("", st)
        | _ -> (String.sub s i n, st)
      in
      go (i + n) (piece :: acc) st
  in
  go 0 [] st

let ascii_escape s =
  let rec go i acc =
    if i >= String.length s then List.rev acc
    else
      let cp, n = utf8_decode_at s i in
      let piece =
        if cp < 0x80 then String.sub s i n
        else if cp <= 0xff then Printf.sprintf "\\x%02x" cp
        else if cp <= 0xffff then Printf.sprintf "\\u%04x" cp
        else Printf.sprintf "\\U%08x" cp
      in
      go (i + n) (piece :: acc)
  in
  String.concat "" (go 0 [])

let pad_str s width fill ~left ~right =
  let len = utf8_length s in
  if len >= width then s
  else
    let pad = width - len in
    if left && right then
      let l = pad / 2 in
      String.make l fill ^ s ^ String.make (pad - l) fill
    else if left then String.make pad fill ^ s
    else s ^ String.make pad fill

let radix_repr prefix base z =
  let sign = if Z.sign z < 0 then "-" else "" in
  let digits =
    if Z.equal z Z.zero then "0"
    else
      let rec go acc z =
        if Z.equal z Z.zero then acc
        else
          let d = Z.to_int (Z.rem z (Z.of_int base)) in
          let c =
            if d < 10 then Char.chr (d + Char.code '0')
            else Char.chr (d - 10 + Char.code 'a')
          in
          go (String.make 1 c ^ acc) (Z.div z (Z.of_int base))
      in
      go "" (Z.abs z)
  in
  sign ^ prefix ^ digits

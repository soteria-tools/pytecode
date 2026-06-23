(* str methods, format() spec rendering, printf-style %, and str.format.

   Back-edges into the core protocol go through [Effects]; pure string
   work is in [Strutil]. *)

open Value
open Errors
open Strutil
open Effects

let build_bytes = Py_bytes.build_bytes

let rec str_method st meth args : value r =
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
  | "split", [ Str s; None_ ] ->
      let l, st =
        alloc st (List (List.map (fun x -> Str x) (split_whitespace s)))
      in
      Ok (l, st)
  | "split", [ Str s; sep; cnt ] ->
      (* ref: str.split(sep, maxsplit) — None sep splits on runs of whitespace *)
      let* m, st = as_int st cnt "split" in
      let parts =
        match sep with
        | Str sep -> split_on_sep_max s sep m
        | _ -> split_whitespace_max s m
      in
      let l, st = alloc st (List (List.map (fun x -> Str x) parts)) in
      Ok (l, st)
  | "removeprefix", [ Str s; Str p ] ->
      (* ref: str.removeprefix/removesuffix (PEP 616) *)
      let lp = String.length p and ls = String.length s in
      Ok
        ( Str
            (if lp <= ls && String.sub s 0 lp = p then String.sub s lp (ls - lp)
             else s),
          st )
  | "removesuffix", [ Str s; Str p ] ->
      let lp = String.length p and ls = String.length s in
      Ok
        ( Str
            (if lp <= ls && lp > 0 && String.sub s (ls - lp) lp = p then
               String.sub s 0 (ls - lp)
             else s),
          st )
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
  | "rfind", [ Str s; Str sub ] -> (
      match rfind_substring s sub with
      | Some b -> Ok (Int (Z.of_int (utf8_length (String.sub s 0 b))), st)
      | None -> Ok (Int Z.minus_one, st))
  | "rindex", [ Str s; Str sub ] -> (
      match rfind_substring s sub with
      | Some b -> Ok (Int (Z.of_int (utf8_length (String.sub s 0 b))), st)
      | None -> raise_py st "ValueError" "substring not found")
  | "casefold", [ Str s ] -> Ok (Str (String.lowercase_ascii s), st)
  | "isspace", [ Str s ] -> Ok (Bool (s <> "" && String.for_all is_space s), st)
  | "isalnum", [ Str s ] ->
      Ok
        ( Bool
            (s <> ""
            && String.for_all
                 (fun c ->
                   (c >= 'a' && c <= 'z')
                   || (c >= 'A' && c <= 'Z')
                   || (c >= '0' && c <= '9'))
                 s),
          st )
  | ("isnumeric" | "isdecimal"), [ Str s ] ->
      Ok (Bool (s <> "" && String.for_all (fun c -> c >= '0' && c <= '9') s), st)
  | "isidentifier", [ Str s ] ->
      (* ref: a valid identifier — [A-Za-z_][A-Za-z0-9_]* (ASCII subset) *)
      let id_start c =
        (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c = '_'
      in
      let id_cont c = id_start c || (c >= '0' && c <= '9') in
      Ok (Bool (s <> "" && id_start s.[0] && String.for_all id_cont s), st)
  | "istitle", [ Str s ] -> Ok (Bool (is_titlecased s), st)
  | "expandtabs", [ Str s ] -> Ok (Str (expand_tabs s 8), st)
  | "expandtabs", [ Str s; n ] ->
      let* w, st = as_int st n "expandtabs" in
      Ok (Str (expand_tabs s w), st)
  | "translate", [ Str s; table ] ->
      let* out, st = str_translate st s table in
      Ok (Str out, st)
  | "encode", Str s :: rest ->
      (* ref: str.encode(encoding='utf-8') -> bytes *)
      let enc = match rest with Str e :: _ -> e | _ -> "utf-8" in
      let* b, st = build_bytes st [ Str s; Str enc ] in
      Ok (Bytes b, st)
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
                (* "{}" is format(arg, "") — delegate to __format__ for instances *)
                let* s, st = format_value st (List.nth args idx) "" in
                go st (j + 1) auto (acc ^ s))
      | c -> go st (i + 1) auto (acc ^ String.make 1 c)
  in
  go st 0 0 ""

and printf_format st fmt arg : value r =
  (* a tuple supplies positional args (and is checked for leftovers); any other
     value is a single positional arg that %(key) specifiers also index into *)
  let is_tuple = match arg with Tuple _ -> true | _ -> false in
  let pos_args = match arg with Tuple xs -> xs | single -> [ single ] in
  let n = String.length fmt in
  let span pred i =
    let rec go j = if j < n && pred fmt.[j] then go (j + 1) else j in
    go i
  in
  (* translate printf (flags,width,prec,conv) to a format-spec string *)
  let to_spec flags width prec conv =
    let has c = String.contains flags c in
    let numeric = String.contains "diuoxXeEfFgG" conv in
    let conv =
      match conv with 'i' | 'u' -> "d" | 'F' -> "f" | c -> String.make 1 c
    in
    let align = if has '-' then "<" else if numeric then "" else ">" in
    let sign = if has '+' then "+" else if has ' ' then " " else "" in
    let alt = if has '#' then "#" else "" in
    let zero = if has '0' && (not (has '-')) && numeric then "0" else "" in
    String.concat ""
      [
        align;
        sign;
        alt;
        zero;
        width;
        (if prec = "" then "" else "." ^ prec);
        conv;
      ]
  in
  let rec scan st i args acc =
    if i >= n then
      if is_tuple && args <> [] then
        raise_py st "TypeError"
          "not all arguments converted during string formatting"
      else Ok (String.concat "" (List.rev acc), st)
    else if fmt.[i] <> '%' then
      scan st (i + 1) args (String.make 1 fmt.[i] :: acc)
    else begin
      (* %[(key)][flags][width][.prec][len]conv *)
      let key, j =
        if i + 1 < n && fmt.[i + 1] = '(' then
          let close = span (fun c -> c <> ')') (i + 2) in
          (Some (String.sub fmt (i + 2) (close - i - 2)), close + 1)
        else (None, i + 1)
      in
      let fl_end = span (fun c -> String.contains "-+ #0" c) j in
      let flags = String.sub fmt j (fl_end - j) in
      let w_end = span (fun c -> c >= '0' && c <= '9') fl_end in
      let width = String.sub fmt fl_end (w_end - fl_end) in
      let prec, p_end =
        if w_end < n && fmt.[w_end] = '.' then
          let pe = span (fun c -> c >= '0' && c <= '9') (w_end + 1) in
          (String.sub fmt (w_end + 1) (pe - w_end - 1), pe)
        else ("", w_end)
      in
      let p_end = span (fun c -> String.contains "hlL" c) p_end in
      if p_end >= n then raise_py st "ValueError" "incomplete format"
      else
        let conv = fmt.[p_end] in
        let next = p_end + 1 in
        if conv = '%' then scan st next args ("%" :: acc)
        else
          let* (value, args'), st =
            match key with
            | Some k -> (
                (* %(key) indexes the mapping without consuming positional args *)
                match deref st arg with
                | Some (Dict _) -> (
                    let* v, st = dget st (addr arg) (Str k) in
                    match v with
                    | Some v -> Ok ((v, args), st)
                    | None -> raise_key st (Str k))
                | _ -> raise_py st "TypeError" "format requires a mapping")
            | None -> (
                match args with
                | v :: rest -> Ok ((v, rest), st)
                | [] ->
                    raise_py st "TypeError"
                      "not enough arguments for format string")
          in
          (* conversions needing pre-processing into a string value *)
          let* value, conv, st =
            match conv with
            | 'r' ->
                let* s, st = py_repr st value in
                Ok (Str s, 's', st)
            | 'a' ->
                let* s, st = py_repr st value in
                Ok (Str (ascii_escape s), 's', st)
            | 's' ->
                let* s, st = py_str st value in
                Ok (Str s, 's', st)
            | 'c' -> (
                match value with
                | Str s when utf8_length s = 1 -> Ok (Str s, 's', st)
                | _ ->
                    let* cp, st = as_int st value "%c" in
                    Ok (Str (utf8_encode cp), 's', st))
            | ('d' | 'i' | 'u')
              when match value with Float _ -> true | _ -> false ->
                (* %d truncates a float toward zero *)
                let x = Option.get (as_float value) in
                Ok (Int (Z.of_float x), conv, st)
            | _ -> Ok (value, conv, st)
          in
          let* piece, st =
            format_value st value (to_spec flags width prec conv)
          in
          scan st next args' (piece :: acc)
    end
  in
  let* s, st = scan st 0 pos_args [] in
  Ok (Str s, st)

and format_value st v spec : string r =
  if is_instance_value st v then
    (* ref: 3.3.1 __format__ — format()/f-strings/str.format delegate here;
       object's default delegates to __str__ for an empty spec and rejects a
       non-empty one *)
    let* m, st = find_dunder st v "__format__" in
    match m with
    | Some f -> (
        let* r, st = call st f [ Str spec ] [] in
        match r with
        | Str s -> Ok (s, st)
        | _ ->
            raise_py st "TypeError"
              (Printf.sprintf "__format__ must return a str, not %s"
                 (type_name st r)))
    | None -> (
        (* ref: 3.2 — a built-in subclass formats via its payload *)
        match native_of st v with
        | Some p -> format_value st p spec
        | None ->
            if spec = "" then py_str st v
            else
              raise_py st "TypeError"
                (Printf.sprintf
                   "unsupported format string passed to %s.__format__"
                   (type_name st v)))
  else if spec = "" then py_str st v
  else
    let fill, align, rest =
      let n = String.length spec in
      if n >= 2 && String.contains "<>^=" spec.[1] then
        (spec.[0], Some spec.[1], String.sub spec 2 (n - 2))
      else if n >= 1 && String.contains "<>^=" spec.[0] then
        (' ', Some spec.[0], String.sub spec 1 (n - 1))
      else (' ', None, spec)
    in
    (* ref: format spec — [[fill]align][sign][#][0][width][grouping][.prec][type] *)
    let sign, rest =
      if rest <> "" && (rest.[0] = '+' || rest.[0] = '-' || rest.[0] = ' ') then
        (rest.[0], String.sub rest 1 (String.length rest - 1))
      else ('-', rest)
    in
    let alt, rest =
      if rest <> "" && rest.[0] = '#' then
        (true, String.sub rest 1 (String.length rest - 1))
      else (false, rest)
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
      if rest <> "" && (rest.[0] = ',' || rest.[0] = '_') then
        (Some rest.[0], String.sub rest 1 (String.length rest - 1))
      else (None, rest)
    in
    let precision, rest =
      if rest <> "" && rest.[0] = '.' then
        let p, r = digits (String.sub rest 1 (String.length rest - 1)) in
        (p, r)
      else (None, rest)
    in
    let conv = rest in
    (* group an unsigned digit string from the right with [sep] every [size] *)
    let group_digits sep size s =
      let rec go acc s =
        let n = String.length s in
        if n <= size then s :: acc
        else
          go (String.sub s (n - size) size :: acc) (String.sub s 0 (n - size))
      in
      String.concat (String.make 1 sep) (go [] s)
    in
    (* apply grouping to a signed (possibly fractional) numeric string *)
    let group_number size s =
      match grouping with
      | None -> s
      | Some sep ->
          let intpart, frac =
            match String.index_opt s '.' with
            | Some i -> (String.sub s 0 i, String.sub s i (String.length s - i))
            | None -> (s, "")
          in
          group_digits sep size intpart ^ frac
    in
    (* the sign prefix for a numeric body, given whether the value is negative *)
    let sign_prefix neg =
      if neg then "-" else match sign with '+' -> "+" | ' ' -> " " | _ -> ""
    in
    (* build a numeric result: (sign+altprefix, grouped magnitude) *)
    let* (prefix, mag, numeric), st =
      match (v, conv) with
      | _, ("" | "d" | "n") when as_z v <> None ->
          let z = Option.get (as_z v) in
          let s = Z.to_string (Z.abs z) in
          Ok ((sign_prefix (Z.sign z < 0), group_number 3 s, true), st)
      | _, ("x" | "X" | "o" | "b") when as_z v <> None ->
          let z = Option.get (as_z v) in
          let conv_fmt = "%" ^ conv in
          let s = Z.format conv_fmt (Z.abs z) in
          let altp =
            if not alt then ""
            else
              match conv with
              | "x" -> "0x"
              | "X" -> "0X"
              | "o" -> "0o"
              | _ -> "0b"
          in
          let size = match conv with "" | "d" | "n" -> 3 | _ -> 4 in
          Ok ((sign_prefix (Z.sign z < 0) ^ altp, group_number size s, true), st)
      | _, "f" when is_number v ->
          let x = Option.get (as_float v) in
          let p = Option.value precision ~default:6 in
          let s = Printf.sprintf "%.*f" p (Float.abs x) in
          Ok ((sign_prefix (1. /. x < 0. || x < 0.), group_number 3 s, true), st)
      | _, ("e" | "E") when is_number v ->
          let x = Option.get (as_float v) in
          let p = Option.value precision ~default:6 in
          let s = Printf.sprintf "%.*e" p (Float.abs x) in
          let s = if conv = "E" then String.uppercase_ascii s else s in
          Ok ((sign_prefix (x < 0.), s, true), st)
      | _, ("g" | "G") when is_number v ->
          let x = Option.get (as_float v) in
          let p = max 1 (Option.value precision ~default:6) in
          let s = Printf.sprintf "%.*g" p (Float.abs x) in
          let s = if conv = "G" then String.uppercase_ascii s else s in
          Ok ((sign_prefix (x < 0.), s, true), st)
      | _, "%" when is_number v ->
          let x = Option.get (as_float v) in
          let p = Option.value precision ~default:6 in
          let s = Printf.sprintf "%.*f%%" p (Float.abs x *. 100.) in
          Ok ((sign_prefix (x < 0.), s, true), st)
      | _, ("" | "s") ->
          let* s, st = py_str st v in
          let s =
            match precision with
            | Some p when utf8_length s > p -> utf8_sub s ~pos:0 ~len:p
            | _ -> s
          in
          Ok (("", s, is_number v), st)
      | _ ->
          raise_py st "ValueError"
            (Printf.sprintf "unsupported format spec '%s'" spec)
    in
    let body = prefix ^ mag in
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
            | '=' -> prefix ^ mk pad ^ mag
            | _ -> body)
    in
    Ok (result, st)

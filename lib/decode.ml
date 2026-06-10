let format_version = 1
let expected_python_prefix = "3.13."

type json = Yojson.Safe.t

exception Fail of string * string
exception Unknown_op of string

let fail ctx msg = raise (Fail (ctx, msg))

let shape j =
  match (j : json) with
  | `Null -> "null"
  | `Bool _ -> "bool"
  | `Int _ | `Intlit _ -> "int"
  | `Float _ -> "float"
  | `String _ -> "string"
  | `List _ -> "list"
  | `Assoc _ -> "object"

let to_int ctx : json -> int = function
  | `Int n -> n
  | j -> fail ctx ("expected int, got " ^ shape j)

let field ctx fields name =
  match List.assoc_opt name fields with
  | Some v -> v
  | None -> fail ctx ("missing field " ^ name)

let nibble ctx = function
  | '0' .. '9' as c -> Char.code c - Char.code '0'
  | 'a' .. 'f' as c -> Char.code c - Char.code 'a' + 10
  | 'A' .. 'F' as c -> Char.code c - Char.code 'A' + 10
  | _ -> fail ctx "invalid hex digit"

let hex_decode ctx s =
  let n = String.length s in
  if n mod 2 <> 0 then fail ctx "odd-length hex string";
  String.init (n / 2) (fun i ->
      Char.chr ((nibble ctx s.[2 * i] lsl 4) lor nibble ctx s.[(2 * i) + 1]))

let tagged_string ctx fields =
  match field ctx fields "v" with
  | `String s -> s
  | j -> fail ctx ("expected string payload, got " ^ shape j)

(* A Python str: plain JSON string, or {"t":"sw","v":<hex of WTF-8>} for
   strings containing lone surrogates. *)
let pystr ctx : json -> string = function
  | `String s -> s
  | `Assoc fields when List.assoc_opt "t" fields = Some (`String "sw") ->
      hex_decode ctx (tagged_string ctx fields)
  | j -> fail ctx ("expected Python string, got " ^ shape j)

let float_of_hex ctx s =
  match float_of_string_opt s with
  | Some f -> f
  | None -> fail ctx ("invalid float literal " ^ s)

let local_kind ctx : json -> Ast.local_kind = function
  | `String "l" -> Local
  | `String "lc" -> Local_and_cell
  | `String "c" -> Cell
  | `String "f" -> Free
  | j -> fail ctx ("invalid local kind " ^ Yojson.Safe.to_string j)

let rec const ctx : json -> Ast.const = function
  | `Null -> None_
  | `Bool b -> Bool b
  | `Int n -> Int (Z.of_int n)
  | `Intlit s -> Int (Z.of_string s)
  | `String s -> Str s
  | `List xs -> Tuple (Array.of_list (List.map (const ctx) xs))
  | `Assoc fields as j -> (
      match List.assoc_opt "t" fields with
      | Some (`String "f") ->
          Float (float_of_hex ctx (tagged_string ctx fields))
      | Some (`String "z") ->
          let part name =
            match field ctx fields name with
            | `String s -> float_of_hex ctx s
            | j -> fail ctx ("complex part: expected string, got " ^ shape j)
          in
          Complex { re = part "re"; im = part "im" }
      | Some (`String "b") -> Bytes (hex_decode ctx (tagged_string ctx fields))
      | Some (`String "sw") -> Str (hex_decode ctx (tagged_string ctx fields))
      | Some (`String "fs") -> (
          match field ctx fields "v" with
          | `List xs -> Frozenset (Array.of_list (List.map (const ctx) xs))
          | j -> fail ctx ("frozenset: expected list, got " ^ shape j))
      | Some (`String "el") -> Ellipsis
      | Some (`String "co") -> Code (code ctx (field ctx fields "v"))
      | _ -> fail ctx ("unknown constant tag in " ^ Yojson.Safe.to_string j))
  | j -> fail ctx ("invalid constant: " ^ shape j)

and instr_row ctx : json -> Ast.instr * int * Ast.positions option = function
  | `List (`String opname :: `Int arg :: `Int line :: rest) ->
      let op =
        match Opcode.of_string opname with
        | Some op -> op
        | None -> raise (Unknown_op opname)
      in
      let pos =
        match rest with
        | [] -> None
        | [ `List [ `Int l; `Int el; `Int c; `Int ec ] ] ->
            Some
              {
                Ast.lineno = l;
                end_lineno = el;
                col_offset = c;
                end_col_offset = ec;
              }
        | _ -> fail ctx "invalid positions in instruction row"
      in
      ({ Ast.op; arg }, line, pos)
  | j -> fail ctx ("invalid instruction row: " ^ Yojson.Safe.to_string j)

and exn_row ctx : json -> Ast.exn_entry = function
  | `List [ `Int s; `Int e; `Int t; `Int d; `Int l ] ->
      {
        start_idx = s;
        end_idx = e;
        target_idx = t;
        depth = d;
        push_lasti = l <> 0;
      }
  | j -> fail ctx ("invalid exception-table row: " ^ Yojson.Safe.to_string j)

and localsplus_row ctx : json -> string * Ast.local_kind = function
  | `List [ name; kind ] -> (pystr ctx name, local_kind ctx kind)
  | j -> fail ctx ("invalid localsplus row: " ^ Yojson.Safe.to_string j)

and code ctx : json -> Ast.code = function
  | `Assoc fields ->
      let ctx =
        match List.assoc_opt "qualname" fields with
        | Some (`String q) -> ctx ^ "/" ^ q
        | _ -> ctx
      in
      let f name = field ctx fields name in
      let int name = to_int (ctx ^ "." ^ name) (f name) in
      let str name = pystr (ctx ^ "." ^ name) (f name) in
      let list name =
        match f name with
        | `List l -> l
        | j -> fail (ctx ^ "." ^ name) ("expected list, got " ^ shape j)
      in
      let rows = List.map (instr_row ctx) (list "instrs") in
      let instrs = Array.of_list (List.map (fun (i, _, _) -> i) rows) in
      let lines = Array.of_list (List.map (fun (_, l, _) -> l) rows) in
      let positions =
        if rows <> [] && List.for_all (fun (_, _, p) -> p <> None) rows then
          Array.of_list (List.map (fun (_, _, p) -> Option.get p) rows)
        else [||]
      in
      {
        Ast.filename = str "filename";
        name = str "name";
        qualname = str "qualname";
        firstlineno = int "firstlineno";
        argcount = int "argcount";
        posonlyargcount = int "posonlyargcount";
        kwonlyargcount = int "kwonlyargcount";
        nlocals = int "nlocals";
        stacksize = int "stacksize";
        flags = int "flags";
        consts =
          Array.of_list (List.map (const (ctx ^ ".consts")) (list "consts"));
        names = Array.of_list (List.map (pystr (ctx ^ ".names")) (list "names"));
        localsplus =
          Array.of_list
            (List.map
               (localsplus_row (ctx ^ ".localsplus"))
               (list "localsplus"));
        instrs;
        exn_table =
          Array.of_list (List.map (exn_row (ctx ^ ".exn")) (list "exn"));
        lines;
        positions;
      }
  | j -> fail ctx ("expected code object, got " ^ shape j)

let error_of_doc ctx ~python fields =
  let str name =
    match List.assoc_opt name fields with Some (`String s) -> s | _ -> ""
  in
  let int name =
    match List.assoc_opt name fields with Some (`Int n) -> n | _ -> -1
  in
  match str "kind" with
  | "syntax" | "compile" ->
      Error.Python_syntax_error
        {
          msg = str "msg";
          filename = str "filename";
          line = int "line";
          col = int "col";
          text = str "text";
        }
  | "io" -> Io_error (str "msg")
  | "version" ->
      Version_mismatch { expected = expected_python_prefix ^ "x"; got = python }
  | kind ->
      Decode_error
        {
          context = ctx;
          msg = "unknown error kind " ^ Printf.sprintf "%S" kind;
        }

let code_of_envelope (j : json) : (Ast.code, Error.t) result =
  match j with
  | `Assoc fields -> (
      match List.assoc_opt "format" fields with
      | Some (`Int v) when v = format_version -> (
          let python =
            match List.assoc_opt "python" fields with
            | Some (`String v) -> v
            | _ -> "unknown"
          in
          match List.assoc_opt "ok" fields with
          | Some (`Bool true) -> (
              if not (String.starts_with ~prefix:expected_python_prefix python)
              then
                Error
                  (Version_mismatch
                     { expected = expected_python_prefix ^ "x"; got = python })
              else
                match List.assoc_opt "code" fields with
                | Some code_json -> (
                    try Ok (code "" code_json) with
                    | Fail (context, msg) ->
                        Error (Decode_error { context; msg })
                    | Unknown_op name -> Error (Unknown_opcode name)
                    | Invalid_argument msg | Failure msg ->
                        Error (Decode_error { context = ""; msg }))
                | None ->
                    Error
                      (Decode_error
                         { context = "envelope"; msg = "missing code field" }))
          | Some (`Bool false) -> (
              match List.assoc_opt "error" fields with
              | Some (`Assoc err) -> Error (error_of_doc "envelope" ~python err)
              | _ ->
                  Error
                    (Decode_error
                       { context = "envelope"; msg = "missing error field" }))
          | _ ->
              Error
                (Decode_error { context = "envelope"; msg = "missing ok field" })
          )
      | Some j ->
          Error
            (Decode_error
               {
                 context = "envelope";
                 msg = "unsupported format " ^ Yojson.Safe.to_string j;
               })
      | None ->
          Error
            (Decode_error { context = "envelope"; msg = "missing format field" })
      )
  | j ->
      Error
        (Decode_error
           { context = "envelope"; msg = "expected object, got " ^ shape j })

let envelope_file (j : json) =
  match j with
  | `Assoc fields -> (
      match List.assoc_opt "file" fields with
      | Some (`String s) -> Some s
      | _ -> None)
  | _ -> None

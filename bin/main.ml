let usage () =
  prerr_endline
    "usage: pytecode dump [--no-positions] [--python EXE] FILE\n\
    \       pytecode phir [--no-positions] [--python EXE] FILE\n\
    \       pytecode json [--no-positions] [--python EXE] FILE\n\n\
     dump: pretty-print the bytecode AST of a Python file (dis-like)\n\
     phir: pretty-print the Phir (Python High IR) of a Python file\n\
     json: print the raw JSON envelope produced by the dump script";
  exit 2

type opts = { python : string option; positions : bool; file : string }

let parse_opts args =
  let rec go acc = function
    | "--no-positions" :: rest -> go { acc with positions = false } rest
    | "--python" :: exe :: rest -> go { acc with python = Some exe } rest
    | [ file ] when file <> "" && file.[0] <> '-' -> { acc with file }
    | _ -> usage ()
  in
  go { python = None; positions = true; file = "" } args

let () =
  match Array.to_list Sys.argv with
  | _ :: "dump" :: args ->
      let { python; positions; file } = parse_opts args in
      let backend = Pytecode.Subprocess.make ?python ~positions () in
      let (module B : Pytecode.Backend_intf.S) = backend in
      (match B.compile_file file with
      | Ok code -> Format.printf "%a@?" Pytecode.Ast.pp_code code
      | Error e ->
          prerr_endline (Pytecode.Error.to_string e);
          exit 1)
  | _ :: "phir" :: args ->
      let { python; positions; file } = parse_opts args in
      let backend = Pytecode.Subprocess.make ?python ~positions () in
      let (module B : Pytecode.Backend_intf.S) = backend in
      (match B.compile_file file with
      | Ok code -> (
          match Pytecode.Phir.of_code code with
          | phir -> Format.printf "%a@?" Pytecode.Phir.pp_code phir
          | exception Pytecode.Phir.Unsupported msg ->
              prerr_endline ("unsupported: " ^ msg);
              exit 1)
      | Error e ->
          prerr_endline (Pytecode.Error.to_string e);
          exit 1)
  | _ :: "json" :: args ->
      let { python; positions; file } = parse_opts args in
      (match Pytecode.Subprocess.raw_dump ?python ~positions file with
      | Ok json -> print_string json
      | Error e ->
          prerr_endline (Pytecode.Error.to_string e);
          exit 1)
  | _ -> usage ()

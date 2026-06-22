(* Differential test harness: every program in test/programs is run through
   the real python3.13 and through parse -> bytecode -> phir -> interpreter;
   stdout must match byte for byte.

   Usage: interp_runner [DIR] [FILTER]
   FILTER, if given, restricts to tests whose name contains the substring. *)

open Pytecode

let python_stdout path =
  let python = Subprocess.default_python () in
  let cmd = Filename.quote python ^ " " ^ Filename.quote path in
  let ic = Unix.open_process_in cmd in
  let out = In_channel.input_all ic in
  match Unix.close_process_in ic with
  | Unix.WEXITED 0 -> Ok out
  | Unix.WEXITED n -> Error (Printf.sprintf "python exited %d" n)
  | _ -> Error "python killed"

let interp_stdout code =
  match Phir.of_code code with
  | exception Phir.Unsupported msg -> Error ("phir: " ^ msg)
  | phir -> (
      match Pytecode_interp.Interp.run_module phir with
      | Ok out -> Ok out
      | Error msg -> Error msg)

let first_diff a b =
  let la = String.split_on_char '\n' a and lb = String.split_on_char '\n' b in
  let rec go i = function
    | x :: xs, y :: ys when x = y -> go (i + 1) (xs, ys)
    | x :: _, y :: _ -> Printf.sprintf "line %d: python %S, interp %S" i x y
    | x :: _, [] -> Printf.sprintf "line %d: python %S, interp <eof>" i x
    | [], y :: _ -> Printf.sprintf "line %d: python <eof>, interp %S" i y
    | [], [] -> "no diff?"
  in
  go 1 (la, lb)

let contains_sub haystack needle =
  let n = String.length needle and h = String.length haystack in
  let rec go i =
    i + n <= h && (String.sub haystack i n = needle || go (i + 1))
  in
  n = 0 || go 0

let () =
  let dir, filter =
    match Array.to_list Sys.argv with
    | [ _ ] -> ("programs", None)
    | [ _; dir ] -> (dir, None)
    | [ _; dir; f ] -> (dir, Some f)
    | _ ->
        prerr_endline "usage: interp_runner [DIR] [FILTER]";
        exit 2
  in
  (* Programs are organised into subfolders mirroring the Python Language
     Reference (e.g. programs/6_expressions/6.7_arithmetics/...), so walk the
     tree rather than just the top level. *)
  let rec collect dir =
    Sys.readdir dir |> Array.to_list |> List.sort String.compare
    |> List.concat_map (fun entry ->
        let path = Filename.concat dir entry in
        if Sys.is_directory path then collect path
        else if Filename.check_suffix path ".py" then [ path ]
        else [])
  in
  let files =
    collect dir
    |> List.filter (fun f ->
        match filter with None -> true | Some s -> contains_sub f s)
    |> List.sort String.compare
  in
  let (module B : Backend_intf.S) = Loader.default_backend () in
  let compiled = B.compile_batch files in
  let bad =
    List.fold_left
      (fun bad (path, compiled) ->
        let name = Filename.basename path in
        let result =
          match compiled with
          | Error e -> Error ("compile: " ^ Error.to_string e)
          | Ok code -> (
              match (python_stdout path, interp_stdout code) with
              | Error e, _ -> Error ("harness: " ^ e)
              | _, Error e -> Error e
              | Ok expected, Ok actual ->
                  if expected = actual then Ok ()
                  else Error ("diff: " ^ first_diff expected actual))
        in
        match result with
        | Ok () -> bad
        | Error e ->
            Printf.printf "FAIL %-28s %s\n%!" name e;
            bad + 1)
      0 compiled
  in
  let total = List.length compiled in
  Printf.printf "%d/%d passing\n" (total - bad) total;
  if bad > 0 then exit 1

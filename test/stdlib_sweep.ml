(* Acceptance gate: batch-dump every module of the pinned CPython's stdlib
   and decode it into the AST. Asserts zero failures and zero unknown
   opcodes; prints throughput and opcode coverage.

   Run with: dune build @stdlib
   Optional argument [--cached DIR] wraps the backend in the cache layer
   (run twice to measure cold vs warm). *)

open Pytecode

let python_oneliner code =
  let python = Subprocess.default_python () in
  let ic = Unix.open_process_in (Filename.quote python ^ " -c " ^ Filename.quote code) in
  let line = In_channel.input_line ic in
  match (Unix.close_process_in ic, line) with
  | Unix.WEXITED 0, Some line -> line
  | _ -> failwith ("failed to run " ^ python)

let stdlib_dir () =
  python_oneliner "import sysconfig; print(sysconfig.get_paths()['stdlib'])"

let excluded_dir = function
  | "test" | "tests" | "idle_test" | "__pycache__" -> true
  | _ -> false

let rec walk acc dir =
  Array.fold_left
    (fun acc entry ->
      let path = Filename.concat dir entry in
      if Sys.is_directory path then
        if excluded_dir entry then acc else walk acc path
      else if Filename.check_suffix entry ".py" then path :: acc
      else acc)
    acc (Sys.readdir dir)

let () =
  let cache_dir =
    match Array.to_list Sys.argv with
    | [ _; "--cached"; dir ] -> Some dir
    | [ _ ] -> None
    | _ ->
        prerr_endline "usage: stdlib_sweep [--cached DIR]";
        exit 2
  in
  let backend =
    let base = Subprocess.make () in
    match cache_dir with
    | Some dir -> Cache.wrap ~dir base
    | None -> base
  in
  let (module B : Backend_intf.S) = backend in
  let dir = stdlib_dir () in
  let files = List.sort String.compare (walk [] dir) in
  Printf.printf "stdlib: %s (%d files)\n%!" dir (List.length files);
  let t0 = Unix.gettimeofday () in
  let results = B.compile_batch files in
  let dt = Unix.gettimeofday () -. t0 in
  let opcount = Hashtbl.create 256 in
  let codes = ref 0 and instrs = ref 0 and failures = ref 0 in
  let rec count (c : Ast.code) =
    incr codes;
    Array.iter
      (fun { Ast.op; _ } ->
        incr instrs;
        Hashtbl.replace opcount op (1 + Option.value ~default:0 (Hashtbl.find_opt opcount op)))
      c.instrs;
    Array.iter (function Ast.Code n -> count n | _ -> ()) c.consts
  in
  List.iter
    (fun (path, result) ->
      match result with
      | Ok code -> count code
      | Error e ->
          incr failures;
          Printf.printf "FAIL %s: %s\n" path (Error.to_string e))
    results;
  let used = Hashtbl.length opcount in
  Printf.printf
    "%d files in %.1fs (%.1f ms/file) | %d code objects | %d instructions\n"
    (List.length files) dt
    (1000. *. dt /. float_of_int (max 1 (List.length files)))
    !codes !instrs;
  Printf.printf "opcode coverage: %d/%d used; unused:\n" used
    (Array.length Opcode.all);
  Array.iter
    (fun op ->
      if not (Hashtbl.mem opcount op) then
        Printf.printf "  %s\n" (Opcode.to_string op))
    Opcode.all;
  if !failures > 0 then (
    Printf.printf "FAILED: %d files did not round-trip\n" !failures;
    exit 1);
  print_endline "stdlib sweep: OK"

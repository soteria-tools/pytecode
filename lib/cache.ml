let default_dir () =
  let base =
    match Sys.getenv_opt "XDG_CACHE_HOME" with
    | Some d when d <> "" -> d
    | _ -> (
        match Sys.getenv_opt "HOME" with
        | Some h when h <> "" -> Filename.concat h ".cache"
        | _ -> Filename.get_temp_dir_name ())
  in
  Filename.concat base "pytecode"

(* Bumped whenever Ast.code changes incompatibly. The OCaml version is part
   of the value header because Marshal output is not stable across compiler
   versions. *)
let value_header = "PYTECODE-CACHE-1|" ^ Sys.ocaml_version ^ "\n"

let key ~identity ~path ~source =
  Digest.BLAKE256.to_hex
    (Digest.BLAKE256.string
       (String.concat "\x00" [ value_header; identity; path; source ]))

let rec mkdir_p dir =
  if not (Sys.file_exists dir) then (
    mkdir_p (Filename.dirname dir);
    try Unix.mkdir dir 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ())

let read_value file : Ast.code option =
  match In_channel.with_open_bin file In_channel.input_all with
  | exception Sys_error _ -> None
  | data -> (
      let hlen = String.length value_header in
      if String.length data > hlen && String.sub data 0 hlen = value_header
      then
        match (Marshal.from_string data hlen : Ast.code) with
        | code -> Some code
        | exception _ -> None
      else None)

let write_value ~dir file (code : Ast.code) =
  try
    mkdir_p dir;
    let tmp = Filename.temp_file ~temp_dir:dir "pytecode_" ".tmp" in
    Out_channel.with_open_bin tmp (fun oc ->
        Out_channel.output_string oc value_header;
        Out_channel.output_string oc (Marshal.to_string code []));
    Sys.rename tmp file
  with Sys_error _ | Unix.Unix_error _ -> ()

let read_source path =
  match In_channel.with_open_bin path In_channel.input_all with
  | source -> Some source
  | exception Sys_error _ -> None

let wrap ?dir (module B : Backend_intf.S) : (module Backend_intf.S) =
  let dir = match dir with Some d -> d | None -> default_dir () in
  let identity_memo = lazy (B.identity ()) in
  (* No usable identity (e.g. Python unreachable): bypass the cache entirely
     rather than risk stale or mislabeled entries. *)
  let cache_file ~path ~source =
    match Lazy.force identity_memo with
    | Error _ -> None
    | Ok identity ->
        Some (Filename.concat dir (key ~identity ~path ~source ^ ".bin"))
  in
  let through file result =
    (match (file, result) with
    | Some file, Ok code -> write_value ~dir file code
    | _ -> ());
    result
  in
  (module struct
    let name = "cache(" ^ B.name ^ ")"
    let python_version = B.python_version
    let identity = B.identity

    let compile_file path =
      match read_source path with
      | None -> B.compile_file path (* let the backend report the error *)
      | Some source -> (
          let file = cache_file ~path ~source in
          match Option.map read_value file with
          | Some (Some code) -> Ok code
          | _ -> through file (B.compile_file path))

    let compile_string ?(filename = "<string>") source =
      let file = cache_file ~path:filename ~source in
      match Option.map read_value file with
      | Some (Some code) -> Ok code
      | _ -> through file (B.compile_string ~filename source)

    let compile_batch paths =
      let lookups =
        List.map
          (fun path ->
            let file =
              match read_source path with
              | None -> None
              | Some source -> cache_file ~path ~source
            in
            let hit = Option.bind file read_value in
            (path, file, hit))
          paths
      in
      let misses =
        List.filter_map
          (fun (path, _, hit) -> if hit = None then Some path else None)
          lookups
      in
      let fresh = Hashtbl.create (List.length misses) in
      if misses <> [] then
        List.iter
          (fun (path, result) -> Hashtbl.replace fresh path result)
          (B.compile_batch misses);
      List.map
        (fun (path, file, hit) ->
          match hit with
          | Some code -> (path, Ok code)
          | None -> (
              match Hashtbl.find_opt fresh path with
              | Some result -> (path, through file result)
              | None ->
                  ( path,
                    Error
                      (Error.Decode_error
                         {
                           context = "cache";
                           msg = "batch produced no result for this file";
                         }) )))
        lookups
  end)

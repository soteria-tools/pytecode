let default_python () =
  match Sys.getenv_opt "PYTECODE_PYTHON" with
  | Some p when p <> "" -> p
  | _ -> "python3.13"

let resolve_exe exe =
  if String.contains exe '/' then exe
  else
    let dirs =
      match Sys.getenv_opt "PATH" with
      | None -> []
      | Some path -> String.split_on_char ':' path
    in
    match
      List.find_opt
        (fun d -> d <> "" && Sys.file_exists (Filename.concat d exe))
        dirs
    with
    | Some d -> Filename.concat d exe
    | None -> exe

(* The dump script, materialized once per process. *)
let script_file =
  lazy
    (let file = Filename.temp_file "pytecode_dump_" ".py" in
     Out_channel.with_open_bin file (fun oc ->
         Out_channel.output_string oc Dump_script.source);
     at_exit (fun () -> try Sys.remove file with Sys_error _ -> ());
     file)

(* Writing to a child that died early raises EPIPE; without this guard the
   default SIGPIPE disposition would kill the whole process instead. *)
let with_sigpipe_ignored f =
  match Sys.signal Sys.sigpipe Sys.Signal_ignore with
  | exception (Invalid_argument _ | Sys_error _) -> f () (* e.g. Windows *)
  | previous -> Fun.protect ~finally:(fun () -> ignore (Sys.signal Sys.sigpipe previous)) f

let spawn argv =
  let prog = resolve_exe argv.(0) in
  try Ok (prog, Unix.open_process_args_full prog argv (Unix.environment ()))
  with Unix.Unix_error (e, _, _) ->
    Error (Error.Io_error (prog ^ ": " ^ Unix.error_message e))

let exit_code = function
  | Unix.WEXITED n -> n
  | Unix.WSIGNALED n | Unix.WSTOPPED n -> 128 + n

(* Run [argv], feeding [stdin_data] and slurping stdout/stderr.
   Stdout is read fully before stderr: the child only ever writes large data
   to stdout, so the stderr pipe cannot fill up and deadlock. *)
let run ?stdin_data argv : (string * string * int, Error.t) result =
  match spawn argv with
  | Error _ as e -> e
  | Ok (_, ((stdout_c, stdin_c, stderr_c) as channels)) ->
      with_sigpipe_ignored (fun () ->
          (try
             Option.iter (Out_channel.output_string stdin_c) stdin_data;
             Out_channel.close stdin_c
           with Sys_error _ -> ());
          let out = In_channel.input_all stdout_c in
          let err = In_channel.input_all stderr_c in
          let status = Unix.close_process_full channels in
          Ok (out, err, exit_code status))

let dump_args ~positions extra =
  let script = Lazy.force script_file in
  script :: (if positions then extra else "--no-positions" :: extra)

let parse_envelope ~out ~err ~code =
  match Yojson.Safe.from_string out with
  | j -> Decode.code_of_envelope j
  | exception _ ->
      if code <> 0 then Error (Error.Python_failed { exit_code = code; stderr = err })
      else
        Error
          (Error.Decode_error
             { context = "envelope"; msg = "python produced invalid JSON" })

let raw_dump ?python ?(positions = true) path =
  let python = match python with Some p -> p | None -> default_python () in
  match run (Array.of_list (python :: dump_args ~positions [ path ])) with
  | Error _ as e -> e
  | Ok (out, err, code) ->
      if code <> 0 then Error (Python_failed { exit_code = code; stderr = err })
      else Ok out

let make ?python ?(positions = true) () : (module Backend_intf.S) =
  let python = match python with Some p -> p | None -> default_python () in
  let dump ?stdin_data extra =
    match run ?stdin_data (Array.of_list (python :: dump_args ~positions extra)) with
    | Error _ as e -> e
    | Ok (out, err, code) -> parse_envelope ~out ~err ~code
  in
  (module struct
    let name = "subprocess"

    let python_version () =
      let argv =
        [| python; "-c"; "import sys; print('%d.%d.%d' % sys.version_info[:3])" |]
      in
      match run argv with
      | Error _ as e -> e
      | Ok (out, _, 0) -> Ok (String.trim out)
      | Ok (_, err, code) ->
          Error (Python_failed { exit_code = code; stderr = err })

    let identity_memo =
      lazy
        (match python_version () with
        | Error _ as e -> e
        | Ok version ->
            Ok
              (Printf.sprintf "subprocess|python=%s|positions=%b|script=%s|format=%d"
                 version positions
                 (Digest.BLAKE256.to_hex
                    (Digest.BLAKE256.string Dump_script.source))
                 Decode.format_version))

    let identity () = Lazy.force identity_memo

    let compile_file path = dump [ path ]

    let compile_string ?(filename = "<string>") source =
      dump ~stdin_data:source [ "--stdin-source"; "--filename"; filename ]

    (* Streaming: decode each NDJSON line as it arrives instead of slurping
       the (potentially very large) combined output. The script reads the
       whole path list before writing anything, so write-all-then-read cannot
       deadlock. *)
    let compile_batch paths =
      match
        spawn (Array.of_list (python :: dump_args ~positions [ "--batch" ]))
      with
      | Error e -> List.map (fun p -> (p, Error e)) paths
      | Ok (_, ((stdout_c, stdin_c, stderr_c) as channels)) ->
          with_sigpipe_ignored (fun () ->
              (try
                 List.iter
                   (fun p ->
                     Out_channel.output_string stdin_c p;
                     Out_channel.output_char stdin_c '\n')
                   paths;
                 Out_channel.close stdin_c
               with Sys_error _ -> ());
              let results = Hashtbl.create (List.length paths) in
              let rec read_lines () =
                match In_channel.input_line stdout_c with
                | None -> ()
                | Some line ->
                    (match Yojson.Safe.from_string line with
                    | j -> (
                        match Decode.envelope_file j with
                        | Some file ->
                            Hashtbl.replace results file
                              (Decode.code_of_envelope j)
                        | None -> ())
                    | exception _ -> ());
                    read_lines ()
              in
              read_lines ();
              let err = In_channel.input_all stderr_c in
              let status = Unix.close_process_full channels in
              let code = exit_code status in
              let missing =
                if code <> 0 then
                  Error (Error.Python_failed { exit_code = code; stderr = err })
                else
                  Error
                    (Error.Decode_error
                       {
                         context = "batch";
                         msg = "no envelope produced for this file";
                       })
              in
              List.map
                (fun p ->
                  match Hashtbl.find_opt results p with
                  | Some r -> (p, r)
                  | None -> (p, missing))
                paths)
  end)

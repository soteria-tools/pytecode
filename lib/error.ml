type t =
  | Python_syntax_error of {
      msg : string;
      filename : string;
      line : int;
      col : int;
      text : string;
    }
      (** The source does not compile. [text] is the offending source line (""
          when unavailable); [line]/[col] are 1-based ([-1] when unavailable).
      *)
  | Python_failed of { exit_code : int; stderr : string }
      (** The Python process failed for a reason other than a syntax error
          (interpreter missing, internal error, ...). *)
  | Decode_error of { context : string; msg : string }
      (** The JSON produced by the dump script does not match the expected wire
          format. *)
  | Unknown_opcode of string
      (** The dump contains an opcode name absent from {!Opcode.t} — the pinned
          Python and the generated opcode table have drifted. *)
  | Version_mismatch of { expected : string; got : string }
      (** The Python that produced the dump is not the pinned version. *)
  | Io_error of string

let to_string = function
  | Python_syntax_error { msg; filename; line; col; text } ->
      let where =
        if line >= 0 then Printf.sprintf "%s:%d:%d" filename line col
        else filename
      in
      let text = if text = "" then "" else Printf.sprintf "\n  %s" text in
      Printf.sprintf "%s: syntax error: %s%s" where msg text
  | Python_failed { exit_code; stderr } ->
      Printf.sprintf "python failed (exit %d):\n%s" exit_code stderr
  | Decode_error { context; msg } ->
      Printf.sprintf "decode error at %s: %s" context msg
  | Unknown_opcode name ->
      Printf.sprintf
        "unknown opcode %S (pinned Python and generated Opcode module have \
         drifted; regenerate with `dune build @gen`)"
        name
  | Version_mismatch { expected; got } ->
      Printf.sprintf "Python version mismatch: expected %s, got %s" expected got
  | Io_error msg -> Printf.sprintf "I/O error: %s" msg

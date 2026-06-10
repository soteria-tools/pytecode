(** A bytecode acquisition backend: anything that can turn Python source into an
    {!Ast.code}. The default is {!Subprocess} (runs the pinned CPython with the
    embedded dump script); an in-process pyml backend, or a native [.pyc]
    reader, can implement the same interface. *)
module type S = sig
  val name : string

  val python_version : unit -> (string, Error.t) result
  (** Full version of the CPython this backend compiles with. *)

  val identity : unit -> (string, Error.t) result
  (** A string capturing everything that affects this backend's output for a
      given source: backend name and configuration, dump-script hash, full
      Python version, wire-format version. Cache layers key on
      [(identity, path, source)]. *)

  val compile_file : string -> (Ast.code, Error.t) result

  val compile_string : ?filename:string -> string -> (Ast.code, Error.t) result
  (** [filename] defaults to ["<string>"]. *)

  val compile_batch : string list -> (string * (Ast.code, Error.t) result) list
  (** Compile many files with a single Python process (NDJSON streaming). Order
      is preserved; per-file failures don't abort the batch. *)
end

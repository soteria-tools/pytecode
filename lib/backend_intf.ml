(** A bytecode acquisition backend: anything that can turn Python source into
    an {!Ast.code}. The default is {!Subprocess} (runs the pinned CPython with
    the embedded dump script); an in-process pyml backend, or a native [.pyc]
    reader, can implement the same interface. *)
module type S = sig
  val name : string

  val python_version : unit -> (string, Error.t) result
  (** Full version of the CPython this backend compiles with. *)

  val compile_file : string -> (Ast.code, Error.t) result

  val compile_string : ?filename:string -> string -> (Ast.code, Error.t) result
  (** [filename] defaults to ["<string>"]. *)

  val compile_batch : string list -> (string * (Ast.code, Error.t) result) list
  (** Compile many files with a single Python process (NDJSON streaming).
      Order is preserved; per-file failures don't abort the batch. *)
end

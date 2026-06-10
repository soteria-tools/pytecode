(** Shared decoder from the dump script's JSON wire format to {!Ast.code}. Used
    by every backend (subprocess now, pyml later), so all backends are validated
    by the same tests. *)

val format_version : int
(** Wire-format version this decoder understands (envelope ["format"]). *)

val expected_python_prefix : string
(** Pinned CPython version prefix, e.g. ["3.13."]. Envelopes produced by any
    other version are rejected with {!Error.Version_mismatch}. *)

val code_of_envelope : Yojson.Safe.t -> (Ast.code, Error.t) result
(** Decode one envelope ([{"format": ..., "python": ..., "ok": ..., ...}]). *)

val envelope_file : Yojson.Safe.t -> string option
(** The ["file"] field of a batch-mode envelope. *)

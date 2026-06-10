(** The default acquisition backend: runs the pinned CPython as a subprocess
    with the embedded dump script ({!Dump_script.source}).

    Python executable resolution: [?python] argument, else [$PYTECODE_PYTHON],
    else ["python3.13"] looked up in [$PATH]. *)

val default_python : unit -> string

val make : ?python:string -> ?positions:bool -> unit -> (module Backend_intf.S)
(** [positions] (default [true]): include column-level position info for every
    instruction; disable for smaller dumps when line numbers suffice. *)

val raw_dump :
  ?python:string -> ?positions:bool -> string -> (string, Error.t) result
(** The raw JSON envelope for a file, as emitted by the dump script — debugging
    aid (e.g. [pytecode json]). *)

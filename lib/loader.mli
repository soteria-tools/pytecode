(** Front door: Python source in, {!Ast.code} out. *)

val default_backend : unit -> (module Backend_intf.S)
(** The subprocess backend with default configuration (created once,
    memoized). *)

val load_file :
  ?backend:(module Backend_intf.S) -> string -> (Ast.code, Error.t) result

val load_string :
  ?backend:(module Backend_intf.S) ->
  ?filename:string ->
  string ->
  (Ast.code, Error.t) result

(** Content-addressed cache layer over any backend.

    Keys are BLAKE-256 of (backend {!Backend_intf.S.identity}, file path, source
    bytes), so cache entries are invalidated automatically by: source edits,
    Python patch upgrades, dump-script changes, wire-format bumps, or backend
    configuration changes. Values are [Marshal]ed {!Ast.code} with a magic +
    OCaml-version header; any mismatch or read failure falls back to a
    transparent re-dump. Warm hits never touch Python. *)

val default_dir : unit -> string
(** [$XDG_CACHE_HOME/pytecode], else [~/.cache/pytecode], else a temp dir. *)

val wrap : ?dir:string -> (module Backend_intf.S) -> (module Backend_intf.S)

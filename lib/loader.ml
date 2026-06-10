let default = lazy (Subprocess.make ())
let cached_default = lazy (Cache.wrap (Lazy.force default))
let default_backend () = Lazy.force default

let get ~cache = function
  | Some b -> if cache then Cache.wrap b else b
  | None -> if cache then Lazy.force cached_default else Lazy.force default

let load_file ?backend ?(cache = false) path =
  let (module B : Backend_intf.S) = get ~cache backend in
  B.compile_file path

let load_string ?backend ?(cache = false) ?filename source =
  let (module B : Backend_intf.S) = get ~cache backend in
  B.compile_string ?filename source

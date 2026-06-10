let default = lazy (Subprocess.make ())

let default_backend () = Lazy.force default

let get = function Some b -> b | None -> default_backend ()

let load_file ?backend path =
  let (module B : Backend_intf.S) = get backend in
  B.compile_file path

let load_string ?backend ?filename source =
  let (module B : Backend_intf.S) = get backend in
  B.compile_string ?filename source

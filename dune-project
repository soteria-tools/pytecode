(lang dune 3.16)

(name pytecode)

(generate_opam_files true)

(license MIT)

(authors "Sacha Ayoun")

(maintainers "sachaayoun@gmail.com")

(package
 (name pytecode)
 (synopsis "CPython bytecode AST for OCaml")
 (description
  "Compiles Python source with the host CPython (pinned 3.13) and exposes the\nresulting bytecode as a pure OCaml data structure: an array of instructions\nwith opcodes as a variant type, plus full code-object metadata (constants,\nnames, localsplus, exception table, line/position info). Intended as the\nfoundation for a Python interpreter written in OCaml.")
 (depends
  (ocaml
   (>= 5.2))
  (yojson
   (>= 2.0))
  (zarith
   (>= 1.12))
  (fmt
   (>= 0.9))
  (ppxlib
   (>= 0.32))
  ppx_deriving
  (ppx_expect :with-test)))

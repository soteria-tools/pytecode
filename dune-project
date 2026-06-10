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
  "Compiles Python source with the host CPython (pinned 3.13) and exposes the\n\
   resulting bytecode as a pure OCaml data structure: an array of instructions\n\
   with opcodes as a variant type, plus full code-object metadata (constants,\n\
   names, localsplus, exception table, line/position info). Intended as the\n\
   foundation for a Python interpreter written in OCaml.")
 (depends
  (ocaml (>= 5.2))
  (yojson (>= 2.0))
  (zarith (>= 1.12))
  (ppx_expect :with-test)))

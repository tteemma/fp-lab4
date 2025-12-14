(lang dune 3.10)
(name fp_search)
(generate_opam_files true)

(package
 (name fp_search)
 (synopsis "Mini full-text search engine (index + boolean queries + phrase search) in OCaml")
 (description "A tiny full-text search engine: indexes a directory, supports boolean queries (AND/OR/NOT), parentheses, and phrase search via token positions. Includes CLI, REPL, and tests.")
 (depends
  (ocaml (>= 5.0))
  dune
  alcotest))

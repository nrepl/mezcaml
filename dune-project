(lang dune 3.0)
(name mezcaml)

(generate_opam_files true)

(source (github nrepl/mezcaml))
(authors "Bozhidar Batsov <bozhidar.batsov@gmail.com>")
(maintainers "Bozhidar Batsov <bozhidar.batsov@gmail.com>")
(license MIT)

(package
 (name mezcaml)
 (synopsis "A minimal nREPL client for OCaml")
 (description
  "mezcaml is a small, language-agnostic nREPL client library and command-line
tool. It speaks the default nREPL transport (bencode over a TCP socket) and
supports the core protocol operations: clone, describe, eval, load-file,
completions, interrupt and close.")
 (depends
  (ocaml (>= 4.10))
  dune
  base-unix))

(lang dune 2.9)

(name ocamlformat-mdx-blocks)

(generate_opam_files true)

(authors "Guillaume Petiot <guillaume@tarides.com>")

(maintainers "OCamlFormat Team <ocamlformat-dev@lists.ocaml.org>")

(source
 (github gpetiot/ocamlformat-mdx-blocks))

(license MIT)

(package
 (name ocamlformat-mdx-blocks)
 (synopsis "Formatting MDX blocks in markdown files with OCamlFormat")
 (description
  "ocamlformat-mdx-blocks is a driver tool leveraging OCamlFormat to automatically format OCaml and Toplevel code blocks in markdown files (following the MDX syntax).")
 (depends
  (ocaml
   (and
    (>= 4.08)
    (< 4.15)))
  (cmdliner (>= 1.0.0))
  (fmt (>= 0.8.7))
  (logs (>= 0.7.0))
  (ocamlformat :dev)))

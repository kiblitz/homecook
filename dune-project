(lang dune 3.18)

(version 0.0.1)

(generate_opam_files)

(name homecook)

(source
 (github kiblitz/homecook))

(license MIT)

(maintainers kiblitz)

(authors kiblitz)

(package
 (name homecook)
 (synopsis "homecook")
 (description "homecook")
 (allow_empty)
 (depends
  "bonsai"
  "core"
  "core_kernel"
  ("dune"
   (>= "3.18.1"))
  "js_of_ocaml"
  "js_of_ocaml-ppx"
  ("ocaml"
   (= "5.1.0"))
  "ocaml-embed-file"
  "ppx_jane"
  "virtual_dom"))

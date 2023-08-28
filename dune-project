(lang dune 3.10)
(map_workspace_root false)

(name cast)

(generate_opam_files true)

(maintainers "Wybxc <wybxc@qq.com>")

(license Apache-2.0)

(package
 (name cast)
 (synopsis "C ast printer")
 (depends ocaml dune core pprint yojson)
 (tags
  (topics "")))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project

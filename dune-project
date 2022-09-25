(lang dune 3.4)

(name pacman)

(generate_opam_files true)

(source
 (github zyc2024/pacman))

(authors "djb326 dls422 zc333")

(maintainers "Zhiyuan Chen")

(license MIT)

; (documentation https://url/to/documentation)

(package
 (name pacman)
 (synopsis "A simple version of classic pacman")
 (description
  "A collection of modules each representing the components of the classic pacman game.")
 (depends ocaml dune)
 (tags
  ("game" "functional" "imperative")))

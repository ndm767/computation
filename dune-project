(lang dune 3.17)

(name computation)

(using menhir 3.0)

(generate_opam_files true)

(source
 (github ndm767/computation))

(authors "Nathan Medros <nathan.medros@gmail.com>")

(maintainers "Nathan Medros <nathan.medros@gmail.com>")

(license MIT)


(package
 (name computation)
 (synopsis "Models of computation in OCaml.")
 (description "Implementations of table and instruction Turing machines, register machines, Markov algorithms, and recursive functions over the natural numbers as interpreted programming languages written in OCaml. Based upon the material from Ma/CS 117a Computability at Caltech.")
 (depends ocaml)
)

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html

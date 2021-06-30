(lang dune 2.7)
(name muf)
(version 0.1.0)
(using menhir 2.1)

(generate_opam_files true)

(source (github psg-mit/probzelus-analysis-impl))
(license Apache-2.0)
(authors  "Eric Atkinson"
          "Guillaume Baudart"
          "Michael Carbin"
          "Louis Mandel"
          "Charles Yuan")
(maintainers "Louis Mandel")

(package
 (name muf)
 (synopsis "The muF analyzer")
 (description "Static analyzer for delayed sampling on muF programs"))

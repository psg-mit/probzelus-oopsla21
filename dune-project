(lang dune 2.7)
(name muf)
(version 0.1.0)
(using menhir 2.1)

(generate_opam_files true)

(source (github psg-mit/probzelus-analysis-impl))
(license Apache-2.0)
(authors "Charles Yuan" "Louis Mandel")
(maintainers "Louis Mandel")

(package
 (name muf)
 (synopsis "The muF compiler")
 (description "A compiler for the muF language.")
 (depends
  (muf-compiler-libs (= 0.1.0))))

(package
 (name muf-compiler-libs)
 (synopsis "Internal muF compiler library")
 (description "Internal muF compiler library.")
 (depends
  (dune (> 1.5))))

.PHONY: all build opam
.PHONY: clean cleanall


build:
	dune build @install

install:
	opam reinstall -y muf-compiler-libs muf

opam: muf.opam muf-compiler-libs.opam
	opam pin -k path .

muf.opam muf-compiler-libs.opam: dune-project
	dune build $<

clean:
	rm -rf _build

cleanall: clean
	rm -rf *~ */*~ */*/*~

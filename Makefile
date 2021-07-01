.PHONY: all build init tests
.PHONY: clean cleanall


build:
	dune build

init:
	opam pin -k path .

install:
	opam reinstall -y muf

tests:
	make -C tests

clean:
	rm -rf _build
	make -C tests clean

cleanall: clean
	rm -rf *~ */*~ */*/*~

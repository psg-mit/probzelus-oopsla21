.PHONY: all build init tests
.PHONY: clean cleanall


build:
	dune build

init:
	opam pin -y -k path .

install:
	opam reinstall -y muf

tests:
	make -C tests

bench:
	make -C tests bench

clean:
	dune clean
	make -C tests clean

cleanall: clean
	rm -rf *~ */*~ */*/*~

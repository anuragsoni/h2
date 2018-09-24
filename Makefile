.PHONY: default build install uninstall test clean utop

default: build

build:
	dune build

test:
	dune runtest -f

install:
	dune install

uninstall:
	dune uninstall

clean:
	dune clean

utop:
	dune utop lib

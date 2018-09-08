.PHONY: default build install uninstall test clean utop-frames

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

utop-frames:
	dune utop lib

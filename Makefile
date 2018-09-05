.PHONY: default build install uninstall test clean utop-http2

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

utop-http2:
	dune utop lib

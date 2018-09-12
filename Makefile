.PHONY: default build install uninstall test clean utop-frames utop-h2

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
	dune utop frames/lib

utop-h2:
	dune utop h2/lib

.PHONY: default build install uninstall test clean utop

default: build

build:
	jbuilder build

test:
	jbuilder runtest -f

install:
	jbuilder install

uninstall:
	jbuilder uninstall

clean:
	jbuilder clean
	git clean -dfXq

utop:
	jbuilder utop src

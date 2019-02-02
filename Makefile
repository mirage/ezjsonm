.PHONY: build clean test doc install uninstall

build:
	dune build

test:
	dune runtest

doc:
	dune build @doc

install:
	dune install

uninstall:
	dune uninstall

clean:
	rm -rf _build

# Makefile

.PHONY: all clean install

all: ecma_parser.ml ecmarex_parser.ml
	ocamlbuild.native jsure.native

install: all
	cp jsure.native ~/bin/jsure
	cp jsure.native ~/src/baagz/trunk/admin/jsure

ecma_parser.ml: ecma.peg
	aurochs -target ml -base ecma_parser -generate ecma.peg

ecmarex_parser.ml: ecmarex.peg
	aurochs -target ml -base ecmarex_parser -generate ecmarex.peg

clean:
	rm -rf _build

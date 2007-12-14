# Makefile

.PHONY: all clean install

PREFIX?=/usr/local

all:
	ocamlbuild jsure.native

install: all
	install -m 0755 jsure.native $(PREFIX)/bin/jsure

clean:
	rm -rf _build _log jsure.native

# Makefile

.PHONY: all clean install tests jsure

PREFIX?=/usr/local

all: jsure

jsure:
	ocamlbuild jsure.native

install: all
	install -m 0755 jsure.native $(PREFIX)/bin/jsure

clean:
	rm -rf _build _log jsure.native

tests: jsure
	@cd tests; ./test.sh

# Makefile

.PHONY: all clean install

all:
	ocamlbuild.native jsure.native

install: all
	cp jsure.native ~/bin/jsure
	cp jsure.native ~/src/baagz/trunk/admin/jsure

clean:
	rm -rf _build _log

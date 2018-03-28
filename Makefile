build:
	jbuilder build

_opam:
	opam switch create . --empty

setup: _opam
	opam install ocaml-base-compiler.4.06.0
	opam install . --deps-only --with-test

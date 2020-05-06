.PHONY: all test
all:
	ocamlbuild -package llvm tilisp.native
test:
	ocamlbuild -lib unix testing.native
	./testing.native

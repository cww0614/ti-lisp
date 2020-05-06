.PHONY: all test
all: 
	ocamlbuild tilisp.native
test:
	ocamlbuild -lib unix testing.native
	./testing.native

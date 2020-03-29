.PHONY: all
all: scanner.mll ast.mli tilisp.ml parser.mly
	ocamllex scanner.mll
	ocamlyacc parser.mly
	ocamlc -c ast.mli
	ocamlc -c parser.mli
	ocamlc -c scanner.ml
	ocamlc -c parser.ml
	ocamlc -c tilisp.ml
	ocamlc -o tilisp parser.cmo scanner.cmo tilisp.cmo

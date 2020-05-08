.PHONY: all test clean

SRC_FILES = ast.ml irgen.ml macro.ml parser.mly sast.ml scanner.mll semant.ml symtable.ml tilisp.ml utils.ml
TEST_FILES = testing.ml

all: tilisp.native testing.native libtilisp.a

tilisp.native: $(SRC_FILES)
	ocamlbuild -no-hygiene -package llvm tilisp.native

testing.native: $(TEST_FILES)
	ocamlbuild -lib unix testing.native

tilisp.o: tilisp.h tilisp.cpp helper.cpp helper.h
	g++ tilisp.cpp -c -o tilisp.o
	g++ helper.cpp -c -o helper.o

libtilisp.a: tilisp.o helper.o
	ar -crs libtilisp.a tilisp.o helper.o
	ranlib libtilisp.a

test: testing.native libtilisp.a
	./testing.native

clean:
	rm -rf *.o *.a *.ll _build *.native

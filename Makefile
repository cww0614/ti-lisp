.PHONY: all test clean

SRC_FILES = ast.ml irgen.ml macro.ml parser.mly sast.ml scanner.mll semant.ml symtable.ml tilisp.ml utils.ml
TEST_FILES = testing.ml

all: tilisp.native testing.native libbuiltins.a

tilisp.native: $(SRC_FILES)
	ocamlbuild -no-hygiene -package llvm tilisp.native

testing.native: $(TEST_FILES)
	ocamlbuild -lib unix testing.native

builtins.o: builtins.h builtins.c
	gcc -std=c99 builtins.c -c -o builtins.o

libbuiltins.a: builtins.o
	ar -crs libbuiltins.a builtins.o
	ranlib libbuiltins.a

test: testing.native libbuiltins.a
	./testing.native

clean:
	rm -rf *.o *.a *.ll _build *.native

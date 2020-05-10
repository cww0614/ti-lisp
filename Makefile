.PHONY: all test clean fakegc

SRC_FILES = ast.ml irgen.ml macro.ml parser.mly sast.ml scanner.mll semant.ml symtable.ml tilisp.ml utils.ml
TEST_FILES = testing.ml
RUNTIME_OBJS = tilisp.o helper.o

all: tilisp.native testing.native libtilisp.a

fakegc: bdwgc/fakegc.a

tilisp.native: $(SRC_FILES)
	ocamlbuild -no-hygiene -package llvm tilisp.native

testing.native: $(TEST_FILES)
	ocamlbuild -no-hygiene -lib unix testing.native

%.o: %.cpp %.h
	g++ -c -o $@ $<

bdwgc/fakegc.a:
	mkdir -p bdwgc
	gcc -c fakegc.c -o bdwgc/fakegc.o
	ar -crs bdwgc/fakegc.a bdwgc/fakegc.o
	ranlib bdwgc/fakegc.a

libtilisp.a: $(RUNTIME_OBJS)
	ar -crs libtilisp.a $^
	ranlib libtilisp.a

test: testing.native libtilisp.a
	./testing.native

clean:
	rm -rf *.out *.s *.o *.a *.ll _build *.native

prune:
	rm tilisp.o
	rm helper.o
	rm libtilisp.a

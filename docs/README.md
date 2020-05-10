# ti-lisp: a variant of Scheme 

## Get started

### Environment requirements

System packages:

1. llvm-dev >= 6.0
2. cmake >= 3.10

OCaml packages from opam:

1. llvm
2. ocamlbuild

Boehm GC (or execute `make fakegc` to skip this step):

```bash
cd ti-lisp # cd to the project root
git clone https://github.com/ivmai/bdwgc
cd bdwgc
./autogen.sh
./configure
make -j 4
```

### Setup

1. Build the compiler:

```sh
make tilisp.native
```

2. Run the testcases:

```
make test
```

## Documents

### Compliation phases

1. [Lexing + Parsing](parsing.md)
2. [Macro Processing](macro.md)
3. [Semantic Analysis](semant.md)

### General modules

1. [Testing](testing.md)

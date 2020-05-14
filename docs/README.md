# TI-Lisp: a variant of Scheme

<img src="logo.png" width="100px"/>

## What is TI-Lisp?

A LISP-like language with type inference, so users will be able to utilize the power of a LISP language combined with a form of “type checking”. We believe that type inference provides the best of both dynamically typed and statically typed languages: in that programs benefit from type checking, while at the same time not needing type annotations and being naturally generic, which can only be done in more conventional languages like Java and C++ through the use of templates.

## Directory
      .
      ├── docs/ # Final Report + Documentation
      ├── tests/ # Test cases containing .tisp and .log files
      ├── bdwgc/ # Boehm garbage collector
      └── ...

## Get started

### Environment requirements

System packages:

1. llvm-dev >= 9.0
2. cmake >= 3.10

OCaml packages from opam:

1. llvm
2. ocamlbuild

Boehm GC (optional, will not have GC if skipped):

```bash
# Make sure GNU Autotools are installed before compiling
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

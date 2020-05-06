# Testing

To print AST, AST after macro expansion, SAST. Default is to print SAST.
```bash
./tilisp.native [-a|-m|-s] [file.tlsp]
```
Fib example
```bash
./tilisp.native -a tests/fib.tlsp
```

For Unit Testing, use `testing.ml`. 

1. Add two files: `testname.log` (expected output) and `testname.tisp` (source codes) to `tests` directory.
2. Add the description tuple to `testing.ml`.
3. Run `ocamlbuild -lib unix testing.native`.
4. Run `./testing.native`.

Cons [Cons [a, Cons [b, Cons [b, Nil]]], Cons [1, Nil]]

## Modules to test

### Parsing

- [x] bracket pairing
- [x] expansion in macro

### Macro

- [x] macro expansion example
- [x] reserved keywords

### Semantic analysis

- [x] undefined vairable

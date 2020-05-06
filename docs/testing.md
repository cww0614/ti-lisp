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

1. Add the `testname.output` file and `testname.tisp` source codes to `tests` directory.
2. Add the description tuple to `testing.ml`.
3. Run `ocamlbuild -lib unix testing.native`.
4. Run `./testing.native`.

Cons [Cons [a, Cons [b, Cons [b, Nil]]], Cons [1, Nil]]

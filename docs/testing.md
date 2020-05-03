# Testing

To print AST, AST after macro expansion, SAST. Default is to print SAST.
```bash
./tilisp.native [-a|-m|-s] [file.tlsp]
```
Fib example
```bash
./tilisp.native -a tests/fib.tlsp
```

For Unit Testing, use `testing.ml`. This creates 2 files in /tests/ directory: the expected output `test.output`
and generated output `test.gen`.
```bash
ocaml testing.ml
```


Cons [Cons [a, Cons [b, Cons [b, Nil]]], Cons [1, Nil]]
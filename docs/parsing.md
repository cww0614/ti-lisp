# `ast.ml`

The ti-lisp parser is basically a lisp-syntax list parser. It does not
try to understand control structure like `if`, `define`, ... Thus, we
don't need to define any keywords for the language, making the syntax
more flexible and macro processing easier.

Each statement/expression is parsed into a single list or an atom
(like a single number), represented by `expr` ADT. And the whole
program is parsed into a list of `expr` (corresponding to `%type
<Ast.expr list> program` in `parser.mly`)

The `expr` ADT consists of several groups of clauses:

- identifier: the `Id` clause
- quote: the `Quote` clause
- literals: `CharLit`, `StrLit` and `IntLit`
- list constructs: including `Cons` and `Nil`
- expansion: the `...` syntax used in macro declarations

With this `expr` definition, function invocations and control
structures are represented by nested `Cons` and `Nil`, lists are
represented by `Quote [Cons ...]`, symbols represented by `Quote [Id
symbol-name]`, and rest are represented by the corresponding clauses
in the ADT.

The reason to use `Cons` and `Nil` to represent lists (instead of
using more readable OCaml list) is primarily to uniform the
representation of "cons" and "list" in lisp:

```scheme
;; cons
(cons 'a 'b) == '(a . b) 
;; list is stored as nested cons
'(a b) == '(a . (b . ()))
;; This weird thing is also valid
'(a b . c) == '(a . (b . c)) == (cons 'a '(b . c))
```

# Examples

## Simple List

A list `(a b c)` is parsed into:

```
Cons [a, Cons [b, Cons [c, Nil]]]
```

Recall that in OCaml, lists could also be represented in this "cons"
way:

```ocaml
[1; 2; 3] == 1 :: 2 :: 3 :: []
```

The difference here is that instead of using the infix operator `::`,
we use `Cons` like an prefix operator with two arguments. And an empty
list is represented by `Nil`

## Nested List

We use the hello world example from the language proposal here:

```scheme
(define (hello-world) "Hello World!")
(hello-world) 
```

The two statements are parsed into two lists:

```
Cons [define, Cons [Cons [hello-world, Nil], Cons ["Hello World!", Nil]]]

Cons [hello-world, Nil]
```

Note that we can identify a nested list by looking for two consecutive
`Cons`, like the `(hello-world)` in the example.

## Fib

The fib example from language proposal:

```scheme
(define (fib n)
  (cond
   ((= n 0) 0)
   ((= n 1) 1)
   (else (+ (fib (- n 1)) (fib (- n 2))))))
```

is parsed into

```
Cons [define, Cons [Cons [fib, Cons [n, Nil]],
Cons [Cons [cond,
Cons [
    Cons [Cons [=, Cons [n, Cons [0, Nil]]],
    Cons [0, Nil]],
Cons [
    Cons [Cons [=, Cons [n, Cons [1, Nil]]],
    Cons [1, Nil]],
Cons [
    Cons [else,
    Cons [Cons [+,
        Cons [Cons [fib, Cons [Cons [-, Cons [n, Cons [1, Nil]]], Nil]],
        Cons [Cons [fib, Cons [Cons [-, Cons [n, Cons [2, Nil]]], Nil]],
    Nil]]], Nil]],
Nil]]]], Nil]]] 
```

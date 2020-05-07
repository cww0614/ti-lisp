# SAST

In the semantic analysis pass, with the help of the symbol table, we
can finally distinguish between language structures and function
invocations (like a `begin` block and a local variable `begin`).

Therefore, the SAST could be much more meaningful now:

- The whole program is represented by a list of statements (`stmt`)
- Statements include `set!` and `define`, and a fallback to
  expressions (`expr`)
- Literals, identifiers, symbols, lists, control structures all have
  their own clauses in `expr` ADT. `Cons` and `Nil` can no longer
  store code, they only represent semantic lists and cons now.

The reason that `set!` and `define` are statements while others are
expressions is primarily that they will always introduce side effects,
and as a result symbol table will probably be mutated. Therefore when
dealing with statements in semantic checking, we need to take care of
the update symbol tables, while for expressions, we can safely assume
that symbol tables will keep unchanged.

The set of `string_of_*` functions in `sast.ml` are used to get the
string representation of SAST. The string representation has almost
the same form as lisp code with some modifications to reduce
ambiguity, like use `(funcall function args)` to differentiate
function invocations from other control structures.

# Semantic Checking

A set of mutually recursive function are implemented to check semantic
meanings of an AST and return the corresponding SAST. They all accept
a symbol table and an AST expr, but differ in return types.

- `check_stmt_block`: use this when a result of type `stmt list` is
  needed, like the body of `let`, `begin` blocks
- `check_expr_list`: use this when a result of type `expr list` is
  needed, for now it is only used to convert function arguments
- `check_stmt`: use this when a result of type `stmt` is needed
- `check_expr`: use this when a result of type `expr` is needed

Each of these four functions will try to determine the semantic
meaning of the AST node from its structure and symbol table, and call
each other recursively when needed.

Semantic checking is then implemented by calling `check_stmt`
sequentially to each expression in the list of expressions passed to
`check`.

Like in macro processing, builtin variables/functions also make the
top level symbol table in semantic analysis. Only the declaration of
their types is needed here in semantic checking. The implementation of
them should be put in the IR generating pass.

## Type Checking

Ti-lisp is a dynamic type language, so I think type checking should be
totally optional in the compiler. But I still implemented very limited
type checking for cases where types can be determined at compile time.

Only four types are defined:

- function and value: we only differentiate between callable and not
  callable objects. Callable objects have `function<return type, min
  arg number max arg number>` as their type signature, while
  non-callable objects simply have type `value`.
- any: when types cannot be determined at compile time, like arguments
  in `lambda`, variables will be assigned `any` type, and type
  checking is totally ignored for `any` type variables.

Type checking is integrated as part of four `check_xxx` functions,
they all do type checking for expression and have a `value_type` in
their return values representing the type of the expression.

Types are not store into the SAST, however, because I suppose we won't
need them in IR generating. We will have to insert dynamic type
checking code when generating IR no matter what type we could infer
for the expression in semantic analysis.

# Examples

## Hello World

```scheme
(define (hello-world) "Hello World!")
(hello-world) 
```

will be parsed into SAST

```scheme
(define hello-world (lambda ()
  "Hello World!"))

(funcall hello-world)
```

## Fib

```scheme
(define (fib n)
  (cond
   ((= n 0) 0)
   ((= n 1) 1)
   (else (+ (fib (- n 1)) (fib (- n 2))))))
```

will be parsed into SAST

```scheme
(define fib (lambda (n)
  (if (funcall = n 0)
    (begin
      0)
    (if (funcall = n 1)
      (begin
        1)
      (begin
        (funcall + (funcall fib (funcall - n 1)) (funcall fib (funcall - n 2))))))))
```

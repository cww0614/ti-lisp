# Macro Syntax

Take `and` macro as an example:

```scheme
(define-syntax and
  (syntax-rules
      () ;; literals, in this case no literals are specified
    ;; clause 1
    (;; pattern
     (and)
     ;; what should the pattern be expanded to
     true)
    ;; clause 2
    ((and test) test)
    ;; clause 3
    ((_ test1 test2 ...)
     (if test1 (and test2 ...) false))))
```

Macro expansion works like pattern matching: a lisp expression is
matched against patterns in the clauses, variables are bound in the
matching process, and the expression will be expanded as specified if
it matches.

`_` matches anything but doesn't bind the matched expression to a
identifier. In this case, though, it will always match `and`.

`...` indicates that the variable preceding it will match all
following expressions in the current list.

Here are some examples for the `and` macro:

```scheme
(and) => true ;; clause 1

(and 1) => 1 ;; clause 2
(and (= a 1)) => (= a 1) ;; clause 2

(and 1 2)
;; clause 3
=> (if 1 (and 2) false)
;; clause 2
=> (if 1 2 false)

(and (= a 1) (= b 2) (= c 3)) ;; `test2 ...` matches (= b 2) (= c 3)
;; clause 3
=> (if (= a 1) (and (= b 2) (= c 3)) false)
;; clause 3
=> (if (= a 1) (if (= b 2) (and (= c 3)) false) false)
;; clause 2
=> (if (= a 1) (if (= b 2) (= c 3) false) false)
```

Identifiers specified in the literals section can only be matched with
the identifier with the same name (normally identifier matches any
expression).

An example of using "literals" is the `cond` macro:

```scheme
(define-syntax cond
  (syntax-rules (else)
    ;; clause 1
    ((_ (predicate body ...) clauses ...)
     (if predicate
         (begin body ...)
         (cond clauses ...)))
    ;; clause 2
    ((_ (predicate body ...))
     (if predicate
         (begin body ...)))
    ;; clause 3
    ((_ (else body ...)) ;; else is specified in the literals section
     (begin body ...))))
```

In this example, the `else` in the pattern can only match identifier
`else`, anything other than `else` in the same position will make the
match against this clause fail.

Example of expanding this macro:

```scheme
(cond
 ((= a 1) a) 
 ((= b 1) b))
;; clause 1
=> (if (= a 1)
       (begin a)
       (cond ((= b 1) b)))
;; clause 2
=> (if (= a 1)
       (begin a)
       (if (= b 1) b))


(cond
 ((= a 1) a) 
 ((= b 1) b)
 (else -1))
;; clause 1
=> (if (= a 1)
       (begin a)
       (cond ((= b 1) b) (else -1)))
;; clause 1
=> (if (= a 1)
       (begin a)
       (if (= b 1)
           (begin b)
           (cond (else -1))))
;; clause 3
=> (if (= a 1)
       (begin a)
       (if (= b 1)
           (begin b)
           (begin -1)))
```

# Implementation

The function that does the most basic macro expansion is `expand1`. It
accepts a symbol table and an lisp expression, then:

- If the expression is a `define-syntax` form (macro definition), it
  add the macro definition to the symbol table, return the updated
  symbol and `None`. The `None` here indicates that the
  `define-syntax` form is stripped from AST.
- If the expression that matches any macro in the symbol table
  (`match_rule` function), it will do expansion recursively until the
  expanded expression is no longer a macro form itself (`replace_rule`
  function) and return the expanded expression (with unchanged symbol
  table).
- Otherwise, it returns the symbol table and expression unchanged.

Now that we have `expand1`, we can deal with macro definitions and
expand macro forms like `(and 1 2)` recursively.  However, normally
macro forms are contained within other expressions like `(set! a (and
1 2))` or `(let ((a true) (b false)) (and 1 2))`. As a result,
recursive expansion is actually not complete now because we can't
further expand macro hidden under nested structures in expanded code
(example: `(and 1 2) => (if 1 (and 2) false)`, we can't further expand
the inner `and` now). We need a way to find macro forms in this nested
structure. This is why we have the `expand_over_list` helper function.

`expand_over_list` first tries to expand the expression itself, in
case it is given a macro form directly. Then it tries to expand each
element in the expanded list (assume we are dealing with a list here,
for other cases, `expand_over_list` can simply return the expression
unchanged) (note that we are expanding elements in a expanded list,
this is where full recursive expansion is enabled). If an element
of the list is of form `Cons [Cons something, ...]`, it means that we
find a nested list (for example, the `(and 1 2)` in `(set! a (and 1
2))`), and we apply `expand_over_list` recursively to this inner
list. This allows us to find macro forms in a nested list recursively.

Finally, since `expand` (a simple rename of `expand_over_list`) only
works with one `expr` while our program is represented by `expr list`,
we create a simple wrapper `expand_all`, which applies `expand`
sequentially to the list of `expr` and return the expanded list of
expressions. The initial symbol table in `expand_all` is made with the
builtin macros.

# Builtin Macros

For now, builtin macros include:

- `cond`: the `cond` structure, expanded into nested `if`
- `let*`: the `let*` structure, expanded into nested `let`
- `and`, `or`: implemented by macros instead of normal functions to
  support short-circuit evaluation.
- `define`: extend the basic `define` (`(define name value)`) to
  support function declaration.

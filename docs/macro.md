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

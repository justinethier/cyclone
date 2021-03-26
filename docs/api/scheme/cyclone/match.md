---
layout: main
title: API
---

---
layout: main
title: API
---

---
layout: main
title: API
---

# Match Library

The `(scheme cyclone match)` library provides a hygienic pattern matcher, based on Alex Shinn's portable `match.scm`.

This is a full superset of the popular [match](http://www.cs.indiana.edu/scheme-repository/code.match.html) package by Andrew Wright, written in fully portable `syntax-rules` and thus preserving hygiene.

The most notable extensions are the ability to use *non-linear* patterns - patterns in which the same identifier occurs multiple times, tail patterns after ellipsis, and the experimental tree patterns.

## Index

- [`match`](#match)
- [`match-lambda`](#match-lambda)
- [`match-lambda*`](#match-lambda-1)
- [`match-let`](#match-let)
- [`match-letrec`](#match-letrec)
- [`match-let*`](#match-let-1)

## Patterns

Patterns are written to look like the printed representation of the objects they match.  The basic usage is

    (match expr (pat body ...) ...)

where the result of `expr` is matched against each pattern in
turn, and the corresponding body is evaluated for the first to
succeed.  Thus, a list of three elements matches a list of three
elements.

    (let ((ls (list 1 2 3))) (match ls ((1 2 3) #t)))

If no patterns match an error is signalled.

Identifiers will match anything, and make the corresponding
binding available in the body.

    (match (list 1 2 3) ((a b c) b))

If the same identifier occurs multiple times, the first instance
will match anything, but subsequent instances must match a value
which is `equal?` to the first.

    (match (list 1 2 1) ((a a b) 1) ((a b a) 2))

The special identifier `_` matches anything, no matter how
many times it is used, and does not bind the result in the body.

    (match (list 1 2 1) ((_ _ b) 1) ((a b a) 2))

To match a literal identifier (or list or any other literal), use `quote`.

    (match 'a ('b 1) ('a 2))

Analogous to its normal usage in scheme, `quasiquote` can
be used to quote a mostly literally matching object with selected
parts unquoted.

    (match (list 1 2 3) (`(1 ,b ,c) (list b c)))

Often you want to match any number of a repeated pattern.  Inside
a list pattern you can append `...` after an element to
match zero or more of that pattern (like a regexp Kleene star).

    (match (list 1 2) ((1 2 3 ...) #t))
    (match (list 1 2 3) ((1 2 3 ...) #t))
    (match (list 1 2 3 3 3) ((1 2 3 ...) #t))

Pattern variables matched inside the repeated pattern are bound to
a list of each matching instance in the body.

    (match (list 1 2) ((a b c ...) c))
    (match (list 1 2 3) ((a b c ...) c))
    (match (list 1 2 3 4 5) ((a b c ...) c))

More than one `...` may not be used in the same list, since
this would require exponential backtracking in the general case.
However, `...` need not be the final element in the list,
and may be succeeded by a fixed number of patterns.

    (match (list 1 2 3 4) ((a b c ... d e) c))
    (match (list 1 2 3 4 5) ((a b c ... d e) c))
    (match (list 1 2 3 4 5 6 7) ((a b c ... d e) c))

`___` is provided as an alias for `...` when it is
inconvenient to use the ellipsis (as in a syntax-rules template).

The `..1` syntax is exactly like the `...` except
that it matches one or more repetitions (like a regexp "+").

    (match (list 1 2) ((a b c ..1) c))
    (match (list 1 2 3) ((a b c ..1) c))

The boolean operators `and`, `or`, and `not`
can be used to group and negate patterns analogously to their
Scheme counterparts.

The `and` operator ensures that all subpatterns match.
This operator is often used with the idiom `(and x pat)` to
bind `x` to the entire value that matches `pat`
(c.f. "as-patterns" in ML or Haskell).  Another common use is in
conjunction with `not` patterns to match a general case
with certain exceptions.

    (match 1 ((and) #t))
    (match 1 ((and x) x))
    (match 1 ((and x 1) x))

The `or` operator ensures that at least one subpattern
matches.  If the same identifier occurs in different subpatterns,
it is matched independently.  All identifiers from all subpatterns
are bound if the `or` operator matches, but the binding is
only defined for identifiers from the subpattern which matched.

    (match 1 ((or) #t) (else #f))
    (match 1 ((or x) x))
    (match 1 ((or x 2) x))

The `not` operator succeeds if the given pattern doesn't
match.  None of the identifiers used are available in the body.

    (match 1 ((not 2) #t))

The more general operator `?` can be used to provide a
predicate.  The usage is `(? predicate pat ...)` where
`predicate` is a Scheme expression evaluating to a predicate
called on the value to match, and any optional patterns after the
predicate are then matched as in an `and` pattern.

    (match 1 ((? odd? x) x))

The field operator `=` is used to extract an arbitrary
field and match against it.  It is useful for more complex or
conditional destructuring that can't be more directly expressed in
the pattern syntax.  The usage is `(= field pat)`, where
`field` can be any expression, and should result in a
procedure of one argument, which is applied to the value to match
to generate a new value to match against `pat`.

Thus the pattern `(and (= car x) (= cdr y))` is equivalent
to `(x . y)`, except it will result in an immediate error
if the value isn't a pair.

    (match '(1 . 2) ((= car x) x))
    (match 4 ((= square x) x))

The record operator `$` is used as a concise way to match
records defined by SRFI-9 (or SRFI-99).  The usage is
`($ rtd field ...)`, where `rtd` should be the record
type descriptor specified as the first argument to
`define-record-type`, and each `field` is a subpattern
matched against the fields of the record in order.  Not all fields
must be present.

    (let ()
      (define-record-type employee
        (make-employee name title)
        employee?
        (name get-name)
        (title get-title))
      (match (make-employee "Bob" "Doctor")
        (($ employee n t) (list t n))))

For records with more fields it can be helpful to match them by
name rather than position.  For this you can use the `@`
operator, originally a Gauche extension:

    (let ()
      (define-record-type employee
        (make-employee name title)
        employee?
        (name get-name)
        (title get-title))
      (match (make-employee "Bob" "Doctor")
        ((@ employee (title t) (name n)) (list t n))))

The `set!` and `get!` operators are used to bind an
identifier to the setter and getter of a field, respectively.  The
setter is a procedure of one argument, which mutates the field to
that argument.  The getter is a procedure of no arguments which
returns the current value of the field.

    (let ((x (cons 1 2))) (match x ((1 . (set! s)) (s 3) x)))
    (match '(1 . 2) ((1 . (get! g)) (g)))

The new operator `***` can be used to search a tree for
subpatterns.  A pattern of the form `(x *** y)` represents
the subpattern `y` located somewhere in a tree where the path
from the current object to `y` can be seen as a list of the
form `(x ...)`.  `y` can immediately match the current
object in which case the path is the empty list.  In a sense it's
a 2-dimensional version of the `...` pattern.

As a common case the pattern `(_ *** y)` can be used to
search for `y` anywhere in a tree, regardless of the path
used.

    (match '(a (a (a b))) ((x *** 'b) x))
    (match '(a (b) (c (d e) (f g))) ((x *** 'g) x))

## Notes

The implementation is a simple generative pattern matcher - each
pattern is expanded into the required tests, calling a failure
continuation if the tests fail.  This makes the logic easy to
follow and extend, but produces sub-optimal code in cases where you
have many similar clauses due to repeating the same tests.
Nonetheless a smart compiler should be able to remove the redundant
tests.  For MATCH-LET and DESTRUCTURING-BIND type uses there is no
performance hit.

The original version was written on 2006/11/29 and described in the
following Usenet post: http://groups.google.com/group/comp.lang.scheme/msg/0941234de7112ffd

and is still available at: http://synthcode.com/scheme/match-simple.scm

It's just 80 lines for the core MATCH, and an extra 40 lines for
MATCH-LET, MATCH-LAMBDA and other syntactic sugar.

# match

*Syntax*

    (match {expression} {clauses})

where `{clauses}` has the form:

    (pattern body ...)

This is the primary pattern match macro. See the [Patterns](#patterns) section above for complete details.

# match-lambda

*Syntax*

    (match-lambda (pattern body ...))

Shortcut for `lambda` + `match`.  Creates a procedure of one argument, and matches that argument against each clause.

# match-lambda*

*Syntax*

    (match-lambda* (pattern body ...))

Similar to `match-lambda`.  Creates a procedure of any number of arguments, and matches the argument list against each clause.

# match-let

*Syntax*

    (match-let ((var value) ...) body ...)
    (match-let loop ((var value) ...) body ...)

Matches each var to the corresponding expression, and evaluates the body with all match variables in scope.  Raises an error if any of the expressions fail to match.  Syntax analogous to named let can also be used for recursive functions which match on their arguments as in `match-lambda*`.

# match-letrec

*Syntax*

    (match-letrec ((var value) ...) body ...)

Similar to `match-let`, but analogously to `letrec` matches and binds the variables with all match variables in scope.

# match-let*

*Syntax*

    (match-let* ((var value) ...) body ...)

Similar to `match-let`, but analogously to `let*` matches and binds the variables in sequence, with preceding match variables in scope.

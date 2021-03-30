# Base Library

The `(scheme base)` library exports many of the procedures and syntax bindings that are traditionally associated with Scheme.

For more information see the [R<sup>7</sup>RS Scheme Specification](../../r7rs.pdf).

- [`abs`](#abs)
- [`and`](#and)
- [`any`](#any)
- [`append`](#append)
- [`assoc`](#assoc)
- [`assq`](#assq)
- [`assv`](#assv)
- [`begin`](#begin)
- [`boolean=?`](#boolean)
- [`bytevector-copy`](#bytevector-copy)
- [`bytevector-copy!`](#bytevector-copy-1)
- [`call-with-current-continuation`](#call-with-current-continuation)
- [`call-with-port`](#call-with-port)
- [`call-with-values`](#call-with-values)
- [`call/cc`](#callcc)
- [`case`](#case)
- [`ceiling`](#ceiling)
- [`char<=?`](#char)
- [`char<?`](#char-1)
- [`char=?`](#char-2)
- [`char>=?`](#char-3)
- [`char>?`](#char-4)
- [`complex?`](#complex)
- [`cond`](#cond)
- [`cond-expand`](#cond-expand)
- [`current-error-port`](#current-error-port)
- [`current-input-port`](#current-input-port)
- [`current-output-port`](#current-output-port)
- [`define-record-type`](#define-record-type)
- [`denominator`](#denominator)
- [`do`](#do)
- [`dynamic-wind`](#dynamic-wind)
- [`eof-object`](#eof-object)
- [`error`](#error)
- [`even?`](#even)
- [`every`](#every)
- [`exact`](#exact)
- [`exact-integer?`](#exact-integer)
- [`exact?`](#exact)
- [`expt`](#expt)
- [`features`](#features)
- [`floor`](#floor)
- [`floor-quotient`](#floor-quotient)
- [`floor-remainder`](#floor-remainder)
- [`floor-remainder`](#floor-remainder )
- [`floor/`](#floor-1)
- [`flush-output-port`](#flush-output-port)
- [`foldl`](#foldl)
- [`foldr`](#foldr)
- [`for-each`](#for-each)
- [`gcd`](#gcd)
- [`get-output-bytevector`](#get-output-bytevector)
- [`get-output-string`](#get-output-string)
- [`guard`](#guard)
- [`inexact`](#inexact)
- [`inexact?`](#inexact-1)
- [`input-port-open?`](#input-port-open)
- [`input-port?`](#input-port)
- [`lcm`](#lcm)
- [`let`](#let)
- [`let*`](#let-1)
- [`let*-values`](#let-values)
- [`let-values`](#let-values-1)
- [`letrec`](#letrec)
- [`letrec*`](#letrec-1)
- [`list`](#list)
- [`list-copy`](#list-copy)
- [`list-ref`](#list-ref)
- [`list-set!`](#list-set)
- [`list-tail`](#list-tail)
- [`list?`](#list-1)
- [`make-constructor`](#make-constructor)
- [`make-getter`](#make-getter)
- [`make-list`](#make-list)
- [`make-parameter`](#make-parameter)
- [`make-setter`](#make-setter)
- [`make-string`](#make-string)
- [`make-type-predicate`](#make-type-predicate)
- [`map`](#map)
- [`max`](#max)
- [`member`](#member)
- [`memq`](#memq)
- [`memv`](#memv)
- [`min`](#min)
- [`modulo`](#modulo)
- [`negative?`](#negative)
- [`newline`](#newline)
- [`not`](#not)
- [`numerator`](#numerator)
- [`odd?`](#odd)
- [`open-input-bytevector`](#open-input-bytevector)
- [`open-input-string`](#open-input-string)
- [`open-output-bytevector`](#open-output-bytevector)
- [`open-output-string`](#open-output-string)
- [`or`](#or)
- [`output-port-open?`](#output-port-open)
- [`output-port?`](#output-port)
- [`parameterize`](#parameterize)
- [`positive?`](#positive)
- [`quasiquote`](#quasiquote)
- [`quotient`](#quotient)
- [`raise`](#raise)
- [`raise-continuable`](#raise-continuable)
- [`rational?`](#rational)
- [`read-line`](#read-line)
- [`read-string`](#read-string)
- [`receive`](#receive)
- [`record?`](#record)
- [`remainder`](#remainder)
- [`reverse`](#reverse)
- [`round`](#round)
- [`slot-set!`](#slot-set)
- [`square`](#square)
- [`string`](#string)
- [`string->list`](#string-list)
- [`string->utf8`](#string-utf8)
- [`string->vector`](#string-vector)
- [`string-copy`](#string-copy)
- [`string-copy!`](#string-copy-1)
- [`string-fill!`](#string-fill)
- [`string-for-each`](#string-for-each)
- [`string-map`](#string-map)
- [`string<=?`](#string-1)
- [`string<?`](#string-2)
- [`string=?`](#string-3)
- [`string>=?`](#string-4)
- [`string>?`](#string-5)
- [`symbol=?`](#symbol)
- [`syntax-error`](#syntax-error)
- [`truncate`](#truncate)
- [`truncate-quotient`](#truncate-quotient)
- [`truncate-remainder`](#truncate-remainder)
- [`truncate/`](#truncate-1)
- [`type-slot-offset`](#type-slot-offset)
- [`unless`](#unless)
- [`utf8->string`](#utf8-string)
- [`values`](#values)
- [`vector`](#vector)
- [`vector->list`](#vector-list)
- [`vector->string`](#vector-string)
- [`vector-append`](#vector-append)
- [`vector-copy`](#vector-copy)
- [`vector-copy!`](#vector-copy-1)
- [`vector-fill!`](#vector-fill)
- [`vector-for-each`](#vector-for-each)
- [`vector-map`](#vector-map)
- [`when`](#when)
- [`with-exception-handler`](#with-exception-handler)
- [`with-handler`](#with-handler)
- [`write-char`](#write-char)
- [`write-string`](#write-string)
- [`zero?`](#zero)


# abs

    (abs num)

Return the absolute value of `num`.

# and

*Syntax*

    (and {test1} ...)

Semantics: The `{test}` expressions are evaluated from left to right, and if any expression evaluates to `#f`, then `#f` is returned. Any remaining expressions are not evaluated. If all the expressions evaluate to true values, the values of the last expression are returned. If there are no expressions, then `#t` is returned.

    (and (= 2 2) (> 2 1)) => #t
    (and (= 2 2) (< 2 1)) => #f
    (and 1 2 '(f g)) => (f g)
    (and) => #t

# any

    (any pred lst)

Return `#t` if predicate function `pred` is true for any value of `lst`. Otherwise `#f` is returned.

# append

    (append list ...)

The last argument, if there is one, can be of any type.

Returns a list consisting of the elements of the first list
followed by the elements of the other lists. If there are no
arguments, the empty list is returned. If there is exactly
one argument, it is returned. Otherwise the resulting list
is always newly allocated, except that it shares structure
with the last argument. An improper list results if the last
argument is not a proper list.

    (append '(x) '(y)) => (x y)
    (append '(a) '(b c d)) => (a b c d)
    (append '(a (b)) '((c))) => (a (b) (c))
    (append '(a b) '(c . d)) => (a b c . d)
    (append '() 'a) => a

# assoc

    (assoc obj alist)

    (assoc obj alist compare)

It is an error if alist (for "association list") is not a list of pairs.

This procedure finds the first pair in `alist` whose car field
is `obj`, and returns that pair. If no pair in `alist` has `obj`
as its car, then `#f` (not the empty list) is returned. 

`assoc` uses `compare` to compare `obj` with the car fields
of the pairs in `alist` if given and `equal?` otherwise.

# assq

    (assq obj alist)

The `assq` procedure is the same as `assoc` except it uses `eq?` to compare `obj` with the car fields of the pairs in `alist`.

# assv

    (assv obj alist)

The `assv` procedure is the same as `assoc` except it uses `eqv?` to compare `obj` with the car fields of the pairs in `alist`.

# begin

*Syntax*

    (begin {expression or definition} ...)

This form of `begin` can appear as part of a `{body}`, or at the outermost level of a `{program}`, or at the REPL, or directly nested in a begin that is itself of this form.  It causes the contained expressions and definitions to be evaluated exactly as if the enclosing begin construct were not present.

Rationale: This form is commonly used in the output of macros which need to generate multiple definitions and splice them into the context in which they are expanded.

    (begin {expression1} {expression2} ...)

This form of `begin` can be used as an ordinary expression.  The `{expression}`'s are evaluated sequentially from left to right, and the values of the last `{expression}` are returned.  This expression type is used to sequence side effects such as assignments or input and output.

    (define x 0)

    (and (= x 0)
         (begin (set! x 5)
                (+ x 1))) => 6

    (begin (display "4 plus 1 equals ")
           (display (+ 4 1))) => unspecified and prints 4 plus 1 equals 5

# boolean=?

    (boolean=? b1 b2 ...)

Returns `#t` if all the arguments are booleans and all are `#t` or all are `#f`.

# bytevector-copy

    (bytevector-copy bytevector)

    (bytevector-copy bytevector start)

    (bytevector-copy bytevector start end)

Returns a newly allocated bytevector containing the bytes in `bytevector` between `start` and `end`.

    (define a #u8(1 2 3 4 5))
    (bytevector-copy a 2 4)) => #u8(3 4)

# bytevector-copy!

    (bytevector-copy! to at from)

    (bytevector-copy! to at from start)

    (bytevector-copy! to at from start end)

Copies the bytes of `bytevector` from between `start` and `end` to bytevector `to`, starting at `at`.  

    (define a (bytevector 1 2 3 4 5))
    (define b (bytevector 10 20 30 40 50))
    (bytevector-copy! b 1 a 0 2)
    b => #u8(10 1 2 40 50)

# call-with-current-continuation

    (call-with-current-continuation proc)

# call-with-port

    (call-with-port port proc)

It is an error if `proc` does not accept one argument.

The `call-with-port` procedure calls `proc` with `port` as an argument. If `proc` returns, then the `port` is closed automatically and the values yielded by the `proc` are returned.

# call-with-values

    (call-with-values producer consumer)

Calls its `producer` argument with no arguments and a
continuation that, when passed some values, calls the
`consumer` procedure with those values as arguments. The
continuation for the call to consumer is the continuation
of the call to `call-with-values`.

    (call-with-values (lambda () (values 4 5))
    (lambda (a b) b))
    => 5
    (call-with-values * -) => -1

# call/cc

    (call/cc proc)

An abbreviation for `call-with-current-continuation`.

# case

*Syntax*

    (case {key} {clause1} {clause2} ...)

Syntax: `{Key}` can be any expression. Each `{clause}` has the form

    (({datum1} ...) {expression1} {expression2} ...),

where each `{datum}` is an external representation of some object. It is an error if any of the `{datum}`'s are the same anywhere in the expression. Alternatively, a `{clause}` can be of the form

    ((hdatum1} ...) => {expression})

The last `{clause}` can be an "else clause," which has one of the forms

    (else {expression1} {expression2} . . . )

or

    (else => {expression}).

Semantics: A `case` expression is evaluated as follows.  `{Key}` is evaluated and its result is compared against each `{datum}`. If the result of evaluating `{key}` is the same to a `{datum}`, then the expressions in the corresponding `{clause}` are evaluated in order and the results of the last expression in the `{clause}` are returned as the results of the case expression.

If the result of evaluating `{key}` is different from every `{datum}`, then if there is an else clause, its expressions are evaluated and the results of the last are the results of the case expression; otherwise the result of the case expression is unspecified.

If the selected `{clause}` or else clause uses the `=>` alternate form, then the `{expression}` is evaluated. It is an error if its value is not a procedure accepting one argument. This procedure is then called on the value of the `{key}` and the values returned by this procedure are returned by the case expression.

    (case (* 2 3)
      ((2 3 5 7) 'prime)
      ((1 4 6 8 9) 'composite)) => composite
    (case (car '(c d))
      ((a) 'a)
      ((b) 'b)) => unspecified
    (case (car '(c d))
      ((a e i o u) 'vowel)
      ((w y) 'semivowel)
      (else => (lambda (x) x))) => c

# ceiling

    (ceiling z)

Returns the smallest integer not smaller than `z`.

# char<=?

    (char<=? c1 c2 c3 ...)

Return `#t` if the results of passing the arguments to `char->integer` are monotonically increasing or equal.

# char<?

    (char<?  c1 c2 c3 ...)

Return `#t` if the results of passing the arguments to `char->integer` are respectively equal, monotonically increasing.

# char=?

    (char=?  c1 c2 c3 ...)

Return `#t` if the results of passing the arguments to `char->integer` are equal.

# char>=?

    (char>=? c1 c2 c3 ...)

Return `#t` if the results of passing the arguments to `char->integer` are monotonically decreasing or equal.

# char>?

    (char>?  c1 c2 c3 ...)

Return `#t` if the results of passing the arguments to `char->integer` are monotonically decreasing.

# complex?

    (complex? obj) 

Return `#t` if `obj` is a complex number, `#f` otherwise.

# cond

*Syntax*

    (cond {clause1} {clause2} ...)
    else
    =>

Syntax: `{Clauses}` take one of two forms, either

    ({test} {expression1} ...)

where `{test}` is any expression, or

    ({test} => {expression})

The last `{clause}` can be an "else clause," which has the
form

    (else {expression1} {expression2} ...)

Semantics: A `cond` expression is evaluated by evaluating
the `{test}` expressions of successive `{clause}`'s in order until
one of them evaluates to a true value.
When a `{test}` evaluates to a true value, the remaining
 `{expression}`'s in its `{clause}` are evaluated in order, and the
results of the last `{expression}` in the `{clause}` are returned
as the results of the entire cond expression.

If the selected `{clause}` contains only the `{test}` and no
 `{expression}`'s, then the value of the `{test}` is returned as
the result. If the selected `{clause}` uses the `=>` alternate
form, then the `{expression}` is evaluated. It is an error if
its value is not a procedure that accepts one argument.
This procedure is then called on the value of the `{test}` and
the values returned by this procedure are returned by the
cond expression.

If all `{test}`'s evaluate to `#f`, and there is no else clause,
then the result of the conditional expression is unspecified;
if there is an else clause, then its `{expression}`'s are evaluated
  in order, and the values of the last one are returned.

    (cond ((> 3 2) 'greater)
          ((< 3 2) 'less)) => greater
    (cond ((> 3 3) 'greater)
          ((< 3 3) 'less)
          (else 'equal)) => equal
    (cond ((assv 'b '((a 1) (b 2))) => cadr)
          (else #f)) => 2

# cond-expand

*Syntax*

    (cond-expand {ce-clause2} {ce-clause2} ...)

Syntax: The `cond-expand` expression type provides a way to statically expand different expressions depending on the implementation. A `{ce-clause}` takes the following form:

    ({feature requirement} {expression} ...)

The last clause can be an "else clause," which has the form

    (else {expression} ...)

A `{feature requirement}` takes one of the following forms:

- `{feature identifier}`
- `(library {library name})`
- `(and {feature requirement} ...)`
- `(or {feature requirement} ...)`
- `(not {feature requirement})`

Semantics: Each Scheme implementation maintains a list of feature identifiers which are present, as well as a list of libraries which can be imported. The value of a `{feature requirement}` is determined by replacing each `{feature identifier}` and `(library {library name})` on the implementation’s lists with `#t`, and all other feature identifiers and library names with `#f`, then evaluating the resulting expression as a Scheme boolean expression under the normal interpretation of `and`, `or`, and `not`.

A `cond-expand` is then expanded by evaluating the `{feature requirement}`'s of successive `{ce-clause}`'s in order until one of them returns `#t`. When a true clause is found, the corresponding `{expression}`'s are expanded to a begin, and the remaining clauses are ignored. If none of the `{feature requirement}`'s evaluate to `#t`, then if there is an else clause, its `{expression}`'s are included. Otherwise, the behavior of the cond-expand is unspecified. Unlike cond, `cond-expand` does not depend on the value of any variables.

# current-error-port

    (current-error-port)

Returns the current error port (an output port).

# current-input-port

    (current-input-port)

Return the current input port.

# current-output-port

    (current-output-port)

Return the current output port.

# define-record-type

*Syntax*

    (define-record-type {name}

      {constructor} {pred} {field} ...)

Create a new record type.

Record-type definitions are used to introduce new data
types, called record types. Like other definitions, they can
appear either at the outermost level or in a body. The values of a record type are called records and are aggregations
of zero or more fields, each of which holds a single location. A predicate, a constructor, and field accessors and
mutators are defined for each record type.

# denominator

    (denominator n)

Return the denominator of `n`.

# do

*Syntax*

    (do (({variable1} {init1} {step1})

         ...)

    ({test} {expression} ...)

    {command} ...)

# dynamic-wind

    (dynamic-wind before thunk after)

Calls `thunk` without arguments, returning the result(s) of this call.

`before` is called whenever execution enters the dynamic extent of the call to `thunk` and `after` is called whenever it exits that dynamic extent.

# eof-object

    (eof-object)

Return the end of file (EOF) object.

# error

    (error message obj ...)

Raise an error with message `message` and one or more associated objects `obj`.

# even?

    (even? num)

Return `#t` if `num` is even and `#f` if it is not. It is an error if `num` is not a number.

# every

    (every pred lst)

Return `#t` if predicate function `pred` is true for every value of `lst`. Otherwise `#f` is returned.

# exact

    (exact num)

Return an exact representation of number `num`.

# exact-integer?

    (exact-integer? num)

Returns `#t` if `num` is both exact and an integer; otherwise returns `#f`.

# exact?

    (exact? num)

Return `#t` if `num` is exact.

# expt

    (expt z1 z2)

# features

    (features) 

Return a list of feature identifiers which `cond-expand` treats as true.

# floor

    (floor z)

Return an integer not larger than `z`.

# floor-quotient 

    (floor-quotient n m)

Returns the integer quotient of dividing `n` by `m`.

# floor-remainder

    (floor-remainder n m)

Returns the integer remainder of dividing `n` by `m`.

# floor/ 

    (floor/ n m)

Return integer quotient and remainder of dividing `n` by `m`.

# flush-output-port

    (flush-output-port)

    (flush-output-port port)

Flushes any buffered output from the buffer of `output-port`
to the underlying file or device and returns an unspecified
value.

# foldl

    (foldl func accum lst)

Perform a left fold.

# foldr

    (foldr func end lst)

# for-each

    (for-each proc list1 list2 ...)

It is an error if `proc` does not accept as many arguments as there are lists.

The arguments to `for-each` are like the arguments to `map`,
but `for-each` calls `proc` for its side effects rather than for
its values. Unlike `map`, `for-each` is guaranteed to call `proc`
on the elements of the lists in order from the first element(s) to the last, and the value returned by `for-each`
is unspecified. If more than one list is given and not all
lists have the same length, for-each terminates when the
shortest list runs out. The lists can be circular, but it is
an error if all of them are circular.

    (let ((v (make-vector 5)))
      (for-each (lambda (i)
                  (vector-set! v i (* i i)))
                ’(0 1 2 3 4))
      v) => #(0 1 4 9 16)

# gcd

    (gcd n1 ...)

Return the greatest commong divisor of the arguments.

# get-output-bytevector

    (get-output-bytevector port)

Returns a bytevector consisting of the bytes that have been output to the `port` so far in the order they were output.

# get-output-string

    (get-output-string port)

Returns a string consisting of the characters that have been output to the `port` so far in the order they were output. If the result string is modified, the effect is unspecified.

    (parameterize
      ((current-output-port
       (open-output-string)))
      (display "piece")
      (display " by piece ")
      (display "by piece.")
      (newline)
      (get-output-string (current-output-port)))

      => "piece by piece by piece.\n"

# guard

*Syntax*

    (guard ({variable}

            {cond clause1} {cond clause2} ...)

      {body})

# inexact

    (inexact z)

Return `z` as an inexact number.

# inexact?

    (inexact? num)

Return `#t` if `num` is inexact and `#f` otherwise.

# input-port-open?

    (input-port-open? port)

Return `#t` if the given input port is open and `#f` otherwise.

# input-port?

    (input-port? port)

Return `#t` if `port` is an input port and `#f` otherwise.

# lcm

    (lcm n1 ...)

Return the least common multiple of the arguments.

# let

*Syntax*

    (let {bindings} {body})

`{bindings}` has the form:

    (({variable1} {init1}) ...)

where each `{init}` is an expression, and `{body}` is a sequence
of zero or more definitions followed by a sequence of one or more expressions. It is an error for a `{variable}` to appear more than once in the list of variables being bound.

Semantics: The `{init}`'s are evaluated in the current environment (in some unspecified order), the `{variable}`'s are bound to fresh locations holding the results, the `{body}` is evaluated in the extended environment, and the values of the last expression of `{body}` are returned. Each binding of a `{variable}` has `{body}` as its region.

    (let ((x 2) (y 3))
      (* x y)) => 6
    
    (let ((x 2) (y 3))
      (let ((x 7)
        (z (+ x y)))
      (* z x))) => 35

# let*

*Syntax*

    (let* {bindings} {body})

`{bindings}` has the form:

    (({variable1} {init1}) ...)

where each `{init}` is an expression, and `{body}` is a sequence
of zero or more definitions followed by a sequence of one or more expressions. It is an error for a `{variable}` to appear more than once in the list of variables being bound.

Semantics: The `let*` binding construct is similar to `let`, but the bindings are performed sequentially from left to right, and the region of a binding indicated by `({variable} {init})` is that part of the `let*` expression to the right of the binding. Thus the second binding is done in an environment in which the first binding is visible, and so on.

The `{variable}`'s need not be distinct.

    (let ((x 2) (y 3))
        (let* ((x 7)
               (z (+ x y)))
          (* z x))) => 70


# let*-values

*Syntax*

    (let*-values {mv binding spec} {body})

# let-values

*Syntax*

    (let-values {mv binding spec} {body})

# letrec

*Syntax*

    (letrec {bindings} {body})

`{bindings}` has the form:

    (({variable1} {init1}) ...)

where each `{init}` is an expression, and `{body}` is a sequence
of zero or more definitions followed by a sequence of one or more expressions. It is an error for a `{variable}` to appear more than once in the list of variables being bound.

Semantics: The `{variable}`'s are bound to fresh locations holding unspecified values, the `{init}`'s are evaluated in the resulting environment (in some unspecified order), each `{variable}` is assigned to the result of the corresponding `{init}`, the `{body}` is evaluated in the resulting environment, and the values of the last expression in `{body}` are returned.

Each binding of a `{variable}` has the entire letrec expression
as its region, making it possible to define mutually
recursive procedures.

    (letrec ((even?
              (lambda (n)
                (if (zero? n)
                    #t
                    (odd? (- n 1)))))
             (odd?
              (lambda (n)
                (if (zero? n)
                    #f
                    (even? (- n 1))))))
      (even? 88))
          => #t

One restriction on `letrec` is very important: if it is not possible to evaluate each `{init}` without assigning or referring to the value of any `{variable}`, it is an error. The restriction is necessary because letrec is defined in terms of a procedure call where a lambda expression binds the `{variable}`'s to the values of the `{init}`'s. In the most common uses of letrec, all the `{init}`'s are lambda expressions and the restriction is satisfied automatically.

# letrec*

*Syntax*

    (letrec* {bindings} {body})

`{bindings}` has the form:

    (({variable1} {init1}) ...)

where each `{init}` is an expression, and `{body}` is a sequence
of zero or more definitions followed by a sequence of one or more expressions. It is an error for a `{variable}` to appear more than once in the list of variables being bound.

Semantics: The `{variable}`'s are bound to fresh locations, each `{variable}` is assigned in left-to-right order to the result of evaluating the corresponding `{init}`, the `{body}` is evaluated in the resulting environment, and the values of the last expression in `{body}` are returned. Despite the left-to-right evaluation and assignment order, each binding of a `{variable}` has the entire `letrec*` expression as its region, making it possible to define mutually recursive procedures.

If it is not possible to evaluate each `{init}` without assigning or referring to the value of the corresponding `{variable}` or the `{variable}` of any of the bindings that follow it in `{bindings}`, it is an error. Another restriction is that it is an error to invoke the continuation of an `{init}` more than once.

    (letrec* ((p
                (lambda (x)
                  (+ 1 (q (- x 1)))))
              (q
                (lambda (y)
                  (if (zero? y)
                      0
                      (+ 1 (p (- y 1))))))
              (x (p 5))
              (y x))
      y)
        => 5

# list

    (list obj ...)

Return a newly allocated list of its arguments.

# list-copy

    (list-copy lst)

Returns a newly allocated copy of the given `obj` if it is a
list. Only the pairs themselves are copied; the cars of the
result are the same (in the sense of `eqv?`) as the cars of list.

If `obj` is an improper list, so is the result, and the final cdrs
are the same in the sense of `eqv?`. An obj which is not a
list is returned unchanged. 

It is an error if obj is a circular list.

    (define a ’(1 8 2 8)) ; a may be immutable
    (define b (list-copy a))
    (set-car! b 3) ; b is mutable

    b => (3 8 2 8)
    a => (1 8 2 8)


# list-ref

    (list-ref lst k)

Returns the kth element of `lst`.

# list-set!

    (list-set! lst k obj)

Stores `obj` in element `k` of `lst`.

# list-tail

    (list-tail lst k) 

Returns the sublist of `lst` obtained by omitting the first `k` elements.

# list?

    (list? o)

Returns `#t` if the given object is a list, and `#f` otherwise.

# make-constructor

    (make-constructor make name)

# make-getter

    (make-getter sym name idx)

# make-list

    (make-list k )

    (make-list k fill)

Returns a newly allocated list of `k` elements. If a second
argument is given, then each element is initialized to fill.
Otherwise the initial contents of each element is unspecified.

# make-parameter

    (make-parameter init)

    (make-parameter init converter)

Returns a newly allocated parameter object, which is a procedure that accepts zero arguments and returns the value associated with the parameter object.

# make-setter

    (make-setter sym name idx)

# make-string

    (make-string k)

    (make-string k fill)

The `make-string` procedure returns a newly allocated
string of length `k`. If `fill` char is given, then all the characters
of the string are initialized to `fill` , otherwise the contents
of the string are unspecified.

# make-type-predicate

    (make-type-predicate pred name)

# map

    (map proc list1 list2 ...)

The `map` procedure applies `proc` element-wise to the elements of the lists and returns a list of the results, in order.

If more than one list is given and not all lists have the same length, `map` terminates when the shortest list runs out. The lists can be circular, but it is an error if all of them are circular. It is an error for `proc` to mutate any of the lists. The dynamic order in which `proc` is applied to the elements of the lists is unspecified. If multiple returns occur from `map`, the values returned by earlier returns are not mutated.

    (map cadr ’((a b) (d e) (g h)))
      => (b e h)

    (map (lambda (n) (expt n n))
         ’(1 2 3 4 5))
      => (1 4 27 256 3125)

    (map + ’(1 2 3) ’(4 5 6 7)) => (5 7 9)

# max

    (max x1 x2 ...)

`max` returns the largest of its arguments.

# member

    (member obj lst) 

    (member obj lst compare) 

Returns the first sublist of `lst` whose car is `obj`, where the sublists are the non-empty lists returned by `(list-tail lst k)` for `k` less than the length of `lst`. 

If obj does not occur in list, then #f (not the empty
list) is returned. 

`member` uses compare to compare elements of the list, if given, and `equal?` otherwise.

# memq

    (memq obj lst)

The `memq` procedure is the same as `member` but uses `eq?` to compare `obj` with the elements of `lst`.

# memv

    (memv obj lst)

The `memv` procedure is the same as `member` but uses `eqv?` to compare `obj` with the elements of `lst`.

# min

    (min x1 x2 ...)

`min` returns the smallest of its arguments.

# modulo

    (modulo a b)

Return the integer remainder after dividing `a` by `b`.

# negative?

    (negative? n)

Returns `#t` if `n` is a negative number and `#f` otherwise. It is an error if `n` is not a number.

# newline

    (newline)

    (newline port) 

Write a newline to `port`, or the current output port if no argument is given.

# not

    (not x)

The `not` procedure returns `#t` if `x` is false, and returns `#f` otherwise.

# numerator

    (numerator n)

Return the numerator of `n`.

# odd?

    (odd? num)

Return `#t` if `num` is an odd number and `#f` otherwise.

# open-input-bytevector

    (open-input-bytevector bv)

Takes a bytevector and returns a binary input port that delivers bytes from the bytevector.

# open-input-string

    (open-input-string string)

Takes a string and returns a textual input port that delivers
characters from the string.

# open-output-bytevector

    (open-output-bytevector open-output-string)

Returns a binary output port that will accumulate bytes for retrieval by `get-output-bytevector`.

# open-output-string

    (open-output-string)

Returns a textual output port that will accumulate characters for retrieval by `get-output-string`.

# or

*Syntax*

    (or {test1} ...)

Semantics: The `{test}` expressions are evaluated from left to right, and the value of the first expression that evaluates to a true value is returned. Any remaining expressions are not evaluated. If all expressions evaluate to `#f` or if there are no expressions, then `#f` is returned.

    (or (= 2 2) (> 2 1)) => #t
    (or (= 2 2) (< 2 1)) => #t
    (or #f #f #f) => #f
    (or (memq 'b '(a b c))
        (/ 3 0)) => (b c)

# output-port-open?

    (output-port-open? port)

Returns `#t` if `port` is an open output port, and `#f` otherwise.

# output-port?

    (output-port? obj)

Returns `#t` if `obj` is an output port, and `#f` otherwise.

# parameterize

*Syntax*

    (parameterize (({param1} {value1}) ...)

      {body})

# positive?

    (positive? n)

Returns `#t` if `n` is a positive number and `#f` otherwise.

# quasiquote

*Syntax*

    (quasiquote {qq template})

# quotient

    (quotient x y)

Return the quotient of dividing `x` by `y`.

# raise

    (raise obj)

Raises an exception by invoking the current exception handler on `obj`. 

The handler is called with the same dynamic environment as that of the call to `raise`, except that the current exception handler is the one that was in place when the handler being called was installed. If the handler returns, a secondary exception is raised in the same dynamic environment as the handler.

# raise-continuable

    (raise-continuable obj)

Raises an exception by invoking the current exception handler on `obj`. 

The handler is called with the same dynamic
environment as the call to `raise-continuable`, except
that: (1) the current exception handler is the one that was
in place when the handler being called was installed, and
(2) if the handler being called returns, then it will again
become the current exception handler. If the handler returns, the values it returns become the values returned by
the call to `raise-continuable`.

# rational?

    (rational? obj)

Returns `#t` if `obj` is a rational number and `#f` otherwise.

# read-line

    (read-line)

    (read-line port)

Read a line of text from the current input port or `port` if specified.

# read-string

    (read-string k)

    (read-string k port)

Read a string from the current input port or `port` if specified.

# receive

*Syntax*

    (receive {formals} {expression} {body})

# record?

    (record? obj)

Returns `#t` if `obj` is a record type and `#f` otherwise.

# remainder

    (remainder num1 num2)

Returns the remainder of dividing `num1` by `num2`.

# reverse

    (reverse lst)

Returns a newly allocated list that is the reverse of `lst`.

# round

    (round z)

Returns the closest integer to `z`.

# slot-set!

    (slot-set! name obj idx val)

# square

    (square z)

Returns the square of `z`.

# string

    (string char ...)

Returns a string containing the given characters.

# string->list

    (string->list string)

    (string->list string start)

    (string->list string start end)

Returns a newly allocated list containing the characters of `string`.

# string->utf8

    (string->utf8 string)

    (string->utf8 string start)

    (string->utf8 string start end)

Returns a newly allocated bytevector containing the UTF-8 bytecodes of `string`.

# string->vector

    (string->vector string)

    (string->vector string start)

    (string->vector string start end)

Returns a newly allocated vector containing the contents of `string`.

# string-copy

    (string-copy string)

    (string-copy string start)

    (string-copy string end)

Returns a copy of `string`.

# string-copy!

    (string-copy! to at from)

    (string-copy! to at from start)

    (string-copy! to at from start end)

Copies the characters of `string` from between `start` and `end`
to string `to`, starting at `at`.

    (define a "12345")
    (define b (string-copy "abcde"))
    (string-copy! b 1 a 0 2)
    b => "a12de"

# string-fill!

    (string-fill! str fill)

    (string-fill! str fill start)

    (string-fill! str fill start end)

The `string-fill!` procedure stores `fill` in the elements of `str` between `start` and `end`.

# string-for-each

    (string-for-each proc string1 string2 ...)

`string-for-each` is like `for-each` but the arguments consist of strings instead of lists.

# string-map

    (string-map proc string1 string2 ...)

`string-maph` is like `map` but the arguments consist of strings instead of lists.

# string<=?

    (string<=? str1 str2)

# string<?

    (string<? str1 str2)

# string=?

    (string=? str1 str2)

Returns `#t` if all of the given strings are equal and false otherwise.

# string>=?

    (string>=? str1 str2)

# string>?

    (string>? str1 str2)

# symbol=?

    (symbol=? symbol1 symbol2 symbol3 ...)

Returns `#t` if all of the arguments are the same symbol and `#f` otherwise.

# syntax-error

*Syntax*

    (syntax-error {message} {args} ...)

# truncate

    (truncate z)

# truncate-quotient 

    (truncate-quotient quotient)

# truncate-remainder 

    (truncate-remainder remainder)

# truncate/ 

    (truncate/ n m)

# type-slot-offset

    (type-slot-offset name sym)

# unless

*Syntax*

    (unless {test} {expression1} {expression2} ...)

Syntax: The `{test}` is an expression.

Semantics: The `test` is evaluated, and if it evaluates to `#f`, the expressions are evaluated in order. The result of the when expression is unspecified.

    (unless (= 1 1.0)
      (display "1")
      (display "2")) => unspecified and prints nothing

# utf8->string

    (utf8->string bytevector)

    (utf8->string bytevector start)

    (utf8->string bytevector start end)

Convert bytecodes in the given `bytevector` to a string.

# values

    (values obj ...)

Return arguments received as multiple values.

# vector

    (vector obj ...)

`vector` returns a vector of its arguments.

# vector->list

    (vector->list vector)

    (vector->list vector start)

    (vector->list vector start end)

Return a newly-allocated list containing the contents of `vector`.

# vector->string

    (vector->string vector)

    (vector->string vector start)

    (vector->string vector start end)

Return a newly-allocated string containing the contents of `vector`.

# vector-append

    (vector-append vector ...)

Returns a newly allocated vector whose elements are the concatenation of the elements of the given vectors.

# vector-copy

    (vector-copy vector)

    (vector-copy vector start)

    (vector-copy vector start end)

Returns a newly allocated copy of the elements of the given
vector between `start` and `end`. The elements of the new
vector are the same (in the sense of `eqv?`) as the elements
of the old.

# vector-copy!

    (vector-copy! to at from)

    (vector-copy! to at from start)

    (vector-copy! to at from start end)

Copies the elements of `vector` from between `start` and `end`
to vector `to`, starting at `at`.

    (define a (vector 1 2 3 4 5))
    (define b (vector 10 20 30 40 50))
    (vector-copy! b 1 a 0 2)
    b => #(10 1 2 40 50)

# vector-fill!

    (vector-fill! vector fill)

    (vector-fill! vector fill start)

    (vector-fill! vector fill start end)

The `vector-fill!` procedure stores `fill` in the elements of `vector` between `start` and `end`.

# vector-for-each

    (vector-for-each proc vector1 vector2 ...)

`vector-for-each` is like `for-each` but the arguments consist of vectors instead of lists.

# vector-map

    (vector-map proc vector1 vector2 ...)

`vector-map` is like `map` but the arguments consist of vectors instead of lists.

# when

*Syntax*

    (when {test} {expression1} {expression2} ...)

Syntax: The `{test}` is an expression.

Semantics: The `test` is evaluated, and if it evaluates to a true value, the expressions are evaluated in order. The result of the when expression is unspecified.

    (when (= 1 1.0)
      (display "1")
      (display "2")) => unspecified and prints 12

# with-exception-handler

    (with-exception-handler handler thunk)

The `with-exception-handler` procedure returns the results of invoking `thunk`. `handler` is installed as the current exception handler in the dynamic environment used for the invocation of `thunk`.

# with-handler

    (with-handler handler body)

# write-char

    (write-char char)

    (write-char char port)

Write `char` to the current output port or `port` if specified.

# write-string

    (write-string string)

    (write-string string port)

Write `string` to the current output port or `port` if specified.

# zero?

    (zero? n)

Returns `#t` if `n` is zero and `#f` otherwise.

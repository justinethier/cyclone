# Case-Lambda Library

The `(scheme case-lambda)` library exports the `case-lambda` syntax.

For more information see the [R<sup>7</sup>RS Scheme Specification](../../r7rs.pdf).

- [`case-lambda`](#case-lambda)

# case-lambda

*Syntax*

    (case-lambda {clause} ...)

Syntax: Each `{clause}` is of the form `({formals} {body})`, where `{formals}` and `{body}` have the same syntax as in a lambda expression.

Semantics: A `case-lambda` expression evaluates to a procedure that accepts a variable number of arguments and is lexically scoped in the same manner as a procedure resulting from a lambda expression.

When the procedure is called, the first `{clause}` for which the arguments agree with `{formals}` is selected, where agreement is specified as for the `{formals}` of a lambda expression. The variables of `{formals}` are bound to fresh locations, the values of the arguments are stored in those locations, the `{body}` is evaluated in the extended environment, and the results of `{body}` are returned as the results of the procedure call.

It is an error for the arguments not to agree with the `{formals}` of any `{clause}`.

    (define range
       (case-lambda
        ((e) (range 0 e))
        ((b e) (do ((r '() (cons e r))
                    (e (- e 1) (- e 1)))
                   ((< e b) r)))))

    (range 3) => (0 1 2)
    (range 3 5) => (3 4)

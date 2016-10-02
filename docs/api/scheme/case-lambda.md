---
layout: main
title: API
---

# Case-Lambda Library

The `(scheme case-lambda)` library exports the `case-lambda` syntax.

For more information see the [R<sup>7</sup>RS Scheme Specification](../../r7rs.pdf).

- [`case-lambda`](#case-lambda)

#case-lambda

    (case-lambda {clause} ...)

Syntax: Each `{clause}` is of the form `({formals} {body})`, where `{formals}` and `{body}` have the same syntax as in a lambda expression.

Semantics: A `case-lambda` expression evaluates to a procedure that accepts a variable number of arguments and is lexically scoped in the same manner as a procedure resulting from a lambda expression.

---
layout: main
title: API
---

# AST library

The `(scheme cyclone ast)` library defines abstract syntax tree types used during compilation.

- [`ast:make-lambda`](#astmake-lambda)
- [`ast:%make-lambda`](#astmake-lambda-1)
- [`ast:lambda?`](#astlambda)
- [`ast:lambda-id`](#astlambda-id)
- [`ast:lambda-args`](#astlambda-args) 
- [`ast:set-lambda-args!`](#astset-lambda-args)
- [`ast:lambda-body`](#astlambda-body)
- [`ast:set-lambda-body!`](#astset-lambda-body)
- [`ast:ast->sexp`](#astast-sexp)
- [`ast:sexp->ast`](#astsexp-ast) 
- [`ast:ast->pp-sexp`](#astast-pp-sexp)

# ast:make-lambda
    (ast:make-lambda args body)

# ast:%make-lambda
    (ast:%make-lambda lambda-id args body)

# ast:lambda?
    (ast:lambda? obj)

# ast:lambda-id

    (ast:lambda-id lambda-obj)

# ast:lambda-args 

    (ast:lambda-args lambda-obj)

# ast:set-lambda-args!

    (ast:set-lambda-args! lambda-obj args)

# ast:lambda-body 

    (ast:lambda-body lambda-obj)

# ast:set-lambda-body!

    (ast:set-lambda-body! lambda-obj body)

# ast:ast->sexp

    (ast:ast->sexp exp)

# ast:sexp->ast

    (ast:sexp->ast exp)

# ast:ast->pp-sexp

    (ast:ast->pp-sexp exp)


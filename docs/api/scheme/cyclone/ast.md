# AST library

The `(scheme cyclone ast)` library defines abstract syntax tree types used during compilation.

*This library is used internally by the compiler and its API may change at any time.*

- [`ast:make-lambda`](#astmake-lambda)
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
    (ast:make-lambda args body cont)

Create an instance of the `ast-lambda` record type. 

This data type is at the center of this module and consists of the following data:

* `id` - Unique numeric ID assigned to each lambda
* `args` - Arguments to the lambda. This may be one of:
     * symbol indicating a function takes any number of arguments
     * list of symbols corresponding to each of a fixed number of arguments to the function
     * improper list indicating a function taking a fixed number of required arguments as well as an arbitrary number of optional arguments
* `body` - Expression in the function body of the lambda
* `cont` - Boolean indicating whether the lambda has a continuation

`ast:make-lambda` automatically assigns the `id` field to a unique value.

# ast:lambda?
    (ast:lambda? obj)

Predicate indicating whether `obj` is an `ast-lambda` object.

# ast:lambda-id

    (ast:lambda-id lambda-obj)

Return the `id` field of the given `ast-lambda` object.

# ast:lambda-args 

    (ast:lambda-args lambda-obj)

Return the `args` field of the given `ast-lambda` object.

# ast:set-lambda-args!

    (ast:set-lambda-args! lambda-obj args)

Change the `args` field of the given `ast-lambda` object.

# ast:lambda-body 

    (ast:lambda-body lambda-obj)

Return the `body` field of the given `ast-lambda` object.

# ast:set-lambda-body!

    (ast:set-lambda-body! lambda-obj body)

Change the `body` field of the given `ast-lambda` object.

# ast:ast->sexp

    (ast:ast->sexp exp)

Convert an abstract syntax tree `exp` back into an equivalent expression consisting of standard Scheme S-expressions. IE: `lambda` forms instead of `ast-lambda` objects.

# ast:sexp->ast

    (ast:sexp->ast exp)

Convert a standard Scheme S-expression tree containing `lambda` forms into an equivalent abstract syntax tree consisting of equivalent `ast-lambda` objects.

# ast:ast->pp-sexp

    (ast:ast->pp-sexp exp)

Transform an abstract syntax tree into one that prints more cleanly.


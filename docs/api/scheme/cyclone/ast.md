# AST library

The `(scheme cyclone ast)` library defines abstract syntax tree types used during compilation.

- [`ast:make-lambda`](#ast:make-lambda)
- [`ast:%make-lambda`](#ast:%make-lambda)
- [`ast:lambda?`](#ast:lambda)
- [`ast:lambda-id`](#ast:lambda-id)
- [`ast:lambda-args`](#ast:lambda-args) 
- [`ast:set-lambda-args!`](#ast:set-lambda-args)
- [`ast:lambda-body`](#ast:lambda-body)
- [`ast:set-lambda-body!`](#ast:set-lambda-body)

#ast:make-lambda
    (ast:make-lambda args body)

#ast:%make-lambda
    (ast:%make-lambda lambda-id args body)

#ast:lambda?
    (ast:lambda? obj)

#ast:lambda-id
    (ast:lambda-id lambda-obj)
#ast:lambda-args 
    (ast:lambda-args lambda-obj)
#ast:set-lambda-args!
    (ast:set-lambda-args! lambda-obj args)
#ast:lambda-body 
    (ast:lambda-body lambda-obj)
#ast:set-lambda-body!
    (ast:set-lambda-body! lambda-obj body)

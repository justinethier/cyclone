# Lazy Library

The `(scheme lazy)` library exports procedures and syntax keywords for lazy evaluation.

For more information see the [R<sup>7</sup>RS Scheme Specification](../../r7rs.pdf).

- [`delay`](#delay) 
- [`force`](#force) 
- [`delay-force`](#delay-force) 
- [`make-promise`](#make-promise) 
- [`promise?`](#promise?)

#delay

    (delay {expression})                                lazy library syntax

#force 

    (force promise)

#delay-force 

    (delay-force {expression})                          lazy library syntax

#make-promise

    (make-promise obj)

#promise?

    (promise? obj)

Returns `#t` if object is a promise, and `#f` otherwise.

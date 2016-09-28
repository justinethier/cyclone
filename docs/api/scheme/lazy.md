# Lazy Library

The `(scheme lazy)` library exports procedures and syntax keywords for lazy evaluation.

For more information see the [R<sup>7</sup>RS Scheme Specification](../../r7rs.pdf).

- [`delay`](#delay) 
- [`force`](#force) 
- [`delay-force`](#delay-force) 
- [`make-promise`](#make-promise) 
- [`promise?`](#promise?)

#delay

    (delay {expression})

Return a promise object that can be asked in the future (via `force`) to evaluate `{expression}` and return the result.

#force 

    (force promise)

Force the value of a promise.

#delay-force 

    (delay-force {expression})

#make-promise

    (make-promise obj)

Return a promise which will return `obj` when forced. This function is similar to delay but does not delay its argument: it is a procedure rather than syntax. If `obj` is already a promise, it is returned.

#promise?

    (promise? obj)

Returns `#t` if object is a promise, and `#f` otherwise.

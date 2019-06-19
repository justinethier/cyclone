# Concurrency Library

The `(cyclone concurrent)` library makes it easier to write concurrent programs using Cyclone.

This library complements the functionality provided by [SRFI 18](../srfi/18.md).

## Index

### [Shared Objects](#shared-objects)
- [`make-shared`](#make-shared)
- [`share-all!`](#share-all)

### [Immutability](#immutability)
- [`immutable?`](#immutable)

### [Atoms](#atoms)

- [`make-atom`](#make-atom)
- [`atom`](#atom)
- [`atom?`](#atom-1)
- [`deref`](#deref)
- [`swap!`](#swap)
- [`compare-and-set!`](#compare-and-set)

## Shared Objects

Cyclone allocates new objects using the current thread's local stack. This is efficient for single-threaded code but makes it difficult to use an object from another thread. An object on a local stack could be overwritten or moved at any time, leading to undefined behavior.  The solution is to guarantee an object is located in a section of memory available for use by any thread. We call these shared objects.

Note that concurrency primitives must still be used to safely coordinate access to shared objects!

The following types of objects are always shared:

- Concurrency primitives (mutex, conditional variable, atom). These object are allocated directly on the heap since by definition multiple threads need to use them for synchronization.

- Fixnum integers and characters. These are immediates (IE, value types) so there is no object in memory to reference.

- Booleans, bignums, symbols, and the EOF object.

All other objects must be explicitly shared before they can be safely used by multiple threads.

### make-shared

    (make-shared obj)

Return a reference to an object that can be safely shared by many threads.

If the given object is atomic or already shared it it simply returned. Otherwise it is necessary to create a copy of the object.

Note this function may trigger a minor GC if a thread-local pair or vector is passed.

### share-all!

    (share-all!)

Allow all objects currently on the calling thread's local stack to be shared with other threads.

Note this function will trigger a minor garbage collection on the calling thread.

## Immutability

Many types of objects are mutable by default: pairs, strings, vectors, and bytevectors. However, if an object is declared as a literal constant then it will be designated immutable. 

The easiest way to do this is to use a single-quote, EG:

    cyclone> (import (cyclone concurrent))
    ok
    Error: Expected immutable object : (1 2)
    cyclone> (define my-lis '(1 2))
    ok
    cyclone> (immutable? my-lis)
    #t

It is an error to call a mutation procedure (such as `set-car!` or `string-set!`) on an immutable object.

### immutable? 

    (immutable? obj)

Predicate that returns `#t` if `obj` is immutable and `#f` otherwise.

## Atoms

This section provides atomic operations. The API is modelled after Clojure's [Atoms](https://clojure.org/reference/atoms). Per the Clojure docs:

> Atoms are an efficient way to represent some state that will never need to be coordinated with any other, and for which you wish to make synchronous changes.

An atom may only reference a shared object that is immutable. This guarantees that the value the atom is referencing cannot be modified unexpectedly by another thread.

For example:

    cyclone> (import (cyclone concurrent))
    ok
    cyclone> (make-atom (list 1 2))
    Error: Expected immutable object : (1 2)
    cyclone> (make-atom (make-shared '(1 2)))
    <atom 0x7f742b99bb00>

Example programs:

- Summing numbers using atomic operations: [`sum-atomic.scm`](../../../examples/threading/sum-atomic.scm)
- For comparison, summing numbers using locks: [`sum-mutex.scm`](../../../examples/threading/sum-mutex.scm)

### make-atom

    (make-atom obj)

Create a new atom referencing `obj`.

`obj` must be an immutable, shared object.

### atom

    (atom)
    (atom obj)

Create a new atom in the same manner as `make-atom`. If `obj` is not provided it will default to `#f`.

### atom?

    (atom? obj)

Type predicate, returns `#t` if `obj` is an atom and `#f` otherwise.

### deref

    (deref atom)

Dereference; returns the current value of `atom`. 

### swap!

    (swap! atom f . args)

Atomically swaps the value of `atom` to be:

    (apply f current-value-of-atom args) 
    
Note that `f` may be called multiple times and thus should be free of side effects. Returns the value that was swapped in.

Based on the procedure of the same name from Clojure.

### compare-and-set!

    (compare-and-set! atom oldval newval)

Atomically changes the value of `atom` to `newval` but only if the value of `atom` is currently equal to `oldval`. Based on the procedure of the same name from Clojure. This is also commonly known as the compare-and-swap (CAS) atomic instruction.


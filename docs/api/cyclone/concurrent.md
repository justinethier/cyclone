# Concurrency Library

The `(cyclone concurrent)` library provides functions to make it easier to write concurrent programs.

TODO: explain relationship to SRFI 18

## Index

- [`make-shared`](#make-shared)
- [`share-all!`](#share-all)
- [`immutable?`](#immutable)
- [`make-atom`](#make-atom)
- [`atom`](#atom)
- [`atom?`](#atom-1)
- [`deref`](#deref)
- [`swap!`](#swap)
- [`compare-and-set!`](#compare-and-set)

## Shared Objects

Cyclone allocates most new objects in a thread-local stack. This is efficient for single-threaded code but causes problems when an object must be used by multiple threads. An object on another thread's stack could be overwritten or moved at any time, leading to undefined behavior.

Cyclone deals with this problem using the concept of Shared Objects. A shared object is an object located in a segment of memory that is available for use by any thread. Note that concurrency primitives must still be used to safely coordinate access to these objects!

The following types of objects are already shared:

- Concurrency primitives (mutex, conditional variable, atom). These object are always allocated on the heap since the intent is for multiple threads to use them for synchronization.

- Fixnum integers and characters. These are immediates (IE, value types) so there is no object reference.

- Booleans, bignums, symbols, and the EOF object.

### make-shared

    (make-shared obj)

Return a reference to an object that can be safely shared by many threads.

If the given object is atomic or already shared it it simply returned. Otherwise it is necessary to create a copy of the object.

Note this function may trigger a minor GC if a thread-local pair or vector is passed.

### share-all!

    (share-all!)

Allow all objects currently on the calling thread's local stack to be shared with other threads.

Note this function will trigger a minor garbage collection on the calling thread.

## Immutable objects

Many types of objects are mutable by default: pairs, strings, vectors, and bytevectors. However, if an object is declared as a literal constant then it will be designated immutable. The easiest way to do this is to use a single-quote, EG:

    cyclone> (import (cyclone concurrent))
    ok
    Error: Expected immutable object : (1 2)
    cyclone> (define my-lis '(1 2))
    ok
    cyclone> (immutable? my-lis)
    #t

It is an error to attempt to call a mutation procedure (such as `set-car!` or `string-set!`) on an immutable object.

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
    cyclone> (make-atom (make-shared (list 1 2)))
    Error: Expected immutable object : (1 2)
    cyclone> (make-atom (make-shared '(1 2)))
    <atom 0x7f742b99bb00>


TODO: link to sum examples
TODO: memoization example?

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

    (deref atm)

Dereference an atom by returning its current value. 

### swap!

    (swap! atom f . args)

Atomically swaps the value of `atom` to be:

    (apply f current-value-of-atom args) 
    
Note that `f` may be called multiple times and thus should be free of side effects. Returns the value that was swapped in.

Based on the procedure of the same name from Clojure.

;; Notes:
;; swap! takes the current value of the Atom, calls the function on it (in this case, inc), and sets the new value. However, just before setting the new value, it checks to make sure the old value is still in there. If it's different, it starts over. It calls the function again with the new value it found. It keeps doing this until it finally writes the value. Because it can check the value and write it in one go, it's an atomic operation, hence the name.
;; 
;; That means the function can get called multiple times. That means it needs to be a pure function. Another thing is that you can't control the order of the function calls. If multiple threads are swapping to an Atom at the same time, order is out of the window. So make sure your functions are independent of order, like we talked about before.
;;

### compare-and-set!

    (compare-and-set! atom oldval newval)

Atomically changes the value of `atom` to `newval` but only if the value of `atom` is currently equal to `oldval`. Based on the procedure of the same name from Clojure. This is also commonly known as the compare-and-swap (CAS) atomic instruction.


# Concurrency Library

The `(cyclone concurrent)` library makes it easier to write concurrent programs using Cyclone, complementing the lower-level multithreading support provided by [SRFI 18](../srfi/18.md).

Much of the API is based on, or inspired by, similar constructs from Clojure.

Shared Queues and Thread Pools are loosly based on API's from [Sagittarius Scheme](https://bitbucket.org/ktakashi/sagittarius-scheme/wiki/Home).

## Index

[Shared Objects](#shared-objects)
- [`make-shared`](#make-shared)
- [`share-all!`](#share-all)

[Immutability](#immutability)
- [`immutable?`](#immutable)

[General](#general)
- [`deref`](#deref)
- [`realized?`](#realized)

[Atoms](#atoms)
- [`make-atom`](#make-atom)
- [`atom`](#atom)
- [`atom?`](#atom-1)
- [`swap!`](#swap)
- [`compare-and-set!`](#compare-and-set)

[Delays](#delays)
- [`shared-delay?`](#shared-delay)
- [`shared-delay`](#shared-delay-1)
- [`make-shared-delay`](#make-shared-delay)

[Promises](#promises)
- [`shared-promise?`](#shared-promise)
- [`make-shared-promise`](#make-shared-promise)
- [`deliver`](#deliver)

[Futures](#futures)
- [`future?`](#future)
- [`future`](#future-1)
- [`future-call`](#future-call)
- [`future-deref`](#future-deref)
- [`future-done?`](#future-done)

[Shared Queues](#shared-queues)
- [`shared-queue?`](#shared-queue)
- [`make-shared-queue`](#make-shared-queue)
- [`shared-queue`](#shared-queue)
- [`shared-queue-add!`](#shared-queue-add)
- [`shared-queue-remove!`](#shared-queue-remove)
- [`shared-queue-clear!`](#shared-queue-clear)
- [`shared-queue-size`](#shared-queue-size)
- [`shared-queue-capacity`](#shared-queue-capacity)
- [`shared-queue-wait-count`](#shared-queue-wait-count)
- [`shared-queue-empty?`](#shared-queue-empty)

[Thread Pool](#thread-pool)
- [`thread-pool?`](#thread-pool-1)
- [`make-thread-pool`](#make-thread-pool)
- [`thread-pool-size`](#thread-pool-size)
- [`thread-pool-idling-count`](#thread-pool-idling-count)
- [`thread-pool-idling?`](#thread-pool-idling)
- [`thread-pool-push-task!`](#thread-pool-push-task)
- [`thread-pool-release!`](#thread-pool-release)


## Shared Objects

Cyclone allocates new objects using the current thread's local stack. This is efficient for single-threaded code but makes it difficult to use an object from another thread. An object on a local stack could be overwritten or moved at any time, leading to undefined behavior.  The solution is to guarantee an object is located in a section of memory available for use by any thread. We call these shared objects.

Note that concurrency primitives must still be used to safely coordinate access to shared objects!

The following types of objects are always shared:

- Concurrency primitives (mutex, conditional variable, atom). These objects are allocated directly on the heap since by definition multiple threads need to use them for synchronization.

- Fixnum integers and characters. These are immediates (IE, value types) so there is no object in memory to reference.

- Booleans, bignums, symbols, and the EOF object.

All other objects must be explicitly shared before they can be safely used by multiple threads.

### make-shared

    (make-shared obj)

Return an object that can be safely shared by many threads.

If the given object is already shared it it simply returned. Otherwise it is necessary to create a copy of the object.

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

## General

### deref

    (deref object)

Dereference; returns the current value of the given concurrency object.

### realized?

    (realized? obj)

Returns `#t` if a value has been produced for a promise, delay, or future. Otherwise returns `#f`.

## Atoms

This section provides atomic operations modelled after Clojure's [Atoms](https://clojure.org/reference/atoms). 

Per the Clojure docs:

> Atoms are an efficient way to represent some state that will never need to be coordinated with any other, and for which you wish to make synchronous changes.

Note an atom may only reference a shared object that is immutable. This guarantees that the value the atom is referencing is not modified unexpectedly by another thread.

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

### swap!

    (swap! atom f . args)

Atomically swaps the value of `atom` to be:

    (apply f current-value-of-atom args) 
    
Note that `f` may be called multiple times and thus should be free of side effects. Returns the value that was swapped in.

Based on the procedure of the same name from Clojure.

### compare-and-set!

    (compare-and-set! atom oldval newval)

Atomically changes the value of `atom` to `newval` but only if the value of `atom` is currently equal to `oldval`. This is also commonly known as the compare-and-swap (CAS) atomic instruction.

Based on the procedure of the same name from Clojure. 

## Delays

A delay stores code that will not be executed until dereferenced via `deref`. The result is then cached.

Delays are based on delay objects from Clojure. 

Note delays are referred to as `shared-delay` to differentiate them from the single-threaded `delay` provided by `(scheme lazy)`.

### shared-delay?

    (shared-delay? obj)

Type predicate, returns `#t` if `obj` is a shared delay and `#f` otherwise.

### shared-delay

*Syntax*

    (shared-delay body ...)

Create a delay object that will execute `body` when dereferenced.

### make-shared-delay

    (make-shared-delay thunk)

Create a delay object that will execute `thunk` when dereferenced.

## Promises

A promise allows one or more threads to wait for a value to be generated by another thread. When `deref` is called on a promise the calling thread blocks until a value is delivered to the promise, via another thread calling `deliver`.

Promises are based off of promise objects from Clojure.

Note promises are referred to as `shared-promise` to differentiate them from the single-threaded functionality provided by `(scheme lazy)`.

###   shared-promise?

    (shared-promise? obj)

Type predicate, returns `#t` if `obj` is a shared promise and `#f` otherwise.

### make-shared-promise

    (make-shared-promise)

Create a new promise object.

### deliver

    (deliver promise obj)

Assign `promise` the value `obj` and unblock any threads that were waiting for the promise.

Note that subsequent calls to `deliver` have no effect. A value may only be delivered once to a given promise.

## Futures

Futures are used to perform computations on another thread. The results are cached and may be retrieved later using `deref`. Note that `deref` will block on a future until a result is generated by the other thread.

### future?

    (future? obj)

Type predicate, returns `#t` if `obj` is a future and `#f` otherwise.

### future

*Syntax*

    (future expr ...)

Executes the given expressions on another thread and returns a future object that can be dereferenced later to retrieve the cached result. Note the result will be the value obtained from executing the last expression.

### future-call

    (future-call thunk)

Invokes `thunk` on another thread and returns a future object that can be dereferenced later to retrieve the cached result.

`thunk` is a function that takes no arguments.

### future-done?

    (future-done? obj)

Returns `#t` if the future has finished executing on another thread, and `#f` otherwise.

## Shared Queues

A shared queue contains a circular buffer of objects intended to be shared among many threads. All operations are locked and thread-safe, and the queue will ensure any objects added are made into shared objects for use by other threads.

Removal from a queue is a blocking operation, so threads can easily wait for new data to arrive.

### shared-queue?

    (shared-queue? obj)

Predicate to determine if `obj` is a shared queue. Returns `#t` if so, `#f` otherwise.

### make-shared-queue      

    (make-shared-queue)

Create a new shared queue.

### shared-queue

    (shared-queue . elements)

Create a new shared queue containing the given elements.

### shared-queue-add!

    (shared-queue-add! q obj)

Add `obj` to the given shared queue `q`.

### shared-queue-remove!

    (shared-queue-remove! q)

Removes an element from the front of shared queue `q` and returns it to the caller. If `q` is empty the calling thread will be blocked until an element is available.

This function is meant to be called on a different thread than the thread(s) adding data to `q`.

### shared-queue-clear! 

    (shared-queue-clear! q)

Remove all elements from the given shared queue `q`.

### shared-queue-size       

    (shared-queue-size q)

Return the number of elements in the given shared queue.

### shared-queue-capacity

    (shared-queue-capacity q)

Return the maximum capacity of `q`. Note that when this capacity is exceeded the queue will automatically be resized.

### shared-queue-wait-count

    (shared-queue-wait-count q)

Return the number of threads currently blocked waiting for data from `q`.

### shared-queue-empty?     

    (shared-queue-empty? q)

Returns `#t` if the given queue is empty, and `#f` otherwise.

## Thread Pool

A thread pool is used to start several OS-level threads that will be used to execute jobs queued to the pool via `thread-pool-push-task!`. This allows an application to run asynchronous tasks on other threads while avoiding the overhead of starting a new thread for each task.

### thread-pool? 

    (thread-pool? obj)

Predicate to determine if `obj` is a thread pool. Returns `#t` if so, `#f` otherwise.

### make-thread-pool 

    (make-thread-pool thread-count [handler])

Create a new thread pool consisting of `thread-count` threads.

If `handler` is specified then it will be used as each thread's default exception handler.

### thread-pool-size

    (thread-pool-size tp)

Return the number of threads in thread pool `tp`.

### thread-pool-idling-count

    (thread-pool-idling-count tp)

Return number of idle threads in thread pool `tp`.

### thread-pool-idling?

    (thread-pool-idling? tp)

Return `#t` if any of the given thread pool's threads are idle, `#f` otherwise.

### thread-pool-push-task!

    (thread-pool-push-task! tp thunk)

Add a new task to the given thread pool `tp`.

`thunk` is a function accepting no arguments and will be queued to run on the next available thread.

### thread-pool-release!  

    (thread-pool-release! tp)

Call this if the thread pool `tp` will no longer be used. Stops and cleans up all thread pool threads. 



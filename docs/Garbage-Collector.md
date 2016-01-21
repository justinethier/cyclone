[<img src="images/cyclone-logo-04-header.png" alt="cyclone-scheme">](http://github.com/justinethier/cyclone)

# Garbage Collector

- [Introduction](#introduction)
- [Terms](#terms)
- [Code](#code)
- [Data Structures](#data-structures)
  - [Heap](#heap)
  - [Thread Data](#thread-data)
- [Minor Collection](#minor-collection)
- [Major Collection](#major-collection)
  - [Collection Cycle](#collection-cycle)
  - [Mutator Functions](#mutator-functions)
  - [Cooperation](#cooperation)
  - [Considerations](#considerations)
- [Looking Ahead](#looking-ahead)
- [Further Reading](#further-reading)

# Introduction

Cyclone uses a garbage collector (GC) to automatically free allocated memory. In practice, most allocations consist of short-lived objects such as temporary variables. A generational collector is used to perform two types of collection. A minor GC executes frequently to clean up most of these short-lived objects. Some objects will survive this collection and remain in memory. A major collection runs less frequently to free longer-lived objects that are no longer being used by the application.

Cheney on the MTA, a technique introduced by Henry Baker, is used to implement the first generation of our garbage collector. Objects are allocated directly on the stack using `alloca` so allocations are very fast, do not cause fragmentation, and do not require a special pass to free unused objects. 

Baker's technique uses a copying collector for both the minor and major generations of collection. One of the drawbacks of using a copying collector for major GC is that it relocates all the live objects during collection. This is a problem when multiple threads are accessing shared objects; an object reference could become invalid at any time when GC relocates the object. To prevent this either all threads must be stopped while major GC is running or a read barrier must be used each time an object is accessed. Both options add a potentially significant overhead. 

Cyclone supports native threads by using a tracing collector based on the Doligez-Leroy-Gonthier (DLG) algorithm for major collections. An advantage of this approach is that objects are not relocated once they are placed on the heap. In addition, major GC executes asynchronously so threads can continue to run concurrently even during collections.

# Terms
- Collector - A thread running the garbage collection code. The collector is responsible for coordinating and performing most of the work for major garbage collections.
- Mutation - A modification to an object. For example, changing a vector (array) entry.
- Mutator - A thread running user (or "application") code; there may be more than one mutator running concurrently.
- Read Barrier - Code that is executed before reading an object. Read barriers have a larger overhead than write barriers because object reads are much more common.
- Root - The collector begins tracing by marking one or more of these objects. A root object is guaranteed to survive a collection cycle.
- Write Barrier - Code that is executed before writing to an object.

# Code

The goal of this paper is to provide a high-level overview of Cyclone's garbage collector. The implementation code is available here:

- [`runtime.c`](../runtime.c) contains most of the runtime system, including code to perform minor GC. A good place to start would be the `GC` and `gc_minor` functions.
- [`gc.c`](../gc.c) contains the major GC code.

# Data Structures

## Heap

- based on implementation from chibi scheme
- linked list of pages
- requested memory always allocated in minimum chunk sizes
- heap is locked during alloc and sweep (free)
- heap is grown if necessary for alloc. collection process is async so no other choice (cannot collect immediately)

## Thread Data

At runtime Cyclone passes the current continuation, number of arguments, and a thread data parameter to each compiled C function. The thread data contains all of the necessary information to perform collections, including:

- Thread state
- Stack boundaries
- Current continuation and arguments
- Jump buffer
- Call history buffer
- Exception handler stack
- Contents of the minor GC write barrier
- Major GC parameters

## TODO: anything else? mutator/collector mark lists? write barrier lists?

# Minor Collection

Cyclone converts the original program to continuation passing style (CPS) and compiles it as a series of C functions that never return. At runtime the code periodically checks to see if the executing thread's stack has exceeded a certain size. When this happens a minor GC is started and all live stack objects are copied to the heap.

Root objects are "live" objects the collector uses to begin the tracing process. Cyclone's minor collector treats the following as roots:

- The current continuation
- Arguments to the current continuation
- Mutations contained in the write barrier
- Closures from the exception stack
- Global variables

A minor collection is always performed for a single mutator thread, usually by the thread itself. The algorithm is based on Cheney on the MTA and consists of the following:

- Move any root objects on the stack to the heap. 
  - Replace the stack object with a forwarding pointer. The forwarding pointer ensures all references to a stack object refer to the same heap object, and allows minor GC to handle cycles.
  - Record each moved object in a buffer to serve as the Cheney "to-space".
- Loop over the "to-space" buffer and check each object moved to the heap. Move any child objects that are still on the stack. This loop continues until all live objects are moved.
- Cooperate with the collection thread (see next section).
- Perform a `longjmp` to reset the stack and call into the current continuation.

Finally, although not mentioned in Baker's paper, a heap object can be modified to contain a reference to a stack object. For example, by using a `set-car!` to change the head of a list. This is problematic since stack references are no longer valid after a minor GC. We account for these "mutations" by using a write barrier to maintain a list of each modified object. During GC, these modified objects are treated as roots to avoid dangling references.

# Major Collection

A single heap is used to store objects relocated from the various thread stacks. Eventually the heap will run too low on space and a collection is required to reclaim unused memory. The collector thread is used to perform a major GC with cooperation from the mutator threads.

Each object is assigned a color to indicate the status of its memory:

- Blue - Unallocated memory.
- Red - Objects on the stack.
- White - Heap memory that has not been scanned by the collector. Memory that is still white after the collector finishes tracing is garbage.
- Gray - Objects marked by the collector that may still have child objects that must be marked.
- Black - Objects marked by the collector whose immediate child objects have also been marked.

White is also referred to as the clear color and black as the mark color. Gray is never explicitly assigned to an object. Instead, objects are grayed by being added to lists of gray objects awaiting marking. This improves performance by avoiding repeated passes over the heap to search for gray objects.

Each of the mutator threads, and the collector itself, has a status variable:

     typedef enum { STATUS_ASYNC 
                  , STATUS_SYNC1 
                  , STATUS_SYNC2 
                  } gc_status_type;

The collector performs a handshake with the mutators to change status. The collector will update its status variable and then wait for all of the collectors to change their status before continuing. The mutators periodically call a cooperate function to check in and update their status to match the collectors. A handshake is complete once all mutators have updated their status.

## Collection Cycle

During a GC cycle the collector thread transitions through the following states:

### Clear
The collector swaps the values of the clear color (white) and the mark color (black). This is more efficient than modifying the color on each object. The collector then transitions to sync 1.

### Mark
The collector transitions to sync 2 and then async. At this point it marks the global variables and waits for the mutators to also transition to async.

### Trace
The collector finds all live objects and marks them black.

### Sweep
The collector scans the heap and frees memory used by all white objects. If the heap is still low on memory at this point the heap will be increased in size.

Any terminated threads will have their thread data freed now. ( TODO: Thread data is kept through the collection cycle to ... ensure live objects are not missed? double-check this)

### Resting
The collector cycle is complete and it rests until it is triggered again.

## Collector Functions

### Mark Gray

TODO: data structure used instead of explicit marking, to improve performance

### Collector Mark Gray

### Mark Black

### Collector Trace

TODO: needed? should this just be part of the collector trace section?

## Mutator Functions

Each mutator calls the following functions to coordinate with the collector.

### Create

This function is called by a mutator to allocate memory on the heap for an object. This is generally only done during a minor GC when each object is relocated to the heap.

### Update

A write barrier is used to ensure any modified objects are properly marked for the current collection cycle. There are two cases:

- Gray the object's new and old values if the mutator is in a synchronous status. Graying of the new value is a special case since it may still be on the stack. Instead of marking it directly, the object is tagged to be grayed when it is relocated to the heap.
- Gray the object's old value if the collector is in the tracing stage.

### Cooperate

Each mutator is required to periodically call this function to cooperate with the collector. Cyclone does this after each minor GC.

During cooperation a mutator will update its status to match the collector's status, to handshake with the collector. 

In addition, when a mutator transitions to async it will:

- Mark all of its roots gray
- Use black as the allocation color for any new objects to prevent them from being collected during this cycle.

## Cooperation

Unfortunately a mutator cannot cooperate with the collector if it is blocked. For example, a mutator could block forever waiting for user input reading from an I/O port.

TODO: explain how collector cooperates on behalf of a mutator.

important to explain how minor/major algorithms are interleaved. EG:

thread states:

    typedef enum { CYC_THREAD_STATE_NEW
                 , CYC_THREAD_STATE_RUNNABLE
                 , CYC_THREAD_STATE_BLOCKED
                 , CYC_THREAD_STATE_BLOCKED_COOPERATING
                 , CYC_THREAD_STATE_TERMINATED
                 } cyc_thread_state_type;


## Considerations

Garbage collection papers are generally silent on when to actually start the collection cycle - presumably leaving this up to the implementation. Cyclone checks the amount of free memory as part of its cooperation code. A major GC cycle is started if the amount of free memory dips below a threshold.

# Looking Ahead

Motivations: 
- Extend baker's approach to support multiple mutators
- Position to potentially support state of the art GC's built on top of DLG (Stopless, Chicken, Clover)

Limitations or potential issues:
- DLG memory fragmentation could be an issue for long-running programs
- Locking, atomics, etc
- Improve performance?

Cyclone's implementation generally tries to use atomic operations, but there is some locking. In particular, heap is protected by lock during object allocation and deallocation. This is one area that could probably be improved.

quote on this? - first priority must be correctness, can address performance over time

# Further Reading

- [CHICKEN internals: the garbage collector](http://www.more-magic.net/posts/internals-gc.html), by Peter Bex
- [CONS Should Not CONS Its Arguments, Part II: Cheney on the M.T.A.](http://www.pipeline.com/~hbaker1/CheneyMTA.html), by Henry Baker
- Fragmentation Tolerant Real Time Garbage Collection (PhD Dissertation), by Filip Pizlo
- Implementing an on-the-fly garbage collector for Java, by Domani et al
- Incremental Parallel Garbage Collection, by Paul Thomas
- Portable, Unobtrusive Garbage Collection for Multiprocessor Systems, by Damien Doligez and Georges Gonthier

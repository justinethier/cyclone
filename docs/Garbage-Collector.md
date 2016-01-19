[<img src="images/cyclone-logo-04-header.png" alt="cyclone-scheme">](http://github.com/justinethier/cyclone)

# Garbage Collector

- [Introduction](#introduction)
  - [Terms](#terms)
- [Minor Collection](#minor-collection)
- [Major Collection](#major-collection)
- [Limitations and Looking Ahead](#limitations-and-looking-ahead)
- [Further Reading](#further-reading)

# Introduction

Cyclone uses the Cheney on the MTA technique to implement tail calls, efficient continuations, and generational garbage collection. Objects are allocated directly on the stack and functions are never allowed to return, until eventually the stack grows too large and a minor garbage collection (GC) is performed. Live objects are then relocated from the stack to the heap and a longjmp is used to continue execution at the beginning of the stack.

The original technique uses a Cheney copying collector for both the minor collection and a major collection. One of the drawbacks of using a copying collector for major GC is that it relocates all the live objects during collection. In order to prevent corrupting references used by other threads, either all threads would need to be stopped at the same time during major GC or a read barrier would need to be used - potentially introducing a high overhead. 

Cyclone supports native threads by using a tracing collector based on the Doligez-Leroy-Gonthier (DLG) algorithm for major collections. And advantage of this approach is that objects are not relocated once they are allcated on the heap. Threads can continue to run concurrently even during collections.

## Terms
- Garbage Collection (GC)
- Collector - A single thread call the collector performs major garbage collections.
- Mutator - Each thread running application code is called a mutator.
- Mutation - A modification to an object. 
- Root - The collector begins tracing by marking one or more of these objects.

# Minor Collection

In Cheney on the MTA, the original program is converted to continuation passing style (CPS) and compiled as a series of C functions that never return. Instead the code periodically checks to see if the stack has exceeded a certain size. When this happens, a minor GC is started and the code traces though all of the live closures to copy stack objects to the heap.

Cyclone passes the current continuation, number of arguments, and a thread data parameter to each compiled C function. The thread data contains all of the necessary information to perform collections, including:

- Thread state
- Stack boundaries
- Minor GC write barrier
- Jump buffer
- Current continuation and arguments
- Major GC parameters
- Call history buffer
- Exception handler stack

Although not mentioned in Baker's paper, a heap object can be modified to contain a reference to a stack object. For example, by using a `set-vector!` to change the contents of a vector slot. This is problematic since stack references are no longer valid after a minor GC. We account for these "mutations" by using a write barrier to maintain a list of each modified object. During GC, these modified objects are treated as roots to avoid dangling references.

Root objects are "live" objects the collector uses to begin the tracing process. A root object is guaranteed to survive a collection. Cyclone's minor collector treats the following as roots:

- The current continuation
- Arguments to the current continuation
- Mutations contained in the write barrier
- Closures from the exception stack
- Global variables

The actual minor collection algorithm consists of the following steps:

- Move any root objects on the stack to the heap. 
  - Replace the stack object with a forwarding pointer. The forwarding pointer ensures all references to a stack object refer to the same heap object, and allows minor GC to handle cycles.
  - Record each moved object in a buffer to serve as the Cheney "to-space".
- Loop over the "to-space" buffer and move any child objects that are on the stack. This loop continues until all "live" objects are moved.
- Cooperate with the collection thread (see next section).
- Perform a `longjmp` to reset the stack and call into the current continuation.

# Major Collection

## Overview

A single heap is used to store objects relocated from the various thread stacks. Eventually the heap will run too low on space and a collection will be required to reclaim unused memory.

Major GC is performed using the DLG algorithm with modifications from Domani et al. A single collector thread is used to perform the major GC, and coordinate with the application (or "mutator") threads.

Cyclone's implementation generally tries to use atomic operations, but there is some locking. In particular, heap is protected by lock during object allocation and deallocation. This is one area that could probably be improved.

## DLG Algorithm

Each object is assigned a color:

- Blue - Unallocated memory.
- Red - Objects on the stack.
- White - Heap memory that has not been scanned by the collector. Memory that is still white after the collector finishes tracing is garbage.
- Gray - Objects marked by the collector that may still have child objects that must be marked.
- Black - Objects marked by the collector whose immediate child objects have also been marked.

Each of the threads, and the collector itself, has a status variable:

     typedef enum { STATUS_ASYNC 
                  , STATUS_SYNC1 
                  , STATUS_SYNC2 
                  } gc_status_type;

At the start of the collection cycle the collector changes its status to sync 1. The mutators periodically cooperate to check the collector's status, and perform a "handshake" by updating their status to sync 1. A second handshake is performed to transition to sync 2.

Once all of the mutators are in sync 2, the collector transitions to async to perform tracing and sweep. Each mutator will respond to async by marking its roots. From this point the mutators will also allocate new objects as black to prevent them from being collected during this cycle. Once swep is complete all garbage has been collected and the collector will rest until the next collection cycle.

## Mutator Functions

Update(x,i,y)
Create
Cooperate

## Collection Cycle
typedef enum { STAGE_CLEAR_OR_MARKING 
             , STAGE_TRACING 
             //, STAGE_REF_PROCESSING 
             , STAGE_SWEEPING 
             , STAGE_RESTING
             } gc_stage_type;


important to explain how minor/major algorithms are interleaved. EG:

thread states:
typedef enum { CYC_THREAD_STATE_NEW
             , CYC_THREAD_STATE_RUNNABLE
             , CYC_THREAD_STATE_BLOCKED
             , CYC_THREAD_STATE_BLOCKED_COOPERATING
             , CYC_THREAD_STATE_TERMINATED
             } cyc_thread_state_type;

typedef struct gc_thread_data_t gc_thread_data;


Collector Functions

When to execute major GC?
ETC

# Limitations and Looking Ahead

Motivations: 
- Extend baker's approach to support multiple mutators
- Position to potentially support state of the art GC's built on top of DLG (Stopless, Chicken, Clover)

Limitations or potential issues:
- DLG memory fragmentation could be an issue for long-running programs

# Further Reading

- [CHICKEN internals: the garbage collector](http://www.more-magic.net/posts/internals-gc.html)
- [CONS Should Not CONS Its Arguments, Part II: Cheney on the M.T.A.](http://www.pipeline.com/~hbaker1/CheneyMTA.html), by Henry Baker
- Fragmentation Tolerant Real Time Garbage Collection (PhD Dissertation), by Filip Pizlo
- Implementing an on-the-fly garbage collector for Java, by Domani et al
- Incremental Parallel Garbage Collection, by Paul Thomas
- Portable, Unobtrusive Garbage Collection for Multiprocessor Systems, by Damien Doligez and Georges Gonthier

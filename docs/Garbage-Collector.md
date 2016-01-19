# Introduction

Cyclone uses the Cheney on the MTA technique to implement tail calls, efficient continuations, and generational garbage collection. Objects are allocated directly on the stack and functions are never allowed to return, until eventually the stack grows too large and a minor garbage collection (GC) is performed. Live objects are then relocated from the stack to the heap and a longjmp is used to continue execution at the beginning of the stack.

The original technique uses a Cheney copying collector for both the minor collection and a major collection. One of the drawbacks of using a copying collector for major GC is coordination among multiple threads...

Cyclone supports native threads by using a tracing collector based on the Doligez-Leroy-Gonthier (DLG) algorithm for major collections.

terms: mutators, collector, what else?

motivations: 
- extend baker's approach to support multiple mutators
- State of the art GC's are built on top of DLG (stopless, chicken, clover)

limitations or potential issues:
- DLG memory fragmentation could be an issue for long-running programs

## minor GC

changes from Cheney on MTA:
- each thread has its own stack

## major GC

overview from paper

- single collector thread, multiple mutator threads
- single heap
- generally try to use atomic operations, but there is some locking. In particular, heap is protected by lock during object allocation / deallocation

GC stages / status:
/* Enums for tri-color marking */
typedef enum { STATUS_ASYNC 
             , STATUS_SYNC1 
             , STATUS_SYNC2 
             } gc_status_type;

typedef enum { STAGE_CLEAR_OR_MARKING 
             , STAGE_TRACING 
             //, STAGE_REF_PROCESSING 
             , STAGE_SWEEPING 
             , STAGE_RESTING
             } gc_stage_type;

object colors:
red
blue
white
gray
black


important to explain how minor/major algorithms are interleaved. EG:

thread states:
typedef enum { CYC_THREAD_STATE_NEW
             , CYC_THREAD_STATE_RUNNABLE
             , CYC_THREAD_STATE_BLOCKED
             , CYC_THREAD_STATE_BLOCKED_COOPERATING
             , CYC_THREAD_STATE_TERMINATED
             } cyc_thread_state_type;

typedef struct gc_thread_data_t gc_thread_data;

ETC

# Further Reading

- [CONS Should Not CONS Its Arguments, Part II: Cheney on the M.T.A.](http://www.pipeline.com/~hbaker1/CheneyMTA.html), by Henry Baker
- Implementing an on-the-fly garbage collector for Java, by Domani et al
- Portable, Unobtrusive Garbage Collection for Multiprocessor Systems, by Damien Doligez and Georges Gonthier

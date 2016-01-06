TODO: notes on how cyclone's GC works

terms: mutators, collector, what else?

two types:

- minor GC (stack portion from Cheney on MTA)
- major GC - Doligez-Leroy-Gonthier (DLG) collector

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

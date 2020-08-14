/* 
 * Cyclone Scheme
 * Copyright (c) 2014, Justin Ethier
 * All rights reserved.
 *
 * This file contains C types used by compiled programs.
 */

#ifndef CYCLONE_TYPES_H
#define CYCLONE_TYPES_H

#include <math.h>
#include <complex.h>
#include <setjmp.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <pthread.h>
#include <stdint.h>
#include <dlfcn.h>
#include "cyclone/bignum.h"

#ifdef CYC_HIGH_RES_TIMERS
/**
 * \defgroup hrt High resolution timers
 */
/**@{*/
long long hrt_get_current();
long long hrt_cmp_current(long long tstamp);
void hrt_log_delta(const char *label, long long tstamp);
/**@}*/
#endif

/**
 * Generic object type
 * \ingroup objects
 */
typedef void *object;

/**
 * Define a unique tag for each possible type of object.
 *
 * Remember to update tag_names in runtime.c when adding new tags
 *\ingroup objects
 */
enum object_tag {
        closure0_tag    = 0
      , closure1_tag    = 1
      , closureN_tag    = 2
      , macro_tag       = 3 // Keep closures here for quick type checking
      , boolean_tag     = 4
      , bytevector_tag  = 5
      , c_opaque_tag    = 6
      , cond_var_tag    = 7
      , cvar_tag        = 8
      , double_tag      = 9
      , eof_tag         = 10
      , forward_tag     = 11
      , integer_tag     = 12
      , bignum_tag      = 13
      , mutex_tag       = 14
      , pair_tag        = 15
      , port_tag        = 16 
      , primitive_tag   = 17
      , string_tag      = 18
      , symbol_tag      = 19
      , vector_tag      = 20
      , complex_num_tag = 21
      , atomic_tag      = 22
      , void_tag        = 23
};

/**
 * Returns a true value if object is not a closure, or false otherwise
 */
#define obj_is_not_closure(obj) \
  ((obj == NULL) || is_value_type(obj) || (type_of(obj) > macro_tag))

/**
 * Defines the size of object tags
 * \ingroup objects
 */
typedef unsigned char tag_type;

/**
 * Access an object's tag.
 * \ingroup objects
 */
#define type_of(obj) (((pair_type *) obj)->tag)

/**
 * \defgroup gc Garbage collection
 *
 * @brief The Cyclone runtime's garbage collector (GC)
 *
 * When using the FFI there is normally no need to call 
 * into this code unless something is specifically mentioned
 * in the User Manual.
 */
/**@{*/

/**
 * \defgroup gc_major Major GC
 * @brief Major GC is responsible for removing unused objects from
 * the heap.
 */
/**@{*/

////////////////////////////////
// Parameters for size of a "page" on the heap (the second generation GC), in bytes.

/** Grow first page by adding this amount to it */
#define GROW_HEAP_BY_SIZE (2 * 1024 * 1024)    

/** Size of the first page */
#define INITIAL_HEAP_SIZE (3 * 1024 * 1024)     

/** Normal size of a heap page */
#define HEAP_SIZE (8 * 1024 * 1024)    

// End heap page size parameters
////////////////////////////////

/////////////////////////////
// Major GC tuning parameters

/** Start GC cycle if % heap space free below this percentage */
#define GC_COLLECTION_THRESHOLD 0.0125 //0.05

/** Start GC cycle if fewer than this many heap pages are unswept */
#define GC_COLLECT_UNDER_UNSWEPT_HEAP_COUNT 3

/** After major GC, grow the heap so at least this percentage is free */
#define GC_FREE_THRESHOLD 0.40
// END GC tuning
/////////////////////////////

/** Number of functions to save for printing call history */
#define MAX_STACK_TRACES 10

/** Show diagnostic information for the GC when program terminates */
#define DEBUG_SHOW_DIAG 0

/** Show diagnostic information before/after sweeping */
#define GC_DEBUG_SHOW_SWEEP_DIAG 0

/** GC debugging flag */
#define GC_DEBUG_TRACE 0

/** GC debugging flag */
#define GC_DEBUG_VERBOSE 0

/** 
 *  Additional runtime checking of the GC system.
 *  This is here because these checks should not be
 *  necessary if GC is working correctly. 
 */
#define GC_SAFETY_CHECKS 0

/** Generic constant used for GC sleep/wake */
#define NANOSECONDS_PER_MILLISECOND 1000000

/* GC data structures */

/**
 * Group heap pages by type, to attempt to limit fragmentation
 * and improve performance.

 TODO: starting to run into problems when adding additional "sizes" of heap page,
 possibly due to increasing amounts of page faults due to non-locality???
 
 Basically for X86_64 everything works great when a 96 byte heap is added, but slows way down when additional
 heaps (128, 160) are also added.

 32 bit x86 is starting to have trouble with just a 96 byte heap added.

 In the future, a better solution might be to allocate arrays (closureN's, vectors, bytevectors, and strings)
 as fixed-size chunks to prevent heap fragmentation. The advantage is then we have no fragmentation directly.
 But, an array will no longer be contiguous so they may cause other problems, and the runtime has to change
 to work with non-contiguous arrays. This would also cause a lot of problems for strings since the built-in
 functions would no longer work (EG: strlen, etc).
 */
typedef enum { 
    HEAP_SM = 0  // 32 byte objects (min gc_heap_align)
  , HEAP_64
  , HEAP_96
  , HEAP_REST    // Everything else
  , HEAP_HUGE    // Huge objects, 1 per page
} gc_heap_type;

/** The first heap type that is not fixed-size */
#if INTPTR_MAX == INT64_MAX
#define LAST_FIXED_SIZE_HEAP_TYPE HEAP_96
#else
#define LAST_FIXED_SIZE_HEAP_TYPE HEAP_64
#endif

/** The number of `gc_heap_type`'s */
#define NUM_HEAP_TYPES (HEAP_HUGE + 1)

/** 
 * Linked list of free memory chunks on a heap page
 */
typedef struct gc_free_list_t gc_free_list;
struct gc_free_list_t {
  unsigned int size;
  gc_free_list *next;
};

/**
 * Heap page
 *
 * @brief Contains data for a single page of the heap.
 *
 * Note there are groups of parameters to support:
 * - Bump-allocation - This type of allocation is faster but only applicable when a page is first created or empty.
 * - Lazy sweep
 */
typedef struct gc_heap_t gc_heap;
struct gc_heap_t {
  gc_heap_type type;
  /** Size of the heap page in bytes */
  unsigned int size;
  /** Keep empty page alive this many times before freeing */
  unsigned int ttl; 
  /** Bump: Track remaining space; this is useful for bump&pop style allocation */
  unsigned int remaining;
  /** For fixed-size heaps, only allocate blocks of this size */
  unsigned block_size;
  /** Lazy-sweep: Amount of heap data that is free */
  unsigned int free_size; 
  /** Lazy-sweep: Determine if the heap is full */
  unsigned char is_full; 
  /** Lazy-sweep: Determine if the heap has been swept */
  unsigned char is_unswept;
  /** Lazy-sweep: Start GC cycle if fewer than this many heap pages are unswept */
  int num_unswept_children;
  /** Last size of object that was allocated, allows for optimizations */
  unsigned int last_alloc_size;
  /** Next page that has free space, lets alloc find that page faster */
  gc_heap *next_free;
  /** Linked list of free memory blocks in this page */
  gc_free_list *free_list;
  /** Next page in this heap */
  gc_heap *next;                // TBD, linked list is not very efficient, but easy to work with as a start
  /** Actual data in this page */
  char *data;
  /** End of the data when using bump alllocation or NULL when using free lists */
  char *data_end;
};

/**
 * A heap root is the heap's first page
 */
typedef struct gc_heap_root_t gc_heap_root;
struct gc_heap_root_t {
  gc_heap **heap;
};

/**
 * Header added to each object for GC purposes
 */
typedef struct gc_header_type_t gc_header_type;
struct gc_header_type_t {
  unsigned char mark;      // mark bits 
  unsigned char grayed:1;    // stack object to be grayed when moved to heap
  unsigned char immutable:1; // Flag normally mutable obj (EG: pair) as read-only
};

/** Get an object's `mark` value */
#define mark(x) (((list) x)->hdr.mark)

/** Get an object's `grayed` value */
#define grayed(x) (((list) x)->hdr.grayed)

//** Access an object's "immutable" field */
#define immutable(x) (((list) x)->hdr.immutable)

/** Enums for tri-color marking */
typedef enum { STATUS_ASYNC, STATUS_SYNC1, STATUS_SYNC2
} gc_status_type;

/** Stages of the Major GC's collector thread */
typedef enum { STAGE_CLEAR_OR_MARKING, STAGE_TRACING
      //, STAGE_REF_PROCESSING 
  , STAGE_SWEEPING, STAGE_RESTING
} gc_stage_type;

// Constant colors are defined here.
// The mark/clear colors are defined in the gc module because
// the collector swaps their values as an optimization.

/** Memory not to be collected by major GC, such as on the stack */
#define gc_color_red  0         

/** Unallocated memory */
#define gc_color_blue 2         

/** Mark buffers */
typedef struct mark_buffer_t mark_buffer;
struct mark_buffer_t {
  void **buf;
  unsigned buf_len;
  mark_buffer *next;
};

/** Threading */
typedef enum { CYC_THREAD_STATE_NEW, CYC_THREAD_STATE_RUNNABLE,
  CYC_THREAD_STATE_BLOCKED, CYC_THREAD_STATE_BLOCKED_COOPERATING,
  CYC_THREAD_STATE_TERMINATED
} cyc_thread_state_type;

/**
 * Thread data structures 
 * @brief Each thread is given an instance of this struct to 
 *        maintain its state
 */
typedef struct gc_thread_data_t gc_thread_data;
struct gc_thread_data_t {
  /** Call History: circular buffer of previous calls */
  char **stack_traces;
  /** Call History: Current place in the buffer */
  int stack_trace_idx;
  /** Call History: Previous frame written to call history; allows us to avoid duplicate entries */
  char *stack_prev_frame;
  /** Current state of this thread */
  cyc_thread_state_type thread_state;
  /** Minor GC: Data needed to initiate stack-based minor GC */
  char *stack_start;
  /** Minor GC: Data needed to initiate stack-based minor GC, defines the end of the memory range */
  char *stack_limit;
  /** Minor GC: write barrier */
  void **mutations;
  /** Minor GC: Size of the minor GC write barrier */
  int mutation_buflen;
  /** Minor GC: Number of entries in the minor GC write barrier */
  int mutation_count;
  /** Minor GC: Is minor collection of globals necessary? */
  unsigned char globals_changed;
  /** Minor GC: List of objects moved to heap during minor GC */
  void **moveBuf;
  /** Minor GC: Length of `moveBuf` */
  int moveBufLen;
  /** Heap GC: mark color used for new allocations */
  unsigned char gc_alloc_color;
  /** Heap GC: mark color the major GC is currently using tracing. This can be different than the alloc color due to lazy sweeping */
  unsigned char gc_trace_color;
  /** Heap GC: Is the major GC done tracing? */
  uint8_t gc_done_tracing;
  /** Heap GC: current state of the collector */
  int gc_status;
  /** Heap GC: index of last write to the mark buffer */
  int last_write;
  /** Heap GC: index of last read from the mark buffer */
  int last_read;
  /** Heap GC: 
   *  Need this because minor GC may still be moving objects to the heap and
   *  if we try to trace before minor GC is done, some of the objects may be
   *  missed. So we "pend" them until minor GC is done and we know everything
   *  is on the heap.
   */
  int pending_writes;
  /** Heap GC: buffer of grey objects */
  mark_buffer *mark_buffer;
  /** Heap GC: length of the mark buffer */
  int mark_buffer_len;
  /** Heap GC: lock used to coordinate access between the collector and this thread */
  pthread_mutex_t lock;
  /** Id of the current thread */
  pthread_t thread_id;
  /** Heap GC: Root of this thread's heap */
  gc_heap_root *heap;
  /** Heap GC: Cached amount of free heap space, so we do not need to recalculate on the fly */
  uintptr_t *cached_heap_free_sizes;
  /** Heap GC: Cached total amount of heap space */
  uintptr_t *cached_heap_total_sizes;
  /** Heap GC: Number of "huge" allocations by this thread */
  int heap_num_huge_allocations;
  /** Heap GC: Keep track of number of minor GC's for use by the major GC */
  int num_minor_gcs;
  /** Exception handler stack */
  object exception_handler_stack;
  /** Parameter object data */
  object param_objs;
  /** Need the following to perform longjmp's */
  jmp_buf *jmp_start;
  /** After longjmp, pick up execution here */
  object gc_cont;
  /** After longjmp, pass continuation these arguments */
  object *gc_args;
  /** Length of `gc_args` */
  short gc_num_args;
  /**  Thread object, if applicable */
  object scm_thread_obj;
};

/* GC prototypes */
void gc_initialize(void);
void gc_add_new_unrunning_mutator(gc_thread_data * thd);
void gc_add_mutator(gc_thread_data * thd);
void gc_remove_mutator(gc_thread_data * thd);
int gc_is_mutator_active(gc_thread_data *thd);
int gc_is_mutator_new(gc_thread_data *thd);
void gc_sleep_ms(int ms);
gc_heap *gc_heap_create(int heap_type, size_t size, gc_thread_data *thd);
gc_heap *gc_heap_free(gc_heap *page, gc_heap *prev_page);
void gc_heap_merge(gc_heap *hdest, gc_heap *hsrc);
void gc_merge_all_heaps(gc_thread_data *dest, gc_thread_data *src);
void gc_print_stats(gc_heap * h);
gc_heap *gc_grow_heap(gc_heap * h, size_t size, gc_thread_data *thd);
char *gc_copy_obj(object hp, char *obj, gc_thread_data * thd);
void *gc_try_alloc(gc_heap * h, size_t size, char *obj,
                   gc_thread_data * thd);
void *gc_try_alloc_slow(gc_heap *h_passed, gc_heap *h, size_t size, char *obj, gc_thread_data *thd);
void *gc_alloc(gc_heap_root * h, size_t size, char *obj, gc_thread_data * thd,
               int *heap_grown);
void *gc_alloc_bignum(gc_thread_data *data);
size_t gc_allocated_bytes(object obj, gc_free_list * q, gc_free_list * r);
gc_heap *gc_heap_last(gc_heap * h);

void gc_heap_create_rest(gc_heap *h, gc_thread_data *thd);
void *gc_try_alloc_rest(gc_heap * h, size_t size, char *obj, gc_thread_data * thd);
void *gc_alloc_rest(gc_heap_root * hrt, size_t size, char *obj, gc_thread_data * thd, int *heap_grown);
void gc_init_fixed_size_free_list(gc_heap *h);

//size_t gc_heap_total_size(gc_heap * h);
//size_t gc_heap_total_free_size(gc_heap *h);
//size_t gc_collect(gc_heap *h, size_t *sum_freed);
//void gc_mark(gc_heap *h, object obj);
void gc_request_mark_globals(void);
void gc_mark_globals(object globals, object global_table);
//size_t gc_sweep(gc_heap * h, size_t * sum_freed_ptr, gc_thread_data *thd);
gc_heap *gc_sweep(gc_heap * h, gc_thread_data *thd);
void gc_thr_grow_move_buffer(gc_thread_data * d);
void gc_thread_data_init(gc_thread_data * thd, int mut_num, char *stack_base,
                         long stack_size);
void gc_thread_data_free(gc_thread_data * thd);
// Prototypes for mutator/collector:
/**
 * @brief Determine if object lives on the thread's stack
 * @param low_limit Temporary object at the current "end" of the stack
 * @param thd Mutator's thread data
 * @param obj Object to inspect
 * @return True if `obj` is on the mutator's stack, false otherwise
 */
#define gc_is_stack_obj(low_limit, thd, obj) \
 (stack_overflow(((object)low_limit), ((object)obj)) && \
  stack_overflow(((object)obj), ((object)((gc_thread_data *)thd)->stack_start)))
void gc_mut_update(gc_thread_data * thd, object old_obj, object value);
void gc_mut_cooperate(gc_thread_data * thd, int buf_len);
void gc_mark_gray(gc_thread_data * thd, object obj);
void gc_mark_gray2(gc_thread_data * thd, object obj);
void gc_collector_trace();
void gc_empty_collector_stack();
void gc_handshake(gc_status_type s);
void gc_post_handshake(gc_status_type s);
void gc_wait_handshake();
void gc_start_collector();
void gc_mutator_thread_blocked(gc_thread_data * thd, object cont);
void gc_mutator_thread_runnable(gc_thread_data * thd, object result, object maybe_copied);
void Cyc_make_shared_object(void *data, object k, object obj);
#define set_thread_blocked(d, c) \
  gc_mutator_thread_blocked(((gc_thread_data *)d), (c))
/**
 * @brief Return from a blocked thread
 */
#define return_thread_runnable(d, r) \
  gc_mutator_thread_runnable(((gc_thread_data *)d), (r), NULL)
/**
 * @brief Return from a blocked thread with an object that may have been copied.
 *        If the object was copied we need to check and may need to copy it again.
 */
#define return_thread_runnable_with_obj(d, r, maybe_copied) \
  gc_mutator_thread_runnable(((gc_thread_data *)d), (r), maybe_copied)
/*
//#define do_with_blocked_thread(data, cont, result, body) \
//  set_thread_blocked((data), (cont)); \
//  body \
//  return_thread_runnable((data), (result));
*/

/**@}*/

/**
 * \defgroup gc_minor Minor GC
 * @brief Minor GC is called periodically to copy live objects off of a thread stack
 *
 */
/**@{*/

/**
 * Maximum number of args that GC will accept 
 */
#define NUM_GC_ARGS 128

/** 
 * Which way does the CPU grow its stack? 
 */
#define STACK_GROWTH_IS_DOWNWARD 1

/** 
 * Size of the stack buffer, in bytes.
 * This is used as the first generation of the GC.
 */
#define STACK_SIZE 500000

/** 
 * Do not allocate objects larger than this on the stack.
 */
#define MAX_STACK_OBJ (STACK_SIZE * 2)

/** Determine if stack has overflowed */
#if STACK_GROWTH_IS_DOWNWARD
#define stack_overflow(x,y) ((x) < (y))
#else
#define stack_overflow(x,y) ((x) > (y))
#endif

/**
 * Access an object's forwarding pointer.
 * Note this is only applicable when objects are relocated
 * during minor GC.
 * \ingroup objects
 */
#define forward(obj) (((pair_type *) obj)->pair_car)


/**
 * \defgroup gc_minor_mut Mutation table
 * @brief Mutation table to support the minor GC write barrier
 */
/**@{*/
void add_mutation(void *data, object var, int index, object value);
void clear_mutations(void *data);
/**@}*/

/**
 * \defgroup gc_minor_sh_obj Shared object write barrier
 * @brief Minor GC write barrier to ensure there are no references to stack objects from the heap.
 */
/**@{*/
object transport_stack_value(gc_thread_data *data, object var, object value, int *run_gc);
/**@}*/

/**@}*/

// END GC section
/**@}*/

/**
 * \defgroup ffi Foreign Function Interface
 */
/**@{*/
object Cyc_scm_call(gc_thread_data *parent_thd, object fnc, int argc, object *args);
object Cyc_scm_call_no_gc(gc_thread_data *parent_thd, object fnc, object arg);
/**@}*/

/**
 * \defgroup datatypes Data types
 * @brief All of the Scheme data types provided by Cyclone.
 */
/**@{*/

/**
 * \defgroup immediates Immediate objects
 *  
 *  @brief Objects that do not require memory allocation.
 *
 *  Immediate objects (also known as value types) are stored directly within
 *  the bits that would otherwise be a pointer to an object type. Since
 *  all of the data is contained in those bits, a value type is never
 *  allocated on the heap and never needs to be garbage collected,
 *  making them very efficient.
 *
 *  Depending on the underlying architecture, compiler, etc these types
 *  have extra least significant bits that can be used to mark them as
 *  values instead of objects (IE, pointer to a tagged object).
 *  On many machines, addresses are multiples of four, leaving the two
 *  least significant bits free - from lisp in small pieces.
 *
 *  The possible types are:
 *
 *  - 0x00 - pointer (an object type)
 *  - 0x01 - integer (also known as fixnum)
 *  - 0x10 - char
 */
/**@{*/

/** Maximum allowed value of a fixnum */
#define CYC_FIXNUM_MAX 1073741823

/** Minimum allowed value of a fixnum */
#define CYC_FIXNUM_MIN -1073741824

/**
 * Explicit character type now that we are using UTF-8.
 * Chars are still value types though
 */
typedef uint32_t char_type;

/**
 * Determine if an object is an integer.
 */
#define obj_is_int(x)  ((unsigned long)(x) & (unsigned long)1)

/**
 * Convert from an object to an integer.
 */
//#define obj_obj2int(n)   (((long)((ulong)(n) & ~1))/(long)(1uL<<1))
#define obj_obj2int(x) ((long)((uintptr_t)x)>>1)

/**
 * Convert from an integer to an object.
 */
//#define obj_int2obj(n) ((void *) ((((long)(n))*(long)(1uL<<1)) | 1))
#define obj_int2obj(c) ((void *)((((long)c)*2) | 1))

/**
 * Determine if the object is a char.
 */
#define obj_is_char(x)  (((unsigned long)(x) & (unsigned long)3) == 2)

/**
 * Convert from an object to a char.
 */
#define obj_obj2char(x) (char_type)((uintptr_t)(x)>>2)

/**
 * Convert from a char to an object.
 */
#define obj_char2obj(c) ((void *)((((uintptr_t)c)<<2) | 2))

/**
 * Is the given object a value type?
 */
#define is_value_type(x) ((unsigned long)(x) & (unsigned long)3)

/**
 * Is the given object an object (non-immediate) type?
 */
#define is_object_type(x) ((x != NULL) && !is_value_type(x))

/**@}*/

/**
 * \defgroup objects Objects
 * @brief Definitions and code for memory-allocated objects.
 *
 * Most Scheme data types are defined as object types.
 *
 * Each object type contains a header for garbage collection and a
 * tag that identifies the type of object, as well as any object-specific
 * fields.
 *
 * Most object types are allocated on the nursery (the C stack) and 
 * relocated to the garbage-collected heap during minor GC. It is only
 * safe for an object on the nursery to be used by the thread that
 * created it, as that object could be relocated at any time.
 */
/**@{*/

/** Function type */
typedef void (*function_type) ();

/** Variable-argument function type */
typedef void (*function_type_va) (int, object, object, object, ...);

/** Non-CPS function type */
typedef object (*inline_function_type) ();

/**
 * @brief C-variable integration type - wrapper around a Cyclone object pointer
 */
typedef struct {
  gc_header_type hdr;
  tag_type tag;
  /** Variable pointer. Note GC assumes this is a Cyclone object! */
  object *pvar;
} cvar_type;
typedef cvar_type *cvar;

/**
 * Create a new cvar in the nursery
 */
#define make_cvar(n,v) \
  cvar_type n; \
  n.hdr.mark = gc_color_red; \
  n.hdr.grayed = 0; \
  n.hdr.immutable = 0; \
  n.tag = cvar_tag; \
  n.pvar = v;

/**
 * @brief C Opaque type - a wrapper around a pointer of any type.
 *
 * Note this requires application code to free any memory
 * before an object is collected by GC.  
 */
typedef struct {
  gc_header_type hdr;
  tag_type tag;
  unsigned char collect_ptr;
  /** This pointer can be anything, GC will not collect it
      unless collect_ptr is set */
  void *ptr;
} c_opaque_type;
typedef c_opaque_type *c_opaque;

/** Create a new opaque in the nursery */
#define make_c_opaque(var, p) \
  c_opaque_type var; \
  var.hdr.mark = gc_color_red; \
  var.hdr.grayed = 0; \
  var.hdr.immutable = 0; \
  var.tag = c_opaque_tag; \
  var.collect_ptr = 0; \
  var.ptr = p;

/** Access the Opaque's pointer */
#define opaque_ptr(x) (((c_opaque)x)->ptr)

/** Access the Opaque's "collect pointer" field */
#define opaque_collect_ptr(x) (((c_opaque)x)->collect_ptr)

/**
 * @brief The mutex thread synchronization type
 *
 * Mutexes are always allocated directly on the heap.
 */
typedef struct {
  gc_header_type hdr;
  tag_type tag;
  pthread_mutex_t lock;
} mutex_type;
typedef mutex_type *mutex;

/**
 * @brief The condition variable thread synchronization type
 *
 * Condition variables are always allocated directly on the heap.
 */
typedef struct {
  gc_header_type hdr;
  tag_type tag;
  pthread_cond_t cond;
} cond_var_type;
typedef cond_var_type *cond_var;

/**
 * @brief The atomic thread synchronization type
 *
 * Atomics are always allocated directly on the heap.
 */
typedef struct {
  gc_header_type hdr;
  tag_type tag;
  object obj;
} atomic_type;
typedef atomic_type *atomic;

/** 
 * @brief The boolean type: True or False
 *
 * Booleans always refer to one of the objects `boolean_t` or `boolean_f` 
 * which are created by the runtime.
 */
typedef struct {
  gc_header_type hdr;
  const tag_type tag;
  const char *desc;
} boolean_type;
typedef boolean_type *boolean;

#define boolean_desc(x) (((boolean_type *) x)->desc)

#define make_boolean(x) (x ? boolean_t : boolean_f)

/**
 * @brief Symbols are similar to strings, but only one instance of each
 * unique symbol is created, so comparisons are O(1).
 *
 * A thread-safe symbol table is used at runtime to store all of
 * the program's symbols.
 */
typedef struct {
  gc_header_type hdr;
  const tag_type tag;
  const char *desc;
} symbol_type;
typedef symbol_type *symbol;

#define symbol_desc(x) (((symbol_type *) x)->desc)

#define defsymbol(name) \
static object quote_##name = NULL;

/* Define numeric types */

/**
 * @brief Deprecated - boxed integers
 *
 * The integer object type is deprecated, integers should be stored using value types instead.
 * This is only still here because it is used internally by the runtime.
 */
typedef struct {
  gc_header_type hdr;
  tag_type tag;
  int value;
  int padding;                  // Prevent mem corruption if sizeof(int) < sizeof(ptr)
} integer_type;

/**
 * @brief Exact integer of unlimited precision.
 *
 * The backing store is the `mp_int` data type from LibTomMath.
 * 
 * Note memory for `mp_int` is allocated via `malloc`, so bignums must
 * always be allocated on Cyclone's heap.
 */
typedef struct {
  gc_header_type hdr;
  tag_type tag;
  mp_int bn;
} bignum_type;

/** Allocate a new bignum on the heap */
#define alloc_bignum(data, p) \
  bignum_type *p = gc_alloc_bignum((gc_thread_data *)data);

/** Helper for handling return value of a bignum function call */
#define BIGNUM_CALL(x) { \
  int __bn_mp_rv; \
  if ((__bn_mp_rv = (x)) != MP_OKAY) { \
    fprintf(stderr, "Error calling bignum function: %s\n", \
      mp_error_to_string(__bn_mp_rv)); \
    exit(1); \
  } \
}

/**
 * @brief Complex number
 */
typedef struct {
  gc_header_type hdr;
  tag_type tag;
  double complex value;
} complex_num_type;

/** Create a new complex number in the nursery */
#define make_complex_num(n,r,i) \
  complex_num_type n; \
  n.hdr.mark = gc_color_red; \
  n.hdr.grayed = 0; \
  n.tag = complex_num_tag; \
  n.value = (r + (i * I));

#define alloca_complex_num(n,r,i) \
  complex_num_type *n = alloca(sizeof(complex_num_type)); \
  n->hdr.mark = gc_color_red; \
  n->hdr.grayed = 0; \
  n->tag = complex_num_tag; \
  n->value = (r + (i * I));

/** Assign given complex value to the given complex number object pointer */
#define assign_complex_num(pobj,v) \
  ((complex_num_type *)pobj)->hdr.mark = gc_color_red; \
  ((complex_num_type *)pobj)->hdr.grayed = 0; \
  ((complex_num_type *)pobj)->tag = complex_num_tag; \
  complex_num_value(pobj) = v;

/**
 * @brief Double-precision floating point type, also known as a flonum.
 */
typedef struct {
  gc_header_type hdr;
  tag_type tag;
  double value;
} double_type;

/** Create a new double in the nursery */
#define make_double(n,v) \
  double_type n; \
  n.hdr.mark = gc_color_red; \
  n.hdr.grayed = 0; \
  n.tag = double_tag; \
  n.value = v;

#define alloca_double(n,v) \
  double_type *n = alloca(sizeof(double_type)); \
  n->hdr.mark = gc_color_red; \
  n->hdr.grayed = 0; \
  n->tag = double_tag; \
  n->value = v;

/** Assign given double value to the given double object pointer */
#define assign_double(pobj,v) \
  ((double_type *)pobj)->hdr.mark = gc_color_red; \
  ((double_type *)pobj)->hdr.grayed = 0; \
  ((double_type *)pobj)->tag = double_tag; \
  double_value(pobj) = v;

/** Access the integer_type integer value directly */
#define integer_value(x) (((integer_type *) x)->value)

/** Access the double directly */
#define double_value(x) (((double_type *) x)->value)

/** Access a bignum's `mp_int` directly */
#define bignum_value(x) (((bignum_type *) x)->bn)

/** Access the complex number directly */
#define complex_num_value(x) (((complex_num_type *) x)->value)

/**
 * This enumeration complements the comparison types from LibTomMath,
 * and provides constants for each of the comparison operators.
 */
typedef enum {
    CYC_BN_LTE = -2
  , CYC_BN_LT = MP_LT
  , CYC_BN_EQ = MP_EQ
  , CYC_BN_GT = MP_GT
  , CYC_BN_GTE = 2
} bn_cmp_type;

/** 
 * @brief The string type 
 */
typedef struct {
  gc_header_type hdr;
  tag_type tag;
  int num_cp;
  int len;
  char *str;
} string_type;

// TODO: below macros are obsolete, need new ones that populate num_cp and
// raise an error if an invalid UTF-8 char is detected

/** Create a new string in the nursery */
#define make_string(cs, s) string_type cs; \
{ int len = strlen(s); \
  cs.hdr.mark = gc_color_red; \
  cs.hdr.grayed = 0; \
  cs.hdr.immutable = 0; \
  cs.tag = string_tag; \
  cs.num_cp = len; \
  cs.len = len; \
  cs.str = alloca(sizeof(char) * (len + 1)); \
  memcpy(cs.str, s, len + 1);}

/** 
 * Create a new string with the given length 
 * (so it does not need to be computed) 
 */
#define make_string_with_len(cs, s, length) string_type cs;  \
{ int len = length; \
  cs.hdr.mark = gc_color_red; \
  cs.hdr.grayed = 0; \
  cs.hdr.immutable = 0; \
  cs.tag = string_tag; cs.len = len; \
  cs.num_cp = len; \
  cs.str = alloca(sizeof(char) * (len + 1)); \
  memcpy(cs.str, s, len); \
  cs.str[len] = '\0';}

/**
 * Create a string object using the given C string and length.
 * No allocation is done for the given C string.
 */
#define make_string_noalloc(cs, s, length) string_type cs; \
{ cs.hdr.mark = gc_color_red; cs.hdr.grayed = 0; cs.hdr.immutable = 0; \
  cs.tag = string_tag; cs.len = length; \
  cs.num_cp = length; \
  cs.str = s; }

/** Create a new string in the nursery */
#define make_utf8_string(data, cs, s) string_type cs; \
{ int len = strlen(s); \
  cs.hdr.mark = gc_color_red; \
  cs.hdr.grayed = 0; \
  cs.hdr.immutable = 0; \
  cs.tag = string_tag; \
  cs.num_cp = Cyc_utf8_count_code_points((uint8_t *)s); \
  if (cs.num_cp < 0) { \
    Cyc_rt_raise_msg(data, "Invalid UTF-8 characters in string"); \
  } \
  cs.len = len; \
  cs.str = alloca(sizeof(char) * (len + 1)); \
  memcpy(cs.str, s, len + 1);}

/** 
 * Create a new string with the given length 
 * (so it does not need to be computed) 
 */
#define make_utf8_string_with_len(cs, s, length, num_code_points) string_type cs;  \
{ int len = length; \
  cs.hdr.mark = gc_color_red; \
  cs.hdr.grayed = 0; \
  cs.hdr.immutable = 0; \
  cs.tag = string_tag; cs.len = len; \
  cs.num_cp = num_code_points; \
  cs.str = alloca(sizeof(char) * (len + 1)); \
  memcpy(cs.str, s, len); \
  cs.str[len] = '\0';}

/**
 * Create a string object using the given C string and length.
 * No allocation is done for the given C string.
 */
#define make_utf8_string_noalloc(cs, s, length) string_type cs; \
{ cs.hdr.mark = gc_color_red; cs.hdr.grayed = 0; cs.hdr.immutable = 0; \
  cs.tag = string_tag; cs.len = length; \
  cs.num_cp = length; \
  cs.str = s; }

/**
 * Allocate a new string, either on the stack or heap depending upon size
 */
#define alloc_string(_data, _s, _len, _num_cp) \
  if (_len >= MAX_STACK_OBJ) { \
    int heap_grown; \
    _s = gc_alloc(((gc_thread_data *)data)->heap,  \
                 sizeof(string_type) + _len + 1, \
                 boolean_f, /* OK to populate manually over here */ \
                 (gc_thread_data *)data,  \
                 &heap_grown); \
    ((string_type *) _s)->hdr.mark = ((gc_thread_data *)data)->gc_alloc_color; \
    ((string_type *) _s)->hdr.grayed = 0; \
    ((string_type *) _s)->hdr.immutable = 0; \
    ((string_type *) _s)->tag = string_tag; \
    ((string_type *) _s)->len = _len; \
    ((string_type *) _s)->num_cp = _num_cp; \
    ((string_type *) _s)->str = (((char *)_s) + sizeof(string_type)); \
  } else { \
    _s = alloca(sizeof(string_type)); \
    ((string_type *)_s)->hdr.mark = gc_color_red;  \
    ((string_type *)_s)->hdr.grayed = 0; \
    ((string_type *)_s)->hdr.immutable = 0; \
    ((string_type *)_s)->tag = string_tag;  \
    ((string_type *)_s)->len = _len; \
    ((string_type *)_s)->num_cp = _num_cp; \
    ((string_type *)_s)->str = alloca(sizeof(char) * (_len + 1)); \
  }

#define alloc_bytevector(_data, _bv, _len) \
  if (_len >= MAX_STACK_OBJ) { \
    int heap_grown; \
    _bv = gc_alloc(((gc_thread_data *)data)->heap, \
                  sizeof(bytevector_type) + _len, \
                  boolean_f, /* OK to populate manually over here */ \
                  (gc_thread_data *)data, \
                  &heap_grown); \
    ((bytevector) _bv)->hdr.mark = ((gc_thread_data *)data)->gc_alloc_color; \
    ((bytevector) _bv)->hdr.grayed = 0; \
    ((bytevector) _bv)->hdr.immutable = 0; \
    ((bytevector) _bv)->tag = bytevector_tag; \
    ((bytevector) _bv)->len = _len; \
    ((bytevector) _bv)->data = (char *)(((char *)_bv) + sizeof(bytevector_type)); \
  } else { \
    _bv = alloca(sizeof(bytevector_type)); \
    ((bytevector) _bv)->hdr.mark = gc_color_red; \
    ((bytevector) _bv)->hdr.grayed = 0; \
    ((bytevector) _bv)->hdr.immutable = 0; \
    ((bytevector) _bv)->tag = bytevector_tag; \
    ((bytevector) _bv)->len = _len; \
    ((bytevector) _bv)->data = alloca(sizeof(char) * _len); \
  }

/** Get the length of a string, in characters (code points) */
#define string_num_cp(x) (((string_type *) x)->num_cp)

/** Get the length of a string, in bytes */
#define string_len(x) (((string_type *) x)->len)

/** Get a string object's C string */
#define string_str(x) (((string_type *) x)->str)

/* I/O types */

// TODO: FILE* may not be good enough
//       consider http://stackoverflow.com/questions/6206893/how-to-implement-char-ready-in-c
// TODO: a simple wrapper around FILE may not be good enough long-term
// TODO: how exactly mode will be used. need to know r/w, bin/txt

/**
 * @brief The port object type 
 */
typedef struct {
  gc_header_type hdr;
  tag_type tag;
  void *unused; // Protect against forwarding pointer, ideally would not be needed.
  FILE *fp;
  int mode;
  unsigned char flags;
  unsigned int line_num;
  unsigned int col_num;
  unsigned int buf_idx;
  unsigned int tok_start; // Start of token in mem_buf (end is unknown yet)
  unsigned int tok_end; // End of token in tok_buf (start is tok_buf[0])
  char *tok_buf; // Alternative buffer for tokens
  size_t tok_buf_len; 
  char *mem_buf;
  size_t mem_buf_len;
  unsigned short read_len;
  char *str_bv_in_mem_buf;
  size_t str_bv_in_mem_buf_len;
} port_type;

#define CYC_BINARY_PORT_FLAG 0x10

#define CYC_IO_BUF_LEN 1024

/** Create a new port object in the nursery */
#define make_port(p,f,m) \
  port_type p; \
  p.hdr.mark = gc_color_red; \
  p.hdr.grayed = 0; \
  p.hdr.immutable = 0; \
  p.tag = port_tag; \
  p.fp = f; \
  p.mode = m; \
  p.flags = 0; \
  p.line_num = 1; \
  p.col_num = 1; \
  p.buf_idx = 0; \
  p.tok_start = 0; \
  p.tok_end = 0; \
  p.tok_buf = NULL; \
  p.tok_buf_len = 0; \
  p.mem_buf = NULL; \
  p.mem_buf_len = 0; \
  p.str_bv_in_mem_buf = NULL; \
  p.str_bv_in_mem_buf_len = 0; \
  p.read_len = 1;

#define make_input_port(p,f,rl) \
  port_type p; \
  p.hdr.mark = gc_color_red; \
  p.hdr.grayed = 0; \
  p.hdr.immutable = 0; \
  p.tag = port_tag; \
  p.fp = f; \
  p.mode = 1; \
  p.flags = 1; \
  p.line_num = 1; \
  p.col_num = 1; \
  p.buf_idx = 0; \
  p.tok_start = 0; \
  p.tok_end = 0; \
  p.tok_buf = malloc(CYC_IO_BUF_LEN); \
  p.tok_buf_len = CYC_IO_BUF_LEN; \
  p.mem_buf = malloc(CYC_IO_BUF_LEN); \
  p.mem_buf_len = 0; \
  p.str_bv_in_mem_buf = NULL; \
  p.str_bv_in_mem_buf_len = 0; \
  p.read_len = rl;

/**
 * @brief Vector type 
 */
typedef struct {
  gc_header_type hdr;
  tag_type tag;
  int num_elements;
  object *elements;
} vector_type;
typedef vector_type *vector;

typedef struct { vector_type v; object arr[2]; } vector_2_type;
typedef struct { vector_type v; object arr[3]; } vector_3_type;
typedef struct { vector_type v; object arr[4]; } vector_4_type;
typedef struct { vector_type v; object arr[5]; } vector_5_type;

/** Create a new vector in the nursery */
#define make_empty_vector(v) \
  vector_type v; \
  v.hdr.mark = gc_color_red; \
  v.hdr.grayed = 0; \
  v.hdr.immutable = 0; \
  v.tag = vector_tag; \
  v.num_elements = 0; \
  v.elements = NULL;

#define alloca_empty_vector(v) \
  vector_type *v = alloca(sizeof(vector_type)); \
  v->hdr.mark = gc_color_red; \
  v->hdr.grayed = 0; \
  v->hdr.immutable = 0; \
  v->tag = vector_tag; \
  v->num_elements = 0; \
  v->elements = NULL;

/**
 * @brief Bytevector type 
 *
 * Bytevectors are similar to regular vectors, but instead of containing
 * objects, each bytevector member is a 8-bit integer.
 */
typedef struct {
  gc_header_type hdr;
  tag_type tag;
  int len;
  char *data;
} bytevector_type;
typedef bytevector_type *bytevector;

/** Create a new bytevector in the nursery */
#define make_empty_bytevector(v) \
  bytevector_type v; \
  v.hdr.mark = gc_color_red; \
  v.hdr.grayed = 0; \
  v.hdr.immutable = 0; \
  v.tag = bytevector_tag; \
  v.len = 0; \
  v.data = NULL;

#define alloca_empty_bytevector(v) \
  bytevector_type *v = alloca(sizeof(bytevector_type)); \
  v->hdr.mark = gc_color_red; \
  v->hdr.grayed = 0; \
  v->hdr.immutable = 0; \
  v->tag = bytevector_tag; \
  v->len = 0; \
  v->data = NULL;

/**
 * @brief The pair (cons) type.
 *
 * Contrary to popular belief, Scheme does not actually have a list type.
 *
 * Instead there is a pair object composed two objects, the `car` and `cdr`.
 * A list can be created by storing values in the `car` and a pointer to
 * the rest of the list in `cdr`. A `NULL` in the `cdr` indicates the end
 * of a list.
 */
typedef struct {
  gc_header_type hdr;
  tag_type tag;
  object pair_car;
  object pair_cdr;
} pair_type;
typedef pair_type *list;
typedef pair_type *pair;

/** Create a new pair in the nursery */
#define make_pair(n,a,d) \
  pair_type n; \
  n.hdr.mark = gc_color_red; \
  n.hdr.grayed = 0; \
  n.hdr.immutable = 0; \
  n.tag = pair_tag; \
  n.pair_car = a; \
  n.pair_cdr = d;

#define alloca_pair(n,a,d) \
  pair_type *n = alloca(sizeof(pair_type)); \
  n->hdr.mark = gc_color_red; \
  n->hdr.grayed = 0; \
  n->hdr.immutable = 0; \
  n->tag = pair_tag; \
  n->pair_car = a; \
  n->pair_cdr = d;

#define set_pair(n,a,d) \
  n->hdr.mark = gc_color_red; \
  n->hdr.grayed = 0; \
  n->hdr.immutable = 0; \
  n->tag = pair_tag; \
  n->pair_car = a; \
  n->pair_cdr = d;

#define set_pair_as_expr(n,a,d) \
 (((pair)(n))->hdr.mark = gc_color_red, \
  ((pair)(n))->hdr.grayed = 0, \
  ((pair)(n))->hdr.immutable = 0, \
  ((pair)(n))->tag = pair_tag, \
  ((pair)(n))->pair_car = a, \
  ((pair)(n))->pair_cdr = d, \
  (n))

//typedef list_1_type pair_type;
typedef struct { pair_type a; pair_type b; } list_2_type;
typedef struct { pair_type a; pair_type b; pair_type c;} list_3_type;
typedef struct { pair_type a; pair_type b; pair_type c; pair_type d;} list_4_type;

/**
 * Create a pair with a single value. 
 * This is useful to create an object that can be modified.
 */
#define make_cell(n,a) make_pair(n,a,NULL)
#define alloca_cell(n,a) alloca_pair(n,a,NULL)
#define set_cell_as_expr(n,a) set_pair_as_expr(n,a,NULL)

/**
 * \defgroup objects_unsafe_cxr Unsafe pair access macros
 * @brief Macros for fast - but unsafe - pair access
 *
 */
/**@{*/
/** Unsafely access a pair's `car` */
#define car(x)    (((pair_type *) x)->pair_car)
/** Unsafely access a pair's `cdr` */
#define cdr(x)    (((pair_type *) x)->pair_cdr)
#define caar(x)   (car(car(x)))
#define cadr(x)   (car(cdr(x)))
#define cdar(x)   (cdr(car(x)))
#define cddr(x)   (cdr(cdr(x)))
#define caaar(x)  (car(car(car(x))))
#define caadr(x)  (car(car(cdr(x))))
#define cadar(x)  (car(cdr(car(x))))
#define caddr(x)  (car(cdr(cdr(x))))
#define cdaar(x)  (cdr(car(car(x))))
#define cdadr(x)  (cdr(car(cdr(x))))
#define cddar(x)  (cdr(cdr(car(x))))
#define cdddr(x)  (cdr(cdr(cdr(x))))
#define caaaar(x) (car(car(car(car(x)))))
#define caaadr(x) (car(car(car(cdr(x)))))
#define caadar(x) (car(car(cdr(car(x)))))
#define caaddr(x) (car(car(cdr(cdr(x)))))
#define cadaar(x) (car(cdr(car(car(x)))))
#define cadadr(x) (car(cdr(car(cdr(x)))))
#define caddar(x) (car(cdr(cdr(car(x)))))
#define cadddr(x) (car(cdr(cdr(cdr(x)))))
#define cdaaar(x) (cdr(car(car(car(x)))))
#define cdaadr(x) (cdr(car(car(cdr(x)))))
#define cdadar(x) (cdr(car(cdr(car(x)))))
#define cdaddr(x) (cdr(car(cdr(cdr(x)))))
#define cddaar(x) (cdr(cdr(car(car(x)))))
#define cddadr(x) (cdr(cdr(car(cdr(x)))))
#define cdddar(x) (cdr(cdr(cdr(car(x)))))
#define cddddr(x) (cdr(cdr(cdr(cdr(x)))))
/**@}*/

/**
 * \defgroup objects_safe_cxr Safe pair access macros
 * @brief Macros for safe pair access
 *
 */
/**@{*/
#define Cyc_caar(d, x) (Cyc_car(d, Cyc_car(d, x)))
#define Cyc_cadr(d, x) (Cyc_car(d, Cyc_cdr(d, x)))
#define Cyc_cdar(d, x) (Cyc_cdr(d, Cyc_car(d, x)))
#define Cyc_cddr(d, x) (Cyc_cdr(d, Cyc_cdr(d, x)))
#define Cyc_caaar(d, x) (Cyc_car(d, Cyc_car(d, Cyc_car(d, x))))
#define Cyc_caadr(d, x) (Cyc_car(d, Cyc_car(d, Cyc_cdr(d, x))))
#define Cyc_cadar(d, x) (Cyc_car(d, Cyc_cdr(d, Cyc_car(d, x))))
#define Cyc_caddr(d, x) (Cyc_car(d, Cyc_cdr(d, Cyc_cdr(d, x))))
#define Cyc_cdaar(d, x) (Cyc_cdr(d, Cyc_car(d, Cyc_car(d, x))))
#define Cyc_cdadr(d, x) (Cyc_cdr(d, Cyc_car(d, Cyc_cdr(d, x))))
#define Cyc_cddar(d, x) (Cyc_cdr(d, Cyc_cdr(d, Cyc_car(d, x))))
#define Cyc_cdddr(d, x) (Cyc_cdr(d, Cyc_cdr(d, Cyc_cdr(d, x))))
#define Cyc_caaaar(d, x) (Cyc_car(d, Cyc_car(d, Cyc_car(d, Cyc_car(d, x)))))
#define Cyc_caaadr(d, x) (Cyc_car(d, Cyc_car(d, Cyc_car(d, Cyc_cdr(d, x)))))
#define Cyc_caadar(d, x) (Cyc_car(d, Cyc_car(d, Cyc_cdr(d, Cyc_car(d, x)))))
#define Cyc_caaddr(d, x) (Cyc_car(d, Cyc_car(d, Cyc_cdr(d, Cyc_cdr(d, x)))))
#define Cyc_cadaar(d, x) (Cyc_car(d, Cyc_cdr(d, Cyc_car(d, Cyc_car(d, x)))))
#define Cyc_cadadr(d, x) (Cyc_car(d, Cyc_cdr(d, Cyc_car(d, Cyc_cdr(d, x)))))
#define Cyc_caddar(d, x) (Cyc_car(d, Cyc_cdr(d, Cyc_cdr(d, Cyc_car(d, x)))))
#define Cyc_cadddr(d, x) (Cyc_car(d, Cyc_cdr(d, Cyc_cdr(d, Cyc_cdr(d, x)))))
#define Cyc_cdaaar(d, x) (Cyc_cdr(d, Cyc_car(d, Cyc_car(d, Cyc_car(d, x)))))
#define Cyc_cdaadr(d, x) (Cyc_cdr(d, Cyc_car(d, Cyc_car(d, Cyc_cdr(d, x)))))
#define Cyc_cdadar(d, x) (Cyc_cdr(d, Cyc_car(d, Cyc_cdr(d, Cyc_car(d, x)))))
#define Cyc_cdaddr(d, x) (Cyc_cdr(d, Cyc_car(d, Cyc_cdr(d, Cyc_cdr(d, x)))))
#define Cyc_cddaar(d, x) (Cyc_cdr(d, Cyc_cdr(d, Cyc_car(d, Cyc_car(d, x)))))
#define Cyc_cddadr(d, x) (Cyc_cdr(d, Cyc_cdr(d, Cyc_car(d, Cyc_cdr(d, x)))))
#define Cyc_cdddar(d, x) (Cyc_cdr(d, Cyc_cdr(d, Cyc_cdr(d, Cyc_car(d, x)))))
#define Cyc_cddddr(d, x) (Cyc_cdr(d, Cyc_cdr(d, Cyc_cdr(d, Cyc_cdr(d, x)))))
/**@}*/

/* Closure types */

/** @brief Closure for a macro */
typedef struct {
  gc_header_type hdr;
  tag_type tag;
  function_type fn;
  int num_args;
} macro_type;

/** @brief A closed-over function with no variables */
typedef struct {
  gc_header_type hdr;
  tag_type tag;
  function_type fn;
  int num_args;
} closure0_type;
/** @brief A closed-over function with one variable */
typedef struct {
  gc_header_type hdr;
  tag_type tag;
  function_type fn;
  int num_args;
  object element;
} closure1_type;
/** @brief A closed-over function with zero or more closed-over variables */
typedef struct {
  gc_header_type hdr;
  tag_type tag;
  function_type fn;
  int num_args;
  int num_elements;
  object *elements;
} closureN_type;

typedef closure0_type *closure0;
typedef closure1_type *closure1;
typedef closureN_type *closureN;
typedef closure0_type *closure;
typedef closure0_type *macro;

#define mmacro(c,f) \
  macro_type c; \
  c.hdr.mark = gc_color_red; \
  c.hdr.grayed = 0; \
  c.tag = macro_tag; \
  c.fn = f; \
  c.num_args = -1;

#define mclosure0(c, f) \
 static closure0_type c = { .hdr.mark = gc_color_red, .hdr.grayed = 0, .tag = closure0_tag, .fn = f, .num_args = -1 }; /* TODO: need a new macro that initializes num_args */

/*
#define mclosure0(c,f) \
  closure0_type c; \
  c.hdr.mark = gc_color_red; \
  c.hdr.grayed = 0; \
  c.tag = closure0_tag; \
  c.fn = f; \
  c.num_args = -1;
*/

#define maclosure0(c,f,na) \
  closure0_type c; \
  c.hdr.mark = gc_color_red; \
  c.hdr.grayed = 0; \
  c.tag = closure0_tag; \
  c.fn = f; \
  c.num_args = na;

#define mclosure1(c,f,a) \
  closure1_type c; \
  c.hdr.mark = gc_color_red; \
  c.hdr.grayed = 0; \
  c.tag = closure1_tag; \
  c.fn = f; \
  c.num_args = -1; \
  c.element = a;

/** 
 * @brief A function built into the runtime.
 */
typedef struct {
  gc_header_type hdr;
  tag_type tag;
  const char *desc;
  function_type fn;
} primitive_type;
typedef primitive_type *primitive;

#define defprimitive(name, desc, fnc) \
static primitive_type name##_primitive = {primitive_tag, #desc, fnc}; \
static const object primitive_##name = &name##_primitive

#define prim(x) (x && ((primitive)x)->tag == primitive_tag)
#define prim_name(x) (((primitive_type *) x)->desc)

/**
 * @brief A union of all the constant-size objects.
 *
 * This type is used internally to (for example) pass a pointer
 * to an inline function that might need to use it for an allocation.
 */
typedef union {
  boolean_type boolean_t;
  pair_type pair_t;
  symbol_type symbol_t;
  primitive_type primitive_t;
  integer_type integer_t;
  double_type double_t;
  bignum_type bignum_t;
  complex_num_type complex_num_t;
} common_type;

#define return_copy(ptr, o) \
{ \
  tag_type t; \
  object obj = o; \
  if (!is_object_type(obj)) \
    return obj; \
  t = type_of(obj); \
  if (t == double_tag) { \
    ((common_type *)ptr)->double_t.hdr.mark = gc_color_red; \
    ((common_type *)ptr)->double_t.hdr.grayed = 0; \
    ((common_type *)ptr)->double_t.tag = double_tag; \
    ((common_type *)ptr)->double_t.value = double_value(obj); \
    return ptr; \
  } else { \
    return obj; \
  } \
}

/**@}*/
/**@}*/

typedef struct vpbuffer_t vpbuffer;
struct vpbuffer_t {
  void **buf;
  int len;
  int count;
};

vpbuffer *vp_create(void);
void vp_add(vpbuffer *v, void *obj);

/* Utility functions */
void **vpbuffer_realloc(void **buf, int *len);
void **vpbuffer_add(void **buf, int *len, int i, void *obj);
void vpbuffer_free(void **buf);

/* Bignum utility functions */
int Cyc_bignum_cmp(bn_cmp_type type, object x, int tx, object y, int ty);
void Cyc_int2bignum(int n, mp_int *bn);

/* Remaining GC prototypes that require objects to be defined */
void *gc_alloc_from_bignum(gc_thread_data *data, bignum_type *src);

/**
 * Do a minor GC
 * \ingroup gc_minor
 */
int gc_minor(void *data, object low_limit, object high_limit, closure cont,
             object * args, int num_args);

void Cyc_import_shared_object(void *data, object cont, object filename, object entry_pt_fnc);
#endif                          /* CYCLONE_TYPES_H */

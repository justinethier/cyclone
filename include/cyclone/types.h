/** 
 * Cyclone Scheme
 * Copyright (c) 2014, Justin Ethier
 * All rights reserved.
 *
 * This file contains C types used by compiled programs.
 */

#ifndef CYCLONE_TYPES_H
#define CYCLONE_TYPES_H

#include <stdlib.h>
#include <stdio.h>
#include <time.h>
#include <setjmp.h>
#include <stdarg.h>
#include <string.h>
#include <math.h>
#include <pthread.h>

// Maximum number of args that GC will accept
#define NUM_GC_ANS 128

// Which way does the CPU grow its stack?
#define STACK_GROWTH_IS_DOWNWARD 1

// Size of the stack buffer, in bytes.
// This is used as the first generation of the GC.
#define STACK_SIZE 500000

// Size of a "page" on the heap (the second generation), in bytes.
#define HEAP_SIZE (16 * 1024 * 1024)

/////////////////////////////
// Major GC tuning parameters

// Start GC cycle if % heap space free below this percentage
#define GC_COLLECTION_THRESHOLD 0.05

// After major GC, grow the heap so at least this percentage is free
#define GC_FREE_THRESHOLD 0.40
// END GC tuning
/////////////////////////////

// Number of functions to save for printing call history
#define MAX_STACK_TRACES 10

// Show diagnostic information for the GC when program terminates
#define DEBUG_SHOW_DIAG 0

// GC debugging flags
#define GC_DEBUG_TRACE 0
#define GC_DEBUG_VERBOSE 0

/* Additional runtime checking of the GC system.
   This is here because these checks should not be
   necessary if GC is working correctly. */
#define GC_SAFETY_CHECKS 1

// General constants
#define NANOSECONDS_PER_MILLISECOND 1000000

/* Define general object type. */
typedef void *object;

/* Threading */
typedef enum { CYC_THREAD_STATE_NEW
             , CYC_THREAD_STATE_RUNNABLE
             , CYC_THREAD_STATE_BLOCKED
             , CYC_THREAD_STATE_BLOCKED_COOPERATING
             , CYC_THREAD_STATE_TERMINATED
             } cyc_thread_state_type;

/* Thread data structures */
typedef struct gc_thread_data_t gc_thread_data;
struct gc_thread_data_t {
// TODO:
//  pthread_t *thread;
  cyc_thread_state_type thread_state;
  // Data needed to initiate stack-based minor GC
  char *stack_start;
  char *stack_limit;
  // Minor GC write barrier
  void *mutations;
  // List of objects moved to heap during minor GC
  void **moveBuf;
  int moveBufLen;
  // Need the following to perform longjmp's
  //int mutator_num;
  jmp_buf *jmp_start;
  // After longjmp, pick up execution using continuation/arguments
  object gc_cont;
  object *gc_args;
  short gc_num_args;
  // Data needed for heap GC
  int gc_alloc_color;
  int gc_status;
  int last_write;
  int last_read;
  int pending_writes;
  void **mark_buffer;
  int mark_buffer_len;
  pthread_mutex_t lock;
  // Data needed for call history
  char **stack_traces;
  int stack_trace_idx;
  char *stack_prev_frame;
  // Exception handler stack
  object exception_handler_stack;
};

/* GC data structures */

typedef enum { HEAP_SM = 0
             , HEAP_MED
             , HEAP_REST } cached_heap_type;

typedef struct gc_free_list_t gc_free_list;
struct gc_free_list_t {
  unsigned int size;
  gc_free_list *next;
};

typedef struct gc_heap_t gc_heap;
struct gc_heap_t {
  unsigned int size;
  unsigned int chunk_size; // 0 for any size, other and heap will only alloc chunks of that size
  unsigned int max_size;
  //unsigned int free_size;
  gc_free_list *free_list;
  gc_heap *next; // TBD, linked list is not very efficient, but easy to work with as a start
  char *data;
};

typedef struct gc_heap_root_t gc_heap_root;
struct gc_heap_root_t {
  gc_heap *small_obj_heap;
  gc_heap *medium_obj_heap;
  gc_heap *heap;
};

typedef struct gc_header_type_t gc_header_type;
struct gc_header_type_t {
  unsigned int mark; // mark bits (TODO: only need 2, reduce size of type?)
  unsigned char grayed; // stack object to be grayed when moved to heap
};
#define mark(x) (((list) x)->hdr.mark)
#define grayed(x) (((list) x)->hdr.grayed)

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

// Constant colors are defined here.
// The mark/clear colors are defined in the gc module because
// the collector swaps their values as an optimization.
#define gc_color_red  0 // Memory not to be GC'd, such as on the stack
#define gc_color_blue 2 // Unallocated memory

/* Define size of object tags */
typedef long tag_type;

/* Determine if stack has overflowed */
#if STACK_GROWTH_IS_DOWNWARD
#define stack_overflow(x,y) ((x) < (y))
#else
#define stack_overflow(x,y) ((x) > (y))
#endif

/* Define object tag values. Could be an enum...
   Remember to update tag_names in runtime.c when adding new tags */
#define cons_tag 0
#define symbol_tag 1
#define forward_tag 2
#define closure0_tag 3
#define closure1_tag 4
#define closureN_tag 8
#define integer_tag 9
#define double_tag 10
#define string_tag 11
#define primitive_tag 12
#define eof_tag 13
#define port_tag 14
#define boolean_tag 15
#define cvar_tag 16
#define vector_tag 17
#define macro_tag 18
#define mutex_tag 19
#define cond_var_tag 20
#define bytevector_tag 21
#define c_opaque_tag 22

#define nil NULL
#define eq(x,y) (x == y)
#define nullp(x) (x == NULL)

#define type_of(x) (((list) x)->tag)
#define forward(x) (((list) x)->cons_car)

/** Define value types. 
 *  Depending on the underlying architecture, compiler, etc these types
 *  have extra least significant bits that can be used to mark them as
 *  values instead of objects (IE, pointer to a tagged object).
 *  On many machines, addresses are multiples of four, leaving the two
 *  least significant bits free - from lisp in small pieces.
 *
 *  Types:
 *  0x00 - pointer (an object type)
 *  0x01 - integer (in progress)
 *  0x10 - char
 */

#define obj_is_int(x)  ((unsigned long)(x) & (unsigned long)1)
#define obj_obj2int(x) ((long)(x)>>1)
#define obj_int2obj(c) ((void *)((((long)c)<<1) | 1))

#define obj_is_char(x)  (((unsigned long)(x) & (unsigned long)3) == 2)
#define obj_obj2char(x) (char)((long)(x)>>2)
#define obj_char2obj(c) ((void *)((((unsigned long)c)<<2) | 2))

#define is_value_type(x) ((unsigned long)(x) & (unsigned long)3)
#define is_object_type(x) (x && !is_value_type(x))

/* Function type */

typedef void (*function_type)();
typedef void (*function_type_va)(int, object, object, object, ...);

/* Define C-variable integration type */
typedef struct {gc_header_type hdr; tag_type tag; object *pvar;} cvar_type;
typedef cvar_type *cvar;
#define make_cvar(n,v) cvar_type n; n.hdr.mark = gc_color_red; n.hdr.grayed = 0; n.tag = cvar_tag; n.pvar = v;

/* C Opaque type - a wrapper around a pointer of any type.
   Note this requires application code to free any memory
   before an object is collected by GC.  */
typedef struct {gc_header_type hdr; tag_type tag; void *ptr;} c_opaque_type;
typedef c_opaque_type *c_opaque;
#define make_c_opaque(var, ptr) c_opaque_type var; var.hdr.mark = gc_color_red; var.hdr.grayed = 0; var.tag = c_opaque_tag; var.ptr = ptr;

#define opaque_ptr(x) (((c_opaque)x)->ptr)

/* Define mutex type */
typedef struct {gc_header_type hdr; tag_type tag; pthread_mutex_t lock;} mutex_type;
typedef mutex_type *mutex;

/* Define condition variable type */
typedef struct {gc_header_type hdr; tag_type tag; pthread_cond_t cond;} cond_var_type;
typedef cond_var_type *cond_var;

/* Define boolean type. */
typedef struct {gc_header_type hdr; const tag_type tag; const char *pname;} boolean_type;
typedef boolean_type *boolean;

#define boolean_pname(x) (((boolean_type *) x)->pname)

/* Define symbol type. */

typedef struct {gc_header_type hdr; const tag_type tag; const char *pname; object plist;} symbol_type;
typedef symbol_type *symbol;

#define symbol_pname(x) (((symbol_type *) x)->pname)
#define symbol_plist(x) (((symbol_type *) x)->plist)

#define defsymbol(name) \
static object quote_##name = nil;

/* Define numeric types */
typedef struct {gc_header_type hdr; tag_type tag; int value; int padding;} integer_type;
#define make_int(n,v) integer_type n; n.hdr.mark = gc_color_red; n.hdr.grayed = 0; n.tag = integer_tag; n.value = v;
typedef struct {gc_header_type hdr; tag_type tag; double value;} double_type;
#define make_double(n,v) double_type n; n.hdr.mark = gc_color_red; n.hdr.grayed = 0; n.tag = double_tag; n.value = v;

#define integer_value(x) (((integer_type *) x)->value)
#define double_value(x) (((double_type *) x)->value)

/* Define string type */
typedef struct {gc_header_type hdr; tag_type tag; int len; char *str;} string_type;
#define make_string(cs, s) string_type cs; \
{ int len = strlen(s); cs.tag = string_tag; cs.len = len; cs.hdr.mark = gc_color_red; cs.hdr.grayed = 0; \
  cs.str = alloca(sizeof(char) * (len + 1)); \
  memcpy(cs.str, s, len + 1);}
#define make_string_with_len(cs, s, length) string_type cs; cs.hdr.mark = gc_color_red; cs.hdr.grayed = 0; \
{ int len = length; \
  cs.tag = string_tag; cs.len = len; \
  cs.str = alloca(sizeof(char) * (len + 1)); \
  memcpy(cs.str, s, len); \
  cs.str[len] = '\0';}
#define make_string_noalloc(cs, s, length) string_type cs; cs.hdr.mark = gc_color_red; cs.hdr.grayed = 0; \
{ cs.tag = string_tag; cs.len = length; \
  cs.str = s; }

#define string_len(x) (((string_type *) x)->len)
#define string_str(x) (((string_type *) x)->str)

/* I/O types */

// TODO: FILE* may not be good enough
//       consider http://stackoverflow.com/questions/6206893/how-to-implement-char-ready-in-c
// TODO: a simple wrapper around FILE may not be good enough long-term
// TODO: how exactly mode will be used. need to know r/w, bin/txt
typedef struct {gc_header_type hdr; tag_type tag; FILE *fp; int mode;} port_type;
#define make_port(p,f,m) port_type p; p.hdr.mark = gc_color_red; p.hdr.grayed = 0; p.tag = port_tag; p.fp = f; p.mode = m;

/* Vector type */

typedef struct {gc_header_type hdr; tag_type tag; int num_elt; object *elts;} vector_type;
typedef vector_type *vector;

#define make_empty_vector(v) vector_type v; v.hdr.mark = gc_color_red; v.hdr.grayed = 0; v.tag = vector_tag; v.num_elt = 0; v.elts = NULL;

/* Bytevector type */

typedef struct {gc_header_type hdr; tag_type tag; int len; char *data;} bytevector_type;
typedef bytevector_type *bytevector;

#define make_empty_bytevector(v) bytevector_type v; v.hdr.mark = gc_color_red; v.hdr.grayed = 0; v.tag = bytevector_tag; v.len = 0; v.data = NULL;

/* Pair (cons) type */

typedef struct {gc_header_type hdr; tag_type tag; object cons_car,cons_cdr;} cons_type;
typedef cons_type *list;

#define car(x) (((list) x)->cons_car)
#define cdr(x) (((list) x)->cons_cdr)
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

#define make_cons(n,a,d) \
cons_type n; n.hdr.mark = gc_color_red; n.hdr.grayed = 0; n.tag = cons_tag; n.cons_car = a; n.cons_cdr = d;

/* Closure types */

typedef struct {gc_header_type hdr; tag_type tag; function_type fn; int num_args; } macro_type;
typedef struct {gc_header_type hdr; tag_type tag; function_type fn; int num_args; } closure0_type;
typedef struct {gc_header_type hdr; tag_type tag; function_type fn; int num_args; object elt1;} closure1_type;
typedef struct {gc_header_type hdr; tag_type tag; function_type fn; int num_args; int num_elt; object *elts;} closureN_type;

typedef closure0_type *closure0;
typedef closure1_type *closure1;
typedef closureN_type *closureN;
typedef closure0_type *closure;
typedef closure0_type *macro;

#define mmacro(c,f) macro_type c; c.hdr.mark = gc_color_red; c.hdr.grayed = 0; c.tag = macro_tag; c.fn = f; c.num_args = -1;
#define mclosure0(c,f) closure0_type c; c.hdr.mark = gc_color_red; c.hdr.grayed = 0; c.tag = closure0_tag; c.fn = f; c.num_args = -1;
#define mclosure1(c,f,a) closure1_type c; c.hdr.mark = gc_color_red; c.hdr.grayed = 0; c.tag = closure1_tag; \
   c.fn = f; c.num_args = -1; c.elt1 = a;

#define make_cell(n,a) make_cons(n,a,nil);

/* Primitive types */
typedef struct {gc_header_type hdr; tag_type tag; const char *pname; function_type fn;} primitive_type;
typedef primitive_type *primitive;

#define defprimitive(name, pname, fnc) \
static primitive_type name##_primitive = {primitive_tag, #pname, fnc}; \
static const object primitive_##name = &name##_primitive

#define prim(x) (x && ((primitive)x)->tag == primitive_tag)
#define prim_name(x) (((primitive_type *) x)->pname)

/* All constant-size objects */
typedef union {
  boolean_type boolean_t;
  cons_type cons_t;
  symbol_type symbol_t;
  primitive_type primitive_t;
  integer_type integer_t;
  double_type double_t;
} common_type;

/* Utility functions */
void **vpbuffer_realloc(void **buf, int *len);
void **vpbuffer_add(void **buf, int *len, int i, void *obj);
void vpbuffer_free(void **buf);

/* GC prototypes */
void gc_initialize();
void gc_add_mutator(gc_thread_data *thd);
void gc_remove_mutator(gc_thread_data *thd);
gc_heap *gc_heap_create(int heap_type, size_t size, size_t max_size, size_t chunk_size);
void gc_print_stats(gc_heap *h);
int gc_grow_heap(gc_heap *h, int heap_type, size_t size, size_t chunk_size);
char *gc_copy_obj(object hp, char *obj, gc_thread_data *thd);
void *gc_try_alloc(gc_heap *h, int heap_type, size_t size, char *obj, gc_thread_data *thd);
void *gc_alloc(gc_heap_root *h, size_t size, char *obj, gc_thread_data *thd, int *heap_grown);
size_t gc_allocated_bytes(object obj, gc_free_list *q, gc_free_list *r);
gc_heap *gc_heap_last(gc_heap *h);
size_t gc_heap_total_size(gc_heap *h);
//size_t gc_heap_total_free_size(gc_heap *h);
//size_t gc_collect(gc_heap *h, size_t *sum_freed);
//void gc_mark(gc_heap *h, object obj);
void gc_mark_globals(void);
size_t gc_sweep(gc_heap *h, int heap_type, size_t *sum_freed_ptr);
void gc_thr_grow_move_buffer(gc_thread_data *d);
void gc_thr_add_to_move_buffer(gc_thread_data *d, int *alloci, object obj);
void gc_thread_data_init(gc_thread_data *thd, int mut_num, char *stack_base, long stack_size);
void gc_thread_data_free(gc_thread_data *thd);
// Prototypes for mutator/collector:
int gc_is_stack_obj(gc_thread_data *thd, object obj);
void gc_mut_update(gc_thread_data *thd, object old_obj, object value);
void gc_mut_cooperate(gc_thread_data *thd, int buf_len);
void gc_mark_gray(gc_thread_data *thd, object obj);
void gc_mark_gray2(gc_thread_data *thd, object obj);
void gc_collector_trace();
void gc_mark_black(object obj);
void gc_collector_mark_gray(object parent, object obj);
void gc_empty_collector_stack();
void gc_handshake(gc_status_type s);
void gc_post_handshake(gc_status_type s);
void gc_wait_handshake();
void gc_start_collector();
void gc_mutator_thread_blocked(gc_thread_data *thd, object cont);
void gc_mutator_thread_runnable(gc_thread_data *thd, object result);
#define set_thread_blocked(d, c) \
  gc_mutator_thread_blocked(((gc_thread_data *)d), (c))
#define return_thread_runnable(d, r) \
  gc_mutator_thread_runnable(((gc_thread_data *)d), (r))
//#define do_with_blocked_thread(data, cont, result, body) \
//  set_thread_blocked((data), (cont)); \
//  body \
//  return_thread_runnable((data), (result));
gc_heap_root *gc_get_heap();
int gc_minor(void *data, object low_limit, object high_limit, closure cont, object *args, int num_args);
/* Mutation table to support minor GC write barrier */
void add_mutation(void *data, object var, int index, object value);
void clear_mutations(void *data);

#endif /* CYCLONE_TYPES_H */

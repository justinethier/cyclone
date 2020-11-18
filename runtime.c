/** 
 * Cyclone Scheme
 * https://github.com/justinethier/cyclone
 *
 * Copyright (c) 2014-2016, Justin Ethier
 * All rights reserved.
 *
 * This file contains the C runtime used by compiled programs.
 */

#include <ck_hs.h>
#include <ck_ht.h>
#include <ck_pr.h>
#include "cyclone/types.h"
#include "cyclone/runtime.h"
#include "cyclone/ck_ht_hash.h"
#include <errno.h>
#include <limits.h>
#include <ctype.h>
//#include <signal.h> // only used for debugging!
#include <sys/stat.h>

static uint32_t Cyc_utf8_decode(uint32_t* state, uint32_t* codep, uint32_t byte);
static int Cyc_utf8_count_code_points_and_bytes(uint8_t* s, char_type *codepoint, int *cpts, int *bytes);

/* Error checking section - type mismatch, num args, etc */
/* Type names to use for error messages */
const char *tag_names[] = {
      /*closure0_tag  */   "procedure"
      /*closure1_tag  */ , "procedure"
      /*closureN_tag  */ , "procedure"
      /*macro_tag     */ , "macro"
      /*boolean_tag   */ , "boolean"
      /*bytevector_tag */ , "bytevector"
      /*c_opaque_tag  */ , "opaque"
      /*cond_var_tag  */ , "condition variable"
      /*cvar_tag      */ , "C primitive"
      /*double_tag    */ , "number"
      /*eof_tag       */ , "eof"
      /*forward_tag   */ , ""
      /*integer_tag   */ , "number"
      /*bignum_tag    */ , "bignum"
      /*mutex_tag     */ , "mutex"
      /*pair_tag      */ , "pair"
      /*port_tag      */ , "port"
      /*primitive_tag */ , "primitive"
      /*string_tag    */ , "string"
      /*symbol_tag    */ , "symbol"
      /*vector_tag    */ , "vector"
      /*complex_num_tag*/ , "complex number"
      /*atomic_tag*/     , "atomic"
      /*void_tag*/       , "void"
  , "Reserved for future use"
};

void Cyc_invalid_type_error(void *data, int tag, object found)
{
  char buf[256];
#if GC_DEBUG_TRACE
  // Object address can be very useful for GC debugging
  snprintf(buf, 255, "Invalid type: expected %s, found (%p) ", tag_names[tag], found);
#else
  snprintf(buf, 255, "Invalid type: expected %s, found ", tag_names[tag]);
#endif
  Cyc_rt_raise2(data, buf, found);
}

void Cyc_immutable_obj_error(void *data, object obj)
{
  Cyc_rt_raise2(data, "Unable to modify immutable object ", obj);
}

void Cyc_mutable_obj_error(void *data, object obj)
{
  Cyc_rt_raise2(data, "Expected immutable object ", obj);
}

void Cyc_check_obj(void *data, int tag, object obj)
{
  if (!is_object_type(obj)) {
    Cyc_invalid_type_error(data, tag, obj);
  }
}

void Cyc_check_bounds(void *data, const char *label, int len, int index)
{
  if (index < 0 || index >= len) {
    char buf[128];
    snprintf(buf, 127, "%s - invalid index %d", label, index);
    Cyc_rt_raise_msg(data, buf);
  }
}

/* END error checking */

#ifdef CYC_HIGH_RES_TIMERS
/* High resolution timers */
#include <sys/time.h>
long long hrt_get_current() 
{
  struct timeval tv;
  gettimeofday(&tv, NULL); /* TODO: longer-term consider using clock_gettime instead */
  long long jiffy = (tv.tv_sec)*1000000LL + tv.tv_usec;
  return jiffy;
}

long long hrt_cmp_current(long long tstamp) 
{
  long long now = hrt_get_current();
  return (now - tstamp);
}

void hrt_log_delta(const char *label, long long tstamp) 
{
  static long long initial = 1;
  static long long initial_tstamp;
  if (initial == 1) {
    initial = 0;
    initial_tstamp = hrt_get_current();
  }
  long long total = hrt_cmp_current(initial_tstamp);
  long long delta = hrt_cmp_current(tstamp);
  fprintf(stderr, "%s, %llu, %llu\n", label, total, delta);
}

/* END High resolution timers */
#endif

/* These macros are hardcoded here to support functions in this module. */
#define closcall1(td, clo, a1) \
if (obj_is_not_closure(clo)) { \
   Cyc_apply(td, 0, (closure)(a1), clo); \
} else { \
   ((clo)->fn)(td, 1, clo, a1);\
}
#define return_closcall1(td, clo, a1) { \
 char top; \
 if (stack_overflow(&top, (((gc_thread_data *)data)->stack_limit))) { \
     object buf[1]; buf[0] = a1;\
     GC(td, clo, buf, 1); \
     return; \
 } else {\
     closcall1(td, (closure) (clo), a1); \
     return;\
 } \
}
#define _return_closcall1(td, clo, a1) { \
 char top; \
 if (stack_overflow(&top, (((gc_thread_data *)data)->stack_limit))) { \
     object buf[1]; buf[0] = a1;\
     GC(td, clo, buf, 1); \
     return NULL; \
 } else {\
     closcall1(td, (closure) (clo), a1); \
     return NULL;\
 } \
}
#define closcall2(td, clo, a1, a2) \
if (obj_is_not_closure(clo)) { \
   Cyc_apply(td, 1, (closure)(a1), clo,a2); \
} else { \
  ((clo)->fn)(td, 2, clo, a1, a2);\
}
#define return_closcall2(td, clo, a1, a2) { \
 char top; \
 if (stack_overflow(&top, (((gc_thread_data *)data)->stack_limit))) { \
     object buf[2]; buf[0] = a1;buf[1] = a2;\
     GC(td, clo, buf, 2); \
     return; \
 } else {\
     closcall2(td, (closure) (clo), a1, a2); \
     return;\
 } \
}
#define _return_closcall2(td, clo, a1, a2) { \
 char top; \
 if (stack_overflow(&top, (((gc_thread_data *)data)->stack_limit))) { \
     object buf[2]; buf[0] = a1;buf[1] = a2;\
     GC(td, clo, buf, 2); \
     return NULL; \
 } else {\
     closcall2(td, (closure) (clo), a1, a2); \
     return NULL;\
 } \
}
/*END closcall section */

/* Global variables. */
object Cyc_global_variables = NULL;
int _cyc_argc = 0;
char **_cyc_argv = NULL;

static symbol_type __EOF = { {0}, eof_tag, ""};  // symbol_type in lieu of custom type
static symbol_type __VOID = { {0}, void_tag, ""};  // symbol_type in lieu of custom type

const object Cyc_EOF = &__EOF;
const object Cyc_VOID = &__VOID;
static ck_hs_t lib_table;
static ck_hs_t symbol_table;
static int symbol_table_initial_size = 4096;
static pthread_mutex_t symbol_table_lock;

char **env_variables = NULL;
char **get_env_variables()
{
  return env_variables;
}

void pack_env_variables(void *data, object k)
{
  char **env = env_variables;
  object tail;
  object head = NULL;
  tail = head;
  for (; *env != NULL; env++) {
    char *e = *env,
         *eqpos = strchr(e, '=');
    pair_type *p = alloca(sizeof(pair_type));
    pair_type *tmp = alloca(sizeof(pair_type));
    string_type *sval = alloca(sizeof(string_type));
    string_type *svar = alloca(sizeof(string_type));

    svar->hdr.mark = gc_color_red; 
    svar->hdr.grayed = 0;
    svar->hdr.immutable = 0;
    svar->tag = string_tag; 
    svar->len = eqpos - e;
    svar->str = alloca(sizeof(char) * (svar->len));
    strncpy(svar->str, e, svar->len);
    (svar->str)[svar->len] = '\0';
    svar->num_cp = Cyc_utf8_count_code_points((uint8_t *)svar->str);

    if (eqpos) {
      eqpos++;
    }
    sval->hdr.mark = gc_color_red; 
    sval->hdr.grayed = 0;
    sval->hdr.immutable = 0;
    sval->tag = string_tag; 
    sval->len = strlen(eqpos);
    svar->num_cp = Cyc_utf8_count_code_points((uint8_t *)eqpos);
    sval->str = eqpos;
    set_pair(tmp, svar, sval);
    set_pair(p, tmp, NULL);
    if (head == NULL) {
        tail = head = p;
    } else {
      cdr(tail) = p;
      tail = p;
    }
  }
  return_closcall1(data, k, head);
}

void set_env_variables(char **vars)
{
  env_variables = vars;
}

// Functions to support concurrency kit hashset
// These are specifically for a table of symbols
static void *hs_malloc(size_t r)
{
  return malloc(r);
}

static void hs_free(void *p, size_t b, bool r)
{
  free(p);
}

static struct ck_malloc my_allocator = {
  .malloc = hs_malloc,
  .free = hs_free
};

static unsigned long hs_hash(const void *object, unsigned long seed)
{
  const symbol_type *c = object;
  unsigned long h;

  h = (unsigned long)MurmurHash64A(c->desc, strlen(c->desc), seed);
  return h;
}

static bool hs_compare(const void *previous, const void *compare)
{
  return strcmp(symbol_desc(previous), symbol_desc(compare)) == 0;
}

static void *set_get(ck_hs_t * hs, const void *value)
{
  unsigned long h;
  void *v;

  h = CK_HS_HASH(hs, hs_hash, value);
  v = ck_hs_get(hs, h, value);
  return v;
}

static bool set_insert(ck_hs_t * hs, const void *value)
{
  unsigned long h;

  h = CK_HS_HASH(hs, hs_hash, value);
  return ck_hs_put(hs, h, value);
}
// End hashset supporting functions

/**
 * @brief Perform one-time heap initializations for the program
 * @param heap_size Unused
 */
void gc_init_heap(long heap_size)
{
  if (!ck_hs_init(&lib_table,
                  CK_HS_MODE_OBJECT | CK_HS_MODE_SPMC,
                  hs_hash, hs_compare,
                  &my_allocator, 32, 43423)) {
    fprintf(stderr, "Unable to initialize library table\n");
    exit(1);
  }
  if (!ck_hs_init(&symbol_table,
                  CK_HS_MODE_OBJECT | CK_HS_MODE_SPMC,
                  hs_hash, hs_compare,
                  &my_allocator, symbol_table_initial_size, 43423)) {
    fprintf(stderr, "Unable to initialize symbol table\n");
    exit(1);
  }
  if (pthread_mutex_init(&(symbol_table_lock), NULL) != 0) {
    fprintf(stderr, "Unable to initialize symbol_table_lock mutex\n");
    exit(1);
  }
  
  //ht_test(); // JAE - DEBUGGING!!
}

object cell_get(object cell)
{
  // Always use unsafe car here, since cell_get calls are computed by compiler
  return car(cell);
}

object Cyc_global_set(void *thd, object identifier, object * glo, object value)
{
  gc_mut_update((gc_thread_data *) thd, *glo, value);
  *(glo) = value;
  ((gc_thread_data *) thd)->globals_changed = 1;
  return value;
}

static void Cyc_global_set_cps_gc_return(void *data, int argc, object cont, object glo_obj, object val, object next)
{
  object *glo = (object *)glo_obj;
  *(glo) = val;
  closcall1(data, (closure)next, val);
}

object Cyc_global_set_cps(void *thd, object cont, object identifier, object * glo, object value)
{
  int do_gc = 0;
  value = transport_stack_value(thd, NULL, value, &do_gc); // glo cannot be thread-local!
  gc_mut_update((gc_thread_data *) thd, *glo, value);
  if (do_gc) {
    // Ensure global is a root. We need to do this here to ensure
    // global and all its children are relocated to the heap.
    cvar_type cv = { {0}, cvar_tag, glo };
    gc_thread_data *data = (gc_thread_data *) thd;
    data->mutations = vpbuffer_add(data->mutations, 
                                  &(data->mutation_buflen), 
                                  data->mutation_count, 
                                  &cv);
    data->mutation_count++;
    // Run GC, then do the actual assignment with heap objects
    mclosure0(clo, (function_type)Cyc_global_set_cps_gc_return);
    object buf[3]; buf[0] = (object)glo; buf[1] = value; buf[2] = cont;
    GC(data, &clo, buf, 3);
  }
  *(glo) = value; // Already have heap objs, do assignment now
  return value;
}


static boolean_type t_boolean = { {0}, boolean_tag, "t" };
static boolean_type f_boolean = { {0}, boolean_tag, "f" };
static symbol_type Cyc_void_symbol = { {0}, symbol_tag, ""};

const object boolean_t = &t_boolean;
const object boolean_f = &f_boolean;
const object quote_void = &Cyc_void_symbol;

/* Stack Traces */

/**
 * @brief Print the contents of the given thread's stack trace buffer.
 * @param data Thread data object
 * @param out Output stream
 */
void Cyc_st_print(void *data, FILE * out)
{
  /* print to stream, note it is possible that
     some traces could be on the stack after a GC.
     not sure what to do about it, may need to
     detect that case and stop printing.
     or, with the tbl being so small, maybe it will
     not be an issue in practice? a bit risky to ignore though
   */
  gc_thread_data *thd = (gc_thread_data *) data;
  int n = 1;
  int i = (thd->stack_trace_idx - 1);
  if (i < 0) { i = MAX_STACK_TRACES - 1; }

  while (i != thd->stack_trace_idx) {
    if (thd->stack_traces[i]) {
      fprintf(out, "[%d] %s\n", n++, thd->stack_traces[i]);
    }
    i = (i - 1);
    if (i < 0) { i = MAX_STACK_TRACES - 1; }
  }
}

/* END Stack Traces section */

/* Symbol Table */

/* Notes for the symbol table

 string->symbol can:
  - lookup symbol in the table
  - if found, return that pointer
  - otherwise, allocate symbol in table and return ptr to it

 For now, GC of symbols is missing. long-term it probably would be desirable
*/
static char *_strdup(const char *s)
{
  char *d = malloc(strlen(s) + 1);
  if (d) {
    strcpy(d, s);
  }
  return d;
}

static object find_symbol_by_name(const char *name)
{
  symbol_type tmp = { {0}, symbol_tag, name};
  object result = set_get(&symbol_table, &tmp);
  return result;
}

object add_symbol(symbol_type * psym)
{
  pthread_mutex_lock(&symbol_table_lock);       // Only 1 "writer" allowed
  set_insert(&symbol_table, psym);
  pthread_mutex_unlock(&symbol_table_lock);
  return psym;
}

static object add_symbol_by_name(const char *name)
{
  symbol_type sym = { {0}, symbol_tag, _strdup(name)};
  symbol_type *psym = malloc(sizeof(symbol_type));
  memcpy(psym, &sym, sizeof(symbol_type));
  return add_symbol(psym);
}

object find_or_add_symbol(const char *name)
{
  object sym = find_symbol_by_name(name);
  if (sym) {
    return sym;
  } else {
    return add_symbol_by_name(name);
  }
}

/* END symbol table */

/* Library table */
object is_library_loaded(const char *name)
{
  symbol_type tmp = { {0}, symbol_tag, name};
  object result = set_get(&lib_table, &tmp);
  if (result)
    return boolean_t;
  return boolean_f;
}

object register_library(const char *name)
{
  symbol_type sym = { {0}, symbol_tag, _strdup(name)};
  symbol_type *psym = malloc(sizeof(symbol_type));
  memcpy(psym, &sym, sizeof(symbol_type));
  // Reuse mutex since lib inserts will be rare
  pthread_mutex_lock(&symbol_table_lock);       // Only 1 "writer" allowed
  set_insert(&lib_table, psym);
  pthread_mutex_unlock(&symbol_table_lock);
  return boolean_t;
}
/* END Library table */


/* Global table */
list global_table = NULL;

void add_global(const char *identifier, object * glo)
{
  // Tried using a vpbuffer for this and the benchmark
  // results were the same or worse.
  global_table = malloc_make_pair(mcvar(glo), global_table);
}

void debug_dump_globals()
{
  list l = global_table;
  for (; l != NULL; l = cdr(l)) {
    cvar_type *c = (cvar_type *) car(l);
    //gc_mark(h, *(c->pvar)); // Mark actual object the global points to
    printf("DEBUG %p ", c->pvar);
    if (*c->pvar) {
      printf("mark = %d ", mark(*c->pvar));
      if (mark(*c->pvar) == gc_color_red) {
        printf("obj = ");
        // TODO: no data param: Cyc_display(*c->pvar, stdout);
      }
      printf("\n");
    } else {
      printf(" is NULL\n");
    }
  }
}

void Cyc_set_globals_changed(gc_thread_data *thd) 
{
  thd->globals_changed = 1;
}

/* END Global table */

/** new write barrier
 * This function determines if a mutation introduces a pointer to a stack
 * object from a heap object, and if so, either copies the object to the
 * heap or lets the caller know a minor GC must be performed.
 *
 * @param data   Current thread's data object
 * @param var    Object being mutated
 * @param value  New value being associated to var
 * @param run_gc OUT parameter, returns 1 if minor GC needs to be invoked
 * @return Pointer to `var` object
 */
object transport_stack_value(gc_thread_data *data, object var, object value, int *run_gc) 
{
  char tmp;
  int inttmp, *heap_grown = &inttmp;
  gc_heap_root *heap = data->heap;

  // Nothing needs to be done unless we are mutating
  // a heap variable to point to a stack var.
  if (!gc_is_stack_obj(&tmp, data, var) && gc_is_stack_obj(&tmp, data, value)) {
    // Must move `value` to the heap to allow use by other threads
    switch(type_of(value)) {
      case string_tag:
      case bytevector_tag:
        if (immutable(value)) {
          // Safe to transport now
          object hp = gc_alloc(heap, gc_allocated_bytes(value, NULL, NULL), value, data, heap_grown);
          return hp;
        }
        // Need to GC if obj is mutable, EG: a string could be mutated so we can't
        // have multiple copies of the object running around
        *run_gc = 1;
        return value;
      case double_tag:
      case port_tag:
      case c_opaque_tag:
      case complex_num_tag: {
        // These objects are immutable, transport now
        object hp = gc_alloc(heap, gc_allocated_bytes(value, NULL, NULL), value, data, heap_grown);
        return hp;
      }
      // Objs w/children force minor GC to guarantee everything is relocated:
      case cvar_tag:
      case closure0_tag:
      case closure1_tag:
      case closureN_tag:
      case pair_tag:
      case vector_tag:
        *run_gc = 1;
        return value;
      default:
        // Other object types are not stack-allocated so should never get here
        printf("Invalid shared object type %d\n", type_of(value));
        exit(1);
    }
  }

  return value;
}


/* Mutation table functions
 *
 * Keep track of mutations (EG: set-car!) so we can avoid having heap
 * objects that point to old stack objects. We need to transport any
 * such stack objects to the heap during minor GC.
 *
 * Note these functions and underlying data structure are only used by
 * the calling thread, so locking is not required.
 */
void add_mutation(void *data, object var, int index, object value)
{
  gc_thread_data *thd = (gc_thread_data *) data;
  char tmp;

  // No need to track for minor GC purposes unless we are mutating
  // a heap variable to point to a stack var.
  //
  // If var is on stack we'll get it anyway in minor GC,
  // and if value is on heap we don't care (no chance of heap pointing to nursery)
  if (!gc_is_stack_obj(&tmp, data, var) && gc_is_stack_obj(&tmp, data, value)) {
    thd->mutations = vpbuffer_add(thd->mutations, 
                                  &(thd->mutation_buflen), 
                                  thd->mutation_count, 
                                  var);
    thd->mutation_count++;
    if (index >= 0) {
      // For vectors only, add index as another var. That way
      // the write barrier only needs to inspect the mutated index.
      thd->mutations = vpbuffer_add(thd->mutations, 
                                     &(thd->mutation_buflen), 
                                     thd->mutation_count, 
                                     obj_int2obj(index));
      thd->mutation_count++;
    }
  }
}

void clear_mutations(void *data)
{
  // Not clearing memory, just resetting count
  gc_thread_data *thd = (gc_thread_data *) data;
  thd->mutation_count = 0;
}

/* END mutation table */

/* Runtime globals */
object Cyc_glo_call_cc = NULL;
object Cyc_glo_eval_from_c = NULL;

/**
 * @brief The default exception handler
 * @param data Thread data object
 * @return argc Unused, just here to maintain calling convention
 * @return _ Unused, just here to maintain calling convention
 * @return err Object containing data for the error
 */
object Cyc_default_exception_handler(void *data, int argc, closure _,
                                     object err)
{
  int is_msg = 1;
  fprintf(stderr, "Error: ");

  if ((err == NULL) || is_value_type(err) || type_of(err) != pair_tag) {
    Cyc_display(data, err, stderr);
  } else {
    // Error is list of form (type arg1 ... argn)
    err = cdr(err);             // skip type field
    for (; (err != NULL); err = cdr(err)) {     // output with no enclosing parens
      if (is_msg && 
          is_object_type(car(err)) && 
          type_of(car(err)) == string_tag) {
        is_msg = 0;
        Cyc_display(data, car(err), stderr);
        if (cdr(err)) {
          fprintf(stderr, ": ");
        }
      } else {
        Cyc_write(data, car(err), stderr);
        fprintf(stderr, " ");
      }
    }
  }

  fprintf(stderr, "\nCall history, most recent first:\n");
  Cyc_st_print(data, stderr);
  fprintf(stderr, "\n");
  //raise(SIGINT); // break into debugger, unix only
  exit(1);
  return NULL;
}

/**
 * @brief Return the current exception handler
 * @param data Thread data object
 * @return Object registered as the exception handler, or the default if none.
 */
object Cyc_current_exception_handler(void *data)
{
  gc_thread_data *thd = (gc_thread_data *) data;
  if (thd->exception_handler_stack == NULL) {
    return primitive_Cyc_91default_91exception_91handler;
  } else {
    return car(thd->exception_handler_stack);
  }
}

/**
 * @brief Raise an exception from the runtime code
 * @param data Thread data object
 * @param err Data for the error
 */
void Cyc_rt_raise(void *data, object err)
{
  make_pair(c2, err, NULL);
  make_pair(c1, boolean_f, &c2);
  make_pair(c0, &c1, NULL);
  apply(data, NULL, Cyc_current_exception_handler(data), &c0);
  // Should never get here
  fprintf(stderr, "Internal error in Cyc_rt_raise\n");
  exit(1);
}

/**
 * @brief Raise an exception from the runtime code
 * @param data Thread data object
 * @param msg A message describing the error
 * @param err Data for the error
 */
void Cyc_rt_raise2(void *data, const char *msg, object err)
{
  make_utf8_string(data, s, msg);
  make_pair(c3, err, NULL);
  make_pair(c2, &s, &c3);
  make_pair(c1, boolean_f, &c2);
  make_pair(c0, &c1, NULL);
  apply(data, NULL, Cyc_current_exception_handler(data), &c0);
  // Should never get here
  fprintf(stderr, "Internal error in Cyc_rt_raise2\n");
  exit(1);
}

/**
 * @brief Raise an exception from the runtime code
 * @param data Thread data object
 * @param err A message describing the error
 */
void Cyc_rt_raise_msg(void *data, const char *err)
{
  make_utf8_string(data, s, err);
  Cyc_rt_raise(data, &s);
}

/* END exception handler */

int equal(object x, object y)
{
  if (x == y)
    return 1;
  if (x == NULL)
    return (y == NULL);
  if (y == NULL)
    return (x == NULL);
  if (obj_is_char(x))
    return obj_is_char(y) && x == y;
  if (obj_is_int(x))
    return (obj_is_int(y) && x == y) ||
        (is_object_type(y) &&
         (
          (type_of(y) == integer_tag && integer_value(y) == obj_obj2int(x)) ||
          (type_of(y) == bignum_tag && Cyc_bignum_cmp(MP_EQ, x, -1, y, bignum_tag))
         ));
  switch (type_of(x)) {
  case string_tag:
    return (is_object_type(y) &&
            type_of(y) == string_tag &&
            strcmp(((string_type *) x)->str, ((string_type *) y)->str) == 0);
  case double_tag:
    return (is_object_type(y) &&
            type_of(y) == double_tag &&
            ((double_type *) x)->value == ((double_type *) y)->value);
  case vector_tag:
    if (is_object_type(y) &&
        type_of(y) == vector_tag &&
        ((vector) x)->num_elements == ((vector) y)->num_elements) {
      int i;
      if (x == y) return 1;
      for (i = 0; i < ((vector) x)->num_elements; i++) {
        if (equalp(((vector) x)->elements[i], ((vector) y)->elements[i]) ==
            boolean_f)
          return 0;
      }
      return 1;
    }
    return 0;
  case bytevector_tag:
    if (is_object_type(y) &&
        type_of(y) == bytevector_tag &&
        ((bytevector) x)->len == ((bytevector) y)->len) {
      int i;
      for (i = 0; i < ((bytevector) x)->len; i++) {
        if (((bytevector)x)->data[i] != ((bytevector)y)->data[i]) {
          return 0;
        }
      }
      return 1;
    }
    return 0;
  case bignum_tag: {
    int ty = -1;
    if (is_value_type(y)) {
      if (!obj_is_int(y)) {
        return 0;
      }
    } else {
      ty = type_of(y);
    }
    
    return Cyc_bignum_cmp(MP_EQ, x, bignum_tag, y, ty);
  //  return (is_object_type(y) &&
  //          type_of(y) == bignum_tag &&
  //          MP_EQ == mp_cmp(&bignum_value(x), &bignum_value(y)));
  }
  //case integer_tag:
  //  return (obj_is_int(y) && obj_obj2int(y) == integer_value(x)) ||
  //      (is_object_type(y) &&
  //       type_of(y) == integer_tag &&
  //       ((integer_type *) x)->value == ((integer_type *) y)->value);
  case complex_num_tag:
    return (is_object_type(y) &&
            type_of(y) == complex_num_tag &&
            ((complex_num_type *) x)->value == ((complex_num_type *) y)->value);

  default:
    return x == y;
  }
}

//object Cyc_car(void *data, object lis)
//{
//  Cyc_check_pair(data, lis);
//  return car(lis);
//}
//
//object Cyc_cdr(void *data, object lis)
//{
//  Cyc_check_pair(data, lis);
//  return cdr(lis);
//}

object Cyc_get_global_variables()
{
  return Cyc_global_variables;
}

object Cyc_get_cvar(object var)
{
  if (is_object_type(var) && type_of(var) == cvar_tag) {
    return *(((cvar_type *) var)->pvar);
  }
  return var;
}

object Cyc_set_cvar(object var, object value)
{
  if (is_object_type(var) && type_of(var) == cvar_tag) {
    *(((cvar_type *) var)->pvar) = value;
  }
  return var;
}

object Cyc_has_vector_cycle(object vec)
{
  int i;
  // TODO: this is not generic enough
  for (i = 0; i < ((vector)vec)->num_elements; i++) {
    if (((vector)vec)->elements[i] == vec) {
      return boolean_t;
    }
  }
  return boolean_f;
}

object Cyc_has_cycle(object lst)
{
  object slow_lst, fast_lst;
  if ((lst == NULL) || is_value_type(lst)) {
    return boolean_f;
  } else if (is_object_type(lst) && type_of(lst) == vector_tag) {
    return Cyc_has_vector_cycle(lst);
  } else if (is_object_type(lst) && type_of(lst) != pair_tag) {
    return boolean_f;
  }
  slow_lst = lst;
  fast_lst = cdr(lst);
  while (1) {
    if (fast_lst == NULL)
      return boolean_f;
    if (Cyc_is_pair(fast_lst) == boolean_f)
      return boolean_f;
    if ((cdr(fast_lst)) == NULL)
      return boolean_f;
    if (Cyc_is_pair(cdr(fast_lst)) == boolean_f)
      return boolean_f;
    if (slow_lst == fast_lst)
      return boolean_t;

    slow_lst = cdr(slow_lst);
    fast_lst = cddr(fast_lst);
  }
}

/**
 * Predicate - is the object a proper list?
 * Based on `Cyc_has_cycle` so it is safe to call on circular lists.
 */
object Cyc_is_list(object lst)
{
  object slow_lst, fast_lst;
  if (lst == NULL){
    return boolean_t;
  } else if (is_value_type(lst)) {
    return boolean_f;
  } else if (is_object_type(lst) && type_of(lst) != pair_tag) {
    return boolean_f;
  }
  slow_lst = lst;
  fast_lst = cdr(lst);
  while (1) {
    if (fast_lst == NULL)
      return boolean_t;
    if (Cyc_is_pair(fast_lst) == boolean_f)
      return boolean_f; // Improper list
    if ((cdr(fast_lst)) == NULL)
      return boolean_t;
    if (Cyc_is_pair(cdr(fast_lst)) == boolean_f)
      return boolean_f; // Improper
    if (slow_lst == fast_lst)
      return boolean_t; // Cycle; we have a list

    slow_lst = cdr(slow_lst);
    fast_lst = cddr(fast_lst);
  }
}

/**
 * Write string representation of a double to a buffer.
 * Added code from Chibi Scheme to print a ".0" if the 
 * double is a whole number (EG: 3.0) to avoid confusion
 * in the output (EG: was "3").
 */
int double2buffer(char *buf, int buf_size, double num)
{
  int i;
  i = snprintf(buf, buf_size, "%.15g", num);
  if (!strchr(buf, '.') && !strchr(buf, 'e')) {
    buf[i++] = '.';
    buf[i++] = '0';
    buf[i++] = '\0';
  }
  return i;
}

// TODO: need to change I/O functions (including display/write below)
// to accept an optional port arg. also, if port is not specified, should
// use (current-output-port) instead of stdout. will need to expose the
// (current-*port) functions somehow (tricky since we do not have param
// object yet) then figure out how to use them.
//
// If port is omitted from any output procedure, it defaults
// to the value returned by (current-output-port). It is an
// error to attempt an output operation on a closed port
//
void dispatch_display_va(void *data, int argc, object clo, object cont,
                           object x, ...)
{
  object result;
  va_list ap;
  va_start(ap, x);
  result = Cyc_display_va_list(data, argc - 1, x, ap);
  va_end(ap);
  return_closcall1(data, cont, result);
}

object Cyc_display_va(void *data, int argc, object x, ...)
{
  object result;
  va_list ap;
  va_start(ap, x);
  result = Cyc_display_va_list(data, argc, x, ap);
  va_end(ap);
  return result;
}

object Cyc_display_va_list(void *data, int argc, object x, va_list ap)
{
  FILE *fp = stdout; // TODO: just a placeholder, should use current-output-port
  if (argc > 1) {
    object tmp;
    tmp = va_arg(ap, object);
    Cyc_check_port(data, tmp);
    fp = ((port_type *) tmp)->fp;
    if (fp == NULL) {
      Cyc_rt_raise2(data, "Unable to write to closed port: ", tmp);
      return quote_void;
    }
  }
  return Cyc_display(data, x, fp);
}

object Cyc_display(void *data, object x, FILE * port)
{
  object tmp = NULL;
  object has_cycle = boolean_f;
  int i = 0;
  if (x == NULL) {
    fprintf(port, "()");
    return quote_void;
  }
  if (obj_is_char(x)) {
    char cbuf[5];
    char_type unbox = obj_obj2char(x);
    Cyc_utf8_encode_char(cbuf, 5, unbox);
    fprintf(port, "%s", cbuf);
    return quote_void;
  }
  if (obj_is_int(x)) {
    fprintf(port, "%ld", obj_obj2int(x));
    return quote_void;
  }
  switch (type_of(x)) {
  case macro_tag:
    fprintf(port, "<macro %p>", (void *)((closure) x)->fn);
    break;
  case closure0_tag:
  case closure1_tag:
  case closureN_tag:
    fprintf(port, "<procedure %p>", (void *)((closure) x)->fn);
    break;
  case eof_tag:
    fprintf(port, "<EOF>");
    break;
  case void_tag:
    break;
  case port_tag:
    fprintf(port, "<port %p>", ((port_type *) x)->fp);
    break;
  case primitive_tag:
    fprintf(port, "<primitive %s>", prim_name(x));
    break;
  case cvar_tag:
    fprintf(port, "<cvar %p>", Cyc_get_cvar(x));
    break;
  case c_opaque_tag:
    fprintf(port, "<C opaque %p>", opaque_ptr(x));
    break;
  case mutex_tag:
    fprintf(port, "<mutex %p>", x);
    break;
  case cond_var_tag:
    fprintf(port, "<condition variable %p>", x);
    break;
  case atomic_tag:
    fprintf(port, "<atom %p>", x);
    break;
  case boolean_tag:
    fprintf(port, "#%s", ((boolean_type *) x)->desc);
    break;
  case symbol_tag:
    fprintf(port, "%s", ((symbol_type *) x)->desc);
    break;
  case integer_tag:
    fprintf(port, "%d", ((integer_type *) x)->value);
    break;
  case double_tag: {
    char buf[33];
    double2buffer(buf, 32, ((double_type *) x)->value);
    fprintf(port, "%s", buf);
    break;
  }
  case string_tag:
    fprintf(port, "%s", ((string_type *) x)->str);
    break;
  case vector_tag:
    has_cycle = Cyc_has_cycle(x);
    fprintf(port, "#(");
    if (has_cycle == boolean_t) {
      fprintf(port, "...");
    } else {
      for (i = 0; i < ((vector) x)->num_elements; i++) {
        if (i > 0) {
          fprintf(port, " ");
        }
        Cyc_display(data, ((vector) x)->elements[i], port);
      }
    }
    fprintf(port, ")");
    break;
  case bytevector_tag:
    fprintf(port, "#u8(");
    for (i = 0; i < ((bytevector) x)->len; i++) {
      if (i > 0) {
        fprintf(port, " ");
      }
      fprintf(port, "%u", (unsigned char)(((bytevector) x)->data[i]));
    }
    fprintf(port, ")");
    break;
  case pair_tag:
    has_cycle = Cyc_has_cycle(x);
    fprintf(port, "(");
    Cyc_display(data, car(x), port);

    // Experimenting with displaying lambda defs in REPL
    // not good enough but this is a start. would probably need
    // the same code in write()
    if (Cyc_is_symbol(car(x)) == boolean_t &&
        strncmp(((symbol) car(x))->desc, "procedure", 10) == 0) {
      fprintf(port, " ");
      Cyc_display(data, cadr(x), port);
      fprintf(port, " ...)");   /* skip body and env for now */
      break;
    }

    for (tmp = cdr(x); Cyc_is_pair(tmp) == boolean_t; tmp = cdr(tmp)) {
      if (has_cycle == boolean_t) {
        if (i++ > 20)
          break;                /* arbitrary number, for now */
      }
      fprintf(port, " ");
      Cyc_display(data, car(tmp), port);
    }
    if (has_cycle == boolean_t) {
      fprintf(port, " ...");
    } else if (tmp) {
      fprintf(port, " . ");
      Cyc_display(data, tmp, port);
    }
    fprintf(port, ")");
    break;
  case bignum_tag: {
    int bufsz; 
    char *buf;
    size_t written;

    BIGNUM_CALL(mp_radix_size(&bignum_value(x), 10, &bufsz));

    buf = alloca(bufsz);
    if (mp_to_radix(&bignum_value(x), buf, bufsz, &written,10) != 0) {
      fprintf(port, "Error displaying bignum!");
      exit(1);
    }
    fprintf(port, "%s", buf);
    break;
  }
  case complex_num_tag: {
    char rbuf[33], ibuf[33];
    const char *plus="+", *empty="";
    double dreal = creal(((complex_num_type *) x)->value);
    double dimag = cimag(((complex_num_type *) x)->value);
    double2buffer(rbuf, 32, dreal);
    double2buffer(ibuf, 32, dimag);
    if (dreal == 0.0) {
      fprintf(port, "%si", ibuf);
    } else {
      fprintf(port, "%s%s%si", 
        rbuf, 
        (dimag < 0.0) ? empty : plus,
        ibuf);
    }
    break;
  }
  default:
    fprintf(port, "Cyc_display: bad tag x=%d\n", ((closure) x)->tag);
    exit(1);
  }
  return quote_void;
}

void dispatch_write_va(void *data, int argc, object clo, object cont,
                         object x, ...)
{
  object result;
  va_list ap;
  va_start(ap, x);
  result = Cyc_write_va_list(data, argc - 1, x, ap);
  va_end(ap);
  return_closcall1(data, cont, result);
}

object Cyc_write_va(void *data, int argc, object x, ...)
{
  object result;
  va_list ap;
  va_start(ap, x);
  result = Cyc_write_va_list(data, argc, x, ap);
  va_end(ap);
  return result;
}

object Cyc_write_va_list(void *data, int argc, object x, va_list ap)
{
  FILE *fp = stdout; // OK since this is the internal version of write
  // Longer-term maybe we get rid of varargs for this one
  if (argc > 1) {
    object tmp;
    tmp = va_arg(ap, object);
    Cyc_check_port(data, tmp);
    fp = ((port_type *) tmp)->fp;
    if (fp == NULL) {
      Cyc_rt_raise2(data, "Unable to write to closed port: ", tmp);
      return quote_void;
    }
  }
  return Cyc_write(data, x, fp);
}

static object _Cyc_write(void *data, object x, FILE * port)
{
  object tmp = NULL;
  object has_cycle = boolean_f;
  int i = 0;
  if (x == NULL) {
    fprintf(port, "()");
    return quote_void;
  }
  if (obj_is_char(x)) {
    char_type c = obj_obj2char(x);
    switch (c) {
    case 0:   fprintf(port, "#\\null"); break;
    case 7:   fprintf(port, "#\\alarm"); break;
    case 8:   fprintf(port, "#\\backspace"); break;
    case 9:   fprintf(port, "#\\tab"); break;
    case 10:  fprintf(port, "#\\newline"); break;
    case 13:  fprintf(port, "#\\return"); break;
    case 27:  fprintf(port, "#\\escape"); break;
    case 32:  fprintf(port, "#\\space"); break;
    case 127: fprintf(port, "#\\delete"); break;
    default: {
      char cbuf[5];
      Cyc_utf8_encode_char(cbuf, 5, c);
      fprintf(port, "#\\%s", cbuf);
      break;
      }
    }
    return quote_void;
  }
  if (obj_is_int(x)) {
    Cyc_display(data, x, port);
    return quote_void;
  }
  switch (type_of(x)) {
  case string_tag: {
    //fprintf(port, "\"%s\"", ((string_type *) x)->str);
    char *s = string_str(x);
    fputc('"', port);
    while (*s){
      switch(*s){
      case '\a': fprintf(port, "\\a"); break;
      case '\b': fprintf(port, "\\b"); break;
      case '\f': fprintf(port, "\\f"); break;
      case '\n': fprintf(port, "\\n"); break;
      case '\r': fprintf(port, "\\r"); break;
      case '\t': fprintf(port, "\\t"); break;
      case '\v': fprintf(port, "\\v"); break;
      case '\\': fprintf(port, "\\\\"); break;
      case '\"': fprintf(port, "\\\""); break;
      default:
        fputc(*s, port);
        break;
      }
      s++;
    }
    fputc('"', port);
    break;
  }
  case vector_tag:
    has_cycle = Cyc_has_cycle(x);
    fprintf(port, "#(");
    if (has_cycle == boolean_t) {
      fprintf(port, "...");
    } else {
      for (i = 0; i < ((vector) x)->num_elements; i++) {
        if (i > 0) {
          fprintf(port, " ");
        }
        _Cyc_write(data, ((vector) x)->elements[i], port);
      }
    }
    fprintf(port, ")");
    break;
  case pair_tag:
    has_cycle = Cyc_has_cycle(x);
    fprintf(port, "(");
    _Cyc_write(data, car(x), port);

    // Experimenting with displaying lambda defs in REPL
    // not good enough but this is a start. would probably need
    // the same code in write()
    if (Cyc_is_symbol(car(x)) == boolean_t &&
        strncmp(((symbol) car(x))->desc, "procedure", 10) == 0) {
      fprintf(port, " ");
      _Cyc_write(data, cadr(x), port);
      fprintf(port, " ...)");   /* skip body and env for now */
      break;
    }

    for (tmp = cdr(x); Cyc_is_pair(tmp) == boolean_t; tmp = cdr(tmp)) {
      if (has_cycle == boolean_t) {
        if (i++ > 20)
          break;                /* arbitrary number, for now */
      }
      fprintf(port, " ");
      _Cyc_write(data, car(tmp), port);
    }
    if (has_cycle == boolean_t) {
      fprintf(port, " ...");
    } else if (tmp) {
      fprintf(port, " . ");
      _Cyc_write(data, tmp, port);
    }
    fprintf(port, ")");
    break;
  default:
    Cyc_display(data, x, port);
  }
  return quote_void;
}

object Cyc_write(void *data, object x, FILE * port)
{
  object y = _Cyc_write(data, x, port);
  //fprintf(port, "\n");
  return y;
}

object Cyc_write_char(void *data, object c, object port)
{
  Cyc_check_port(data, port);
  if (obj_is_char(c)) {
    FILE *fp = ((port_type *) port)->fp;
    if (fp){
      char cbuf[5];
      char_type unbox = obj_obj2char(c);
      Cyc_utf8_encode_char(cbuf, 5, unbox);
      fprintf(fp, "%s", cbuf);
    }
  } else {
    Cyc_rt_raise2(data, "Argument is not a character", c);
  }
  return quote_void;
}

object Cyc_write_u8(void *data, object c, object port)
{
  Cyc_check_port(data, port);
  if (obj_is_int(c)) {
    FILE *fp = ((port_type *) port)->fp;
    if (fp){
      int i = obj_obj2int(c);
      putc(i, fp);
    }
  } else {
    Cyc_rt_raise2(data, "Argument is not an integer", c);
  }
  return quote_void;
}

object Cyc_write_bytevector(void *data, object bvec, object port, object start, object end)
{
  Cyc_check_port(data, port);
  Cyc_check_bvec(data, bvec);
  Cyc_check_fixnum(data, start);
  Cyc_check_fixnum(data, end);

  bytevector bv = (bytevector) bvec;
  FILE *fp = ((port_type *) port)->fp;
  char *bytes = bv->data;
  int s = obj_obj2int(start);
  int e = obj_obj2int(end);

  if (s < 0) { 
    s = 0; 
  } else if (s > bv->len) { 
    s = bv->len; 
  }

  if (e < 0 || e > bv->len) {
    e = bv->len;
  }

  if (s > e) {
    s = e;
  }

  size_t rv = fwrite(
    bytes + s,
    sizeof(char), e - s, fp);
  return obj_int2obj(rv);
}

/* Fast versions of member and assoc */
object memberp(void *data, object x, list l)
{
  for (; l != NULL; l = cdr(l)) {
    Cyc_check_pair_or_null(data, l);
    if (boolean_f != equalp(x, car(l)))
      return l;
  }
  return boolean_f;
}

object memqp(void *data, object x, list l)
{
  for (; l != NULL; l = cdr(l)) {
    Cyc_check_pair_or_null(data, l);
    if (x == car(l))
      return l;
  }
  return boolean_f;
}

list assq(void *data, object x, list l)
{
  if ((l == NULL) || is_value_type(l) || type_of(l) != pair_tag)
    return boolean_f;
  for (; (l != NULL); l = cdr(l)) {
    Cyc_check_pair(data, l);
    list la = car(l);
    Cyc_check_pair(data, la);
    if (x == car(la))
      return la;
  }
  return boolean_f;
}

list assoc(void *data, object x, list l)
{
  if ((l == NULL) || is_value_type(l) || type_of(l) != pair_tag)
    return boolean_f;
  for (; (l != NULL); l = cdr(l)) {
    Cyc_check_pair(data, l);
    list la = car(l);
    Cyc_check_pair(data, la);
    if (boolean_f != equalp(x, car(la)))
      return la;
  }
  return boolean_f;
}

/**
 * Same as assoc but check the cdr of each item for equality
 */
list assoc_cdr(void *data, object x, list l)
{
  if ((l == NULL) || is_value_type(l) || type_of(l) != pair_tag)
    return boolean_f;
  for (; (l != NULL); l = cdr(l)) {
    list la = car(l);
    Cyc_check_pair(data, la);
    if (boolean_f != equalp(x, cdr(la)))
      return la;
  }
  return boolean_f;
}
/* END member and assoc */

object Cyc_fast_list_2(object ptr, object a1, object a2) 
{
  list_2_type *l = (list_2_type *)ptr;
  set_pair( ((pair)(&(l->b))), a2, NULL);
  set_pair( ((pair)(&(l->a))), a1, ((pair)(&(l->b))));
  return ptr;
}

object Cyc_fast_list_3(object ptr, object a1, object a2, object a3) 
{
  list_3_type *l = (list_3_type *)ptr;
  set_pair( ((pair)(&(l->c))), a3, NULL);
  set_pair( ((pair)(&(l->b))), a2, ((pair)(&(l->c))));
  set_pair( ((pair)(&(l->a))), a1, ((pair)(&(l->b))));
  return ptr;
}

object Cyc_fast_list_4(object ptr, object a1, object a2, object a3, object a4) 
{
  list_4_type *l = (list_4_type *)ptr;
  set_pair( ((pair)(&(l->d))), a4, NULL);
  set_pair( ((pair)(&(l->c))), a3, ((pair)(&(l->d))));
  set_pair( ((pair)(&(l->b))), a2, ((pair)(&(l->c))));
  set_pair( ((pair)(&(l->a))), a1, ((pair)(&(l->b))));
  return ptr;
}

object Cyc_fast_vector_2(object ptr, object a1, object a2) 
{
  vector_2_type *v = (vector_2_type *)ptr;
  v->v.hdr.mark = gc_color_red; 
  v->v.hdr.grayed = 0; 
  v->v.hdr.immutable = 0; 
  v->v.tag = vector_tag; 
  v->v.num_elements = 2; 
  v->v.elements = v->arr;
  v->v.elements[0] = a1;
  v->v.elements[1] = a2;
  return ptr;
}

object Cyc_fast_vector_3(object ptr, object a1, object a2, object a3) 
{
  vector_3_type *v = (vector_3_type *)ptr;
  v->v.hdr.mark = gc_color_red; 
  v->v.hdr.grayed = 0; 
  v->v.hdr.immutable = 0; 
  v->v.tag = vector_tag; 
  v->v.num_elements = 3; 
  v->v.elements = v->arr;
  v->v.elements[0] = a1;
  v->v.elements[1] = a2;
  v->v.elements[2] = a3;
  return ptr;
}

object Cyc_fast_vector_4(object ptr, object a1, object a2, object a3, object a4) 
{
  vector_4_type *v = (vector_4_type *)ptr;
  v->v.hdr.mark = gc_color_red; 
  v->v.hdr.grayed = 0; 
  v->v.hdr.immutable = 0; 
  v->v.tag = vector_tag; 
  v->v.num_elements = 4; 
  v->v.elements = v->arr;
  v->v.elements[0] = a1;
  v->v.elements[1] = a2;
  v->v.elements[2] = a3;
  v->v.elements[3] = a4;
  return ptr;
}

object Cyc_fast_vector_5(object ptr, object a1, object a2, object a3, object a4, object a5) 
{
  vector_5_type *v = (vector_5_type *)ptr;
  v->v.hdr.mark = gc_color_red; 
  v->v.hdr.grayed = 0; 
  v->v.hdr.immutable = 0; 
  v->v.tag = vector_tag; 
  v->v.num_elements = 5; 
  v->v.elements = v->arr;
  v->v.elements[0] = a1;
  v->v.elements[1] = a2;
  v->v.elements[2] = a3;
  v->v.elements[3] = a4;
  v->v.elements[4] = a5;
  return ptr;
}

// Internal function, do not use this anywhere outside the runtime
object Cyc_heap_alloc_port(void *data, port_type *stack_p)
{
  object p = NULL;
  int heap_grown;
  p = gc_alloc(((gc_thread_data *)data)->heap, 
               sizeof(port_type),
               (char *)stack_p,
               (gc_thread_data *)data, 
               &heap_grown);
  return p;
}

/**
 * Check two objects for deep equality
 */
object equalp(object x, object y)
{
  int second_cycle = 0;
  object slow_lis = x, fast_lis = NULL;
  object pcar_x = &second_cycle, pcar_y = &second_cycle; // never a car value

  if (Cyc_is_pair(x) == boolean_t &&
      Cyc_is_pair(cdr(x)) == boolean_t){
    fast_lis = cdr(x);
  }

  for (;; x = cdr(x), y = cdr(y)) {
    if (equal(x, y))
      return boolean_t;
    if (is_value_type(x) || is_value_type(y) ||
        (x == NULL) || (y == NULL) ||
        type_of(x) != pair_tag || type_of(y) != pair_tag)
      return boolean_f;

    // Both objects are lists at this point, compare cars
    if (pcar_x == car(x) &&
        pcar_y == car(y)) {
      // do nothing, already equal
    } else {
      if (boolean_f == equalp(car(x), car(y)))
        return boolean_f;
      pcar_x = car(x);
      pcar_y = car(y);
    }

    // If there is no cycle, keep checking equality
    if (fast_lis == NULL ||
        Cyc_is_pair(fast_lis) == boolean_f ||
        cdr(fast_lis) == NULL ||
        Cyc_is_pair(cdr(fast_lis)) == boolean_f ||
        cddr(fast_lis) == NULL){
      continue;
    }

    // If there is a cycle, handle it
    if (slow_lis == fast_lis) {
      // if this is y, both lists have cycles and are equal, return #t
      if (second_cycle)
        return boolean_t;
      // if this is x, keep going and check for a cycle in y
      second_cycle = 1;
      slow_lis = y;
      fast_lis = NULL;
      if (Cyc_is_pair(y) == boolean_t) {
        fast_lis = cdr(y);
      }
      continue;
    }
    slow_lis = cdr(slow_lis);
    fast_lis = cddr(fast_lis);
  }
}

object Cyc_num_cmp_va_list(void *data, int argc,
                           int (fn_op(void *, object, object)), object n,
                           va_list ns)
{
  int i;
  object next;

  if (argc < 2) {
    Cyc_rt_raise_msg(data, "Not enough arguments for boolean operator\n");
  }
  Cyc_check_num(data, n);

  for (i = 1; i < argc; i++) {
    next = va_arg(ns, object);
    Cyc_check_num(data, next);
    if (!fn_op(data, n, next)) {
      return boolean_f;
    }
    n = next;
  }

  return boolean_t;
}

// Convert a bignum back to fixnum if possible
object Cyc_bignum_normalize(void *data, object n)
{
  mp_int bn;
  object result;
  int i;
  if (!is_object_type(n) || type_of(n) != bignum_tag) {
    return n;
  }

  BIGNUM_CALL(mp_init(&bn));
  mp_set_ul(&bn, CYC_FIXNUM_MAX);
  if (mp_cmp_mag(&bignum_value(n), &bn) == MP_GT) {
    result = n;
  } else {
    i = mp_get_i32(&bignum_value(n));
    result = obj_int2obj(i);
  }
  mp_clear(&bn);
  return result;
}

void Cyc_int2bignum(int n, mp_int *bn)
{
  mp_set_ul(bn, abs(n));
  if (n < 0) { 
    BIGNUM_CALL(mp_neg(bn, bn));
  }
}

int Cyc_bignum_cmp(bn_cmp_type type, object x, int tx, object y, int ty)
{
  mp_int tmp;
  int cmp = 0;

  if (tx == bignum_tag && ty == bignum_tag) {
    cmp = mp_cmp(&bignum_value(x), &bignum_value(y));
  } else if (tx == bignum_tag && ty == -1) { \
    // JAE TODO: make a macro out of this, and use for other BN calls
    mp_init(&tmp) ? fprintf(stderr, "Error initializing bignum"), exit(1) : 0;
    Cyc_int2bignum(obj_obj2int(y), &tmp);
    cmp = mp_cmp(&bignum_value(x), &tmp);
    mp_clear(&tmp);
  } else if (tx == -1 && ty == bignum_tag) { \
    BIGNUM_CALL(mp_init(&tmp));
    Cyc_int2bignum(obj_obj2int(x), &tmp);
    cmp = mp_cmp(&tmp, &bignum_value(y));
    mp_clear(&tmp);
  } else {
    return 0;
  }

  return (cmp == type) ||
         ((type == CYC_BN_GTE && cmp > MP_LT) ||
          (type == CYC_BN_LTE && cmp < MP_GT));
}

#define declare_num_cmp(FUNC, FUNC_OP, FUNC_FAST_OP, FUNC_APPLY, OP, BN_CMP) \
int FUNC_OP(void *data, object x, object y) { \
    int result = 0, \
        tx = (obj_is_int(x) ? -1 : type_of(x)), \
        ty = (obj_is_int(y) ? -1 : type_of(y)); \
    if (tx == -1 && ty == -1) { \
      result = (obj_obj2int(x)) OP (obj_obj2int(y)); \
    } else if (tx == -1 && ty == integer_tag) { \
      result = (obj_obj2int(x)) OP (integer_value(y)); \
    } else if (tx == -1 && ty == double_tag) { \
      result = (obj_obj2int(x)) OP (double_value(y)); \
    } else if (tx == integer_tag && ty == -1) { \
      result = (integer_value(x)) OP (obj_obj2int(y)); \
    } else if (tx == integer_tag && ty == integer_tag) { \
      result = (integer_value(x)) OP (integer_value(y)); \
    } else if (tx == integer_tag && ty == double_tag) { \
      result = (integer_value(x)) OP (double_value(y)); \
    } else if (tx == double_tag && ty == -1) { \
      result = (double_value(x)) OP (obj_obj2int(y)); \
    } else if (tx == double_tag && ty == integer_tag) { \
      result = (double_value(x)) OP (integer_value(y)); \
    } else if (tx == double_tag && ty == double_tag) { \
      result = (double_value(x)) OP (double_value(y)); \
    } else if (tx == bignum_tag && ty == -1) { \
      result = Cyc_bignum_cmp(BN_CMP, x, tx, y, ty); \
    } else if (tx == bignum_tag && ty == double_tag) { \
      result = mp_get_double(&bignum_value(x)) OP (double_value(y)); \
    } else if (tx == bignum_tag && ty == bignum_tag) { \
      result = Cyc_bignum_cmp(BN_CMP, x, tx, y, ty); \
    } else if (tx == -1 && ty == bignum_tag) { \
      result = Cyc_bignum_cmp(BN_CMP, x, tx, y, ty); \
    } else if (tx == double_tag && ty == bignum_tag) { \
      result = (double_value(x)) OP mp_get_double(&bignum_value(y)); \
    } else if (tx == complex_num_tag && ty == complex_num_tag) { \
      result = (complex_num_value(x)) == (complex_num_value(y)); \
    } else if (tx == complex_num_tag && ty != complex_num_tag) { \
    } else if (tx != complex_num_tag && ty == complex_num_tag) { \
    } else { \
        make_string(s, "Bad argument type"); \
        make_pair(c1, y, NULL); \
        make_pair(c0, &s, &c1); \
        Cyc_rt_raise(data, &c0); \
    } \
    return result; \
} \
object FUNC(void *data, object cont, int argc, object n, ...) { \
    object result; \
    va_list ap; \
    va_start(ap, n); \
    result = Cyc_num_cmp_va_list(data, argc, FUNC_OP, n, ap); \
    va_end(ap); \
    _return_closcall1(data, cont, result); \
} \
void FUNC_APPLY(void *data, int argc, object clo, object cont, object n, ...) { \
    object result; \
    va_list ap; \
    va_start(ap, n); \
    result = Cyc_num_cmp_va_list(data, argc - 1, FUNC_OP, n, ap); \
    va_end(ap); \
    return_closcall1(data, cont, result); \
} \
object FUNC_FAST_OP(void *data, object x, object y) { \
    int tx, ty; \
    if (obj_is_int(x)) { \
      tx = -1; \
    } else if (is_object_type(x)) { \
      tx = type_of(x); \
    } else { \
      goto bad_arg_type_error; \
    } \
    if (obj_is_int(y)) { \
      ty = -1; \
    } else if (is_object_type(y)) { \
      ty = type_of(y); \
    } else { \
      goto bad_arg_type_error; \
    } \
    if (tx == -1 && ty == -1) { \
      return ((obj_obj2int(x)) OP (obj_obj2int(y))) \
             ? boolean_t : boolean_f; \
    } else if (tx == -1 && ty == integer_tag) { \
      return ((obj_obj2int(x)) OP (integer_value(y))) \
             ? boolean_t : boolean_f; \
    } else if (tx == -1 && ty == double_tag) { \
      return ((obj_obj2int(x)) OP (double_value(y))) \
             ? boolean_t : boolean_f; \
    } else if (tx == integer_tag && ty == -1) { \
      return ((integer_value(x)) OP (obj_obj2int(y))) \
             ? boolean_t : boolean_f; \
    } else if (tx == integer_tag && ty == integer_tag) { \
      return ((integer_value(x)) OP (integer_value(y))) \
             ? boolean_t : boolean_f; \
    } else if (tx == integer_tag && ty == double_tag) { \
      return ((integer_value(x)) OP (double_value(y))) \
             ? boolean_t : boolean_f; \
    } else if (tx == double_tag && ty == -1) { \
      return ((double_value(x)) OP (obj_obj2int(y))) \
             ? boolean_t : boolean_f; \
    } else if (tx == double_tag && ty == integer_tag) { \
      return ((double_value(x)) OP (integer_value(y))) \
             ? boolean_t : boolean_f; \
    } else if (tx == double_tag && ty == double_tag) { \
      return ((double_value(x)) OP (double_value(y))) \
             ? boolean_t : boolean_f; \
    } else if (tx == bignum_tag && ty == -1) { \
      return Cyc_bignum_cmp(BN_CMP, x, tx, y, ty) ? boolean_t : boolean_f; \
    } else if (tx == bignum_tag && ty == double_tag) { \
      return mp_get_double(&bignum_value(x)) OP (double_value(y)) ? boolean_t : boolean_f; \
    } else if (tx == bignum_tag && ty == bignum_tag) { \
      return Cyc_bignum_cmp(BN_CMP, x, tx, y, ty) ? boolean_t : boolean_f; \
    } else if (tx == -1         && ty == bignum_tag) { \
      return Cyc_bignum_cmp(BN_CMP, x, tx, y, ty) ? boolean_t : boolean_f; \
    } else if (tx == double_tag && ty == bignum_tag) { \
      return (double_value(x)) OP mp_get_double(&bignum_value(y)) ? boolean_t : boolean_f; \
    } else if (tx == complex_num_tag && ty == complex_num_tag) { \
      return ((complex_num_value(x)) == (complex_num_value(y))) ? boolean_t : boolean_f; \
    } else if (tx == complex_num_tag && ty != complex_num_tag) { \
      return boolean_f; \
    } else if (tx != complex_num_tag && ty == complex_num_tag) { \
      return boolean_f; \
    } else { \
        goto bad_arg_type_error; \
    } \
    return NULL; \
bad_arg_type_error: \
    { \
        make_string(s, "Bad argument type"); \
        make_pair(c2, y, NULL); \
        make_pair(c1, x, &c2); \
        make_pair(c0, &s, &c1); \
        Cyc_rt_raise(data, &c0); \
        return NULL; \
    } \
}

declare_num_cmp(Cyc_num_eq,  Cyc_num_eq_op,  Cyc_num_fast_eq_op, dispatch_num_eq, ==, CYC_BN_EQ);
declare_num_cmp(Cyc_num_gt,  Cyc_num_gt_op,  Cyc_num_fast_gt_op, dispatch_num_gt, >, CYC_BN_GT);
declare_num_cmp(Cyc_num_lt,  Cyc_num_lt_op,  Cyc_num_fast_lt_op, dispatch_num_lt, <, CYC_BN_LT);
declare_num_cmp(Cyc_num_gte, Cyc_num_gte_op, Cyc_num_fast_gte_op, dispatch_num_gte, >=, CYC_BN_GTE);
declare_num_cmp(Cyc_num_lte, Cyc_num_lte_op, Cyc_num_fast_lte_op, dispatch_num_lte, <=, CYC_BN_LTE);

//object Cyc_is_boolean(object o)
//{
//  if ((o != NULL) &&
//      !is_value_type(o) &&
//      ((list) o)->tag == boolean_tag && ((boolean_f == o) || (boolean_t == o)))
//    return boolean_t;
//  return boolean_f;
//}
//
//object Cyc_is_pair(object o)
//{
//  if (is_object_type(o) && ((list) o)->tag == pair_tag)
//    return boolean_t;
//  return boolean_f;
//}
//
//object Cyc_is_null(object o)
//{
//  if (o == NULL)
//    return boolean_t;
//  return boolean_f;
//}

object Cyc_is_number(object o)
{
  if ((o != NULL) && (obj_is_int(o) || (!is_value_type(o)
                                        && (type_of(o) == integer_tag
                                            || type_of(o) == bignum_tag
                                            || type_of(o) == double_tag
                                            || type_of(o) == complex_num_tag))))
    return boolean_t;
  return boolean_f;
}

object Cyc_is_real(object o)
{
  if ((o != NULL) && (obj_is_int(o) || 
      (!is_value_type(o) && (type_of(o) == integer_tag
                          || type_of(o) == bignum_tag
                          || type_of(o) == double_tag
                          || (type_of(o) == complex_num_tag &&
                              cimag(complex_num_value(o)) == 0.0))))) // Per R7RS
    return boolean_t;
  return boolean_f;
}

//object Cyc_is_complex(object o)
//{
//  if ((o != NULL) && !is_value_type(o) && ((list) o)->tag == complex_num_tag)
//    return boolean_t;
//  return boolean_f;
//}

//object Cyc_is_fixnum(object o)
//{
//  if (obj_is_int(o))
//    return boolean_t;
//  return boolean_f;
//}

object Cyc_is_integer(object o)
{
  if ((o != NULL) && (obj_is_int(o) ||
      (!is_value_type(o) && type_of(o) == integer_tag) ||
      (!is_value_type(o) && type_of(o) == bignum_tag)
   || (!is_value_type(o) && type_of(o) == double_tag && double_value(o) == round(double_value(o)))
      )) // Per R7RS
    return boolean_t;
  return boolean_f;
}

//object Cyc_is_bignum(object o)
//{
//  if ((o != NULL) && !is_value_type(o) && ((list) o)->tag == bignum_tag)
//    return boolean_t;
//  return boolean_f;
//}
//
//object Cyc_is_symbol(object o)
//{
//  if ((o != NULL) && !is_value_type(o) && ((list) o)->tag == symbol_tag)
//    return boolean_t;
//  return boolean_f;
//}
//
//object Cyc_is_vector(object o)
//{
//  if ((o != NULL) && !is_value_type(o) && ((list) o)->tag == vector_tag)
//    return boolean_t;
//  return boolean_f;
//}
//
//object Cyc_is_bytevector(object o)
//{
//  if ((o != NULL) && !is_value_type(o) && ((list) o)->tag == bytevector_tag)
//    return boolean_t;
//  return boolean_f;
//}
//
//object Cyc_is_port(object o)
//{
//  if ((o != NULL) && !is_value_type(o) && ((list) o)->tag == port_tag)
//    return boolean_t;
//  return boolean_f;
//}
//
//object Cyc_is_mutex(object o)
//{
//  if ((o != NULL) && !is_value_type(o) && ((list) o)->tag == mutex_tag)
//    return boolean_t;
//  return boolean_f;
//}
//
//object Cyc_is_cond_var(object o)
//{
//  if ((o != NULL) && !is_value_type(o) && ((list) o)->tag == cond_var_tag)
//    return boolean_t;
//  return boolean_f;
//}
//
//object Cyc_is_string(object o)
//{
//  if ((o != NULL) && !is_value_type(o) && ((list) o)->tag == string_tag)
//    return boolean_t;
//  return boolean_f;
//}
//
//object Cyc_is_char(object o)
//{
//  if (obj_is_char(o))
//    return boolean_t;
//  return boolean_f;
//}

object Cyc_is_procedure(void *data, object o)
{
  int tag;
  if ((o != NULL) && !is_value_type(o)) {
    tag = type_of(o);
    if (tag == closure0_tag ||
        tag == closure1_tag || tag == closureN_tag || tag == primitive_tag) {
      return boolean_t;
    } else if (tag == pair_tag) {
      int i = obj_obj2int(Cyc_length(data, o));
      if (i > 0 && Cyc_is_symbol(car(o)) == boolean_t) {
        if (strncmp(((symbol) car(o))->desc, "primitive", 10) == 0 ||
            strncmp(((symbol) car(o))->desc, "procedure", 10) == 0) {
          return boolean_t;
        }
      }
    }
  }
  return boolean_f;
}

//object Cyc_is_macro(object o)
//{
//  int tag;
//  if ((o != NULL) && !is_value_type(o)) {
//    tag = type_of(o);
//    if (tag == macro_tag) {
//      return boolean_t;
//    }
//  }
//  return boolean_f;
//}
//
//object Cyc_is_eof_object(object o)
//{
//  if ((o != NULL) && !is_value_type(o) && type_of(o) == eof_tag)
//    return boolean_t;
//  return boolean_f;
//}
//
//object Cyc_is_cvar(object o)
//{
//  if ((o != NULL) && !is_value_type(o) && type_of(o) == cvar_tag)
//    return boolean_t;
//  return boolean_f;
//}
//
//object Cyc_is_opaque(object o)
//{
//  if ((o != NULL) && !is_value_type(o) && ((list) o)->tag == c_opaque_tag)
//    return boolean_t;
//  return boolean_f;
//}
//
//object Cyc_eq(object x, object y)
//{
//  if (x == y)
//    return boolean_t;
//  return boolean_f;
//}

object Cyc_eqv(object x, object y)
{
  if (Cyc_eq(x, y) == boolean_t) {
    return boolean_t;
  } else if (Cyc_is_number(x) == boolean_t &&
             equalp(x, y) == boolean_t) {
    return boolean_t;
  } else { 
    return boolean_f;
  }
}

object Cyc_is_immutable(object obj)
{
  if (is_object_type(obj) &&
      (type_of(obj) == pair_tag ||
       type_of(obj) == vector_tag ||
       type_of(obj) == bytevector_tag ||
       type_of(obj) == string_tag
      ) &&
      !immutable(obj) ) {
    return boolean_f;
  }
  return boolean_t;
}

object Cyc_set_cell(void *data, object l, object val)
{
  // FUTURE: always use "unsafe" car here, since set-cell is added by cyclone
  gc_mut_update((gc_thread_data *) data, car(l), val);
  car(l) = val;
  add_mutation(data, l, -1, val);
  return l;
}

object Cyc_set_car(void *data, object l, object val)
{
  if (Cyc_is_pair(l) == boolean_f) {
    Cyc_invalid_type_error(data, pair_tag, l);
  }
  Cyc_verify_mutable(data, l);
  gc_mut_update((gc_thread_data *) data, car(l), val);
  car(l) = val;
  add_mutation(data, l, -1, val);
  return l;
}

object Cyc_set_cdr(void *data, object l, object val)
{
  if (Cyc_is_pair(l) == boolean_f) {
    Cyc_invalid_type_error(data, pair_tag, l);
  }
  Cyc_verify_mutable(data, l);
  gc_mut_update((gc_thread_data *) data, cdr(l), val);
  cdr(l) = val;
  add_mutation(data, l, -1, val);
  return l;
}

object Cyc_vector_set(void *data, object v, object k, object obj)
{
  int idx;
  Cyc_check_vec(data, v);
  Cyc_check_fixnum(data, k);
  Cyc_verify_mutable(data, v);
  idx = unbox_number(k);

  if (idx < 0 || idx >= ((vector) v)->num_elements) {
    Cyc_rt_raise2(data, "vector-set! - invalid index", k);
  }

  gc_mut_update((gc_thread_data *) data, ((vector) v)->elements[idx], obj);

  ((vector) v)->elements[idx] = obj;
  add_mutation(data, v, idx, obj);
  return v;
}

object Cyc_vector_set_unsafe(void *data, object v, object k, object obj)
{
  int idx = unbox_number(k);
  gc_mut_update((gc_thread_data *) data, ((vector) v)->elements[idx], obj);
  ((vector) v)->elements[idx] = obj;
  add_mutation(data, v, idx, obj);
  return v;
}

// Prevent the possibility of a race condition by doing the actual mutation
// after all relevant objects have been relocated to the heap
static void Cyc_set_car_cps_gc_return(void *data, int argc, object cont, object l, object val, object next)
{
  car(l) = val;
  closcall1(data, (closure)next, l);
}

object Cyc_set_car_cps(void *data, object cont, object l, object val)
{
  if (Cyc_is_pair(l) == boolean_f) {
    Cyc_invalid_type_error(data, pair_tag, l);
  }
  Cyc_verify_mutable(data, l);

  // Alternate write barrier
  int do_gc = 0;
  val = transport_stack_value(data, l, val, &do_gc);
  gc_mut_update((gc_thread_data *) data, car(l), val);
  add_mutation(data, l, -1, val); // Ensure val is transported
  if (do_gc) { // GC and then do assignment
    mclosure0(clo, (function_type)Cyc_set_car_cps_gc_return);
    object buf[3]; buf[0] = l; buf[1] = val; buf[2] = cont;
    GC(data, &clo, buf, 3);
    return NULL;
  } else {
    car(l) = val; // Assign now since we have heap objects
    return l;
  }
}

static void Cyc_set_cdr_cps_gc_return(void *data, int argc, object cont, object l, object val, object next)
{
  cdr(l) = val;
  closcall1(data, (closure)next, l);
}

object Cyc_set_cdr_cps(void *data, object cont, object l, object val)
{
  if (Cyc_is_pair(l) == boolean_f) {
    Cyc_invalid_type_error(data, pair_tag, l);
  }
  Cyc_verify_mutable(data, l);

  // Alternate write barrier
  int do_gc = 0;
  val = transport_stack_value(data, l, val, &do_gc);

  gc_mut_update((gc_thread_data *) data, cdr(l), val);
  add_mutation(data, l, -1, val); // Ensure val is transported
  if (do_gc) { // GC and then to assignment
    mclosure0(clo, (function_type)Cyc_set_cdr_cps_gc_return);
    object buf[3]; buf[0] = l; buf[1] = val; buf[2] = cont;
    GC(data, &clo, buf, 3);
    return NULL;
  } else {
    cdr(l) = val; // Assign now since we have heap objects
    return l;
  }
}

static void Cyc_vector_set_cps_gc_return(void *data, int argc, object cont, object vec, object idx, object val, object next)
{
  int i = obj_obj2int(idx);
  ((vector) vec)->elements[i] = val;
  closcall1(data, (closure)next, vec);
}

object Cyc_vector_set_cps(void *data, object cont, object v, object k, object obj)
{
  int idx;
  Cyc_check_vec(data, v);
  Cyc_check_fixnum(data, k);
  Cyc_verify_mutable(data, v);
  idx = unbox_number(k);

  if (idx < 0 || idx >= ((vector) v)->num_elements) {
    Cyc_rt_raise2(data, "vector-set! - invalid index", k);
  }

  int do_gc = 0;
  obj = transport_stack_value(data, v, obj, &do_gc);

  gc_mut_update((gc_thread_data *) data, ((vector) v)->elements[idx], obj);
  add_mutation(data, v, idx, obj);
  if (do_gc) { // GC and then do assignment
    mclosure0(clo, (function_type)Cyc_vector_set_cps_gc_return);
    object buf[4]; buf[0] = v; buf[1] = k; buf[2] = obj; buf[3] = cont;
    GC(data, &clo, buf, 4);
    return NULL;
  } else {
    ((vector) v)->elements[idx] = obj; // Assign now since we have heap objs
    return v; // Let caller pass this to cont
  }
}

object Cyc_vector_set_unsafe_cps(void *data, object cont, object v, object k, object obj)
{
  int idx = unbox_number(k);
  int do_gc = 0;
  obj = transport_stack_value(data, v, obj, &do_gc);
  gc_mut_update((gc_thread_data *) data, ((vector) v)->elements[idx], obj);
  add_mutation(data, v, idx, obj);
  if (do_gc) { // GC and then do assignment
    mclosure0(clo, (function_type)Cyc_vector_set_cps_gc_return);
    object buf[4]; buf[0] = v; buf[1] = k; buf[2] = obj; buf[3] = cont;
    GC(data, &clo, buf, 4);
    return NULL;
  } else {
    ((vector) v)->elements[idx] = obj; // Assign now since we have heap objs
    return v;
  }
}

object Cyc_vector_ref(void *data, object v, object k)
{
  int idx;
  Cyc_check_vec(data, v);
  Cyc_check_fixnum(data, k);

  idx = unbox_number(k);
  if (idx < 0 || idx >= ((vector) v)->num_elements) {
    Cyc_rt_raise2(data, "vector-ref - invalid index", obj_int2obj(idx));
  }

  return ((vector) v)->elements[idx];
}

object _unsafe_Cyc_vector_ref(object v, object k)
{
  int idx;
  if (Cyc_is_vector(v) == boolean_f ||
      Cyc_is_fixnum(k) == boolean_f)
  {
    return NULL;
  }

  idx = unbox_number(k);
  if (idx < 0 || idx >= ((vector) v)->num_elements) {
    return NULL;
  }

  return ((vector) v)->elements[idx];
}

object Cyc_vector_length(void *data, object v)
{
  if ((v != NULL) && !is_value_type(v) && ((list) v)->tag == vector_tag) {
    return obj_int2obj(((vector) v)->num_elements);
  }
  Cyc_rt_raise_msg(data,
                   "vector-length - invalid parameter, expected vector\n");
  return NULL;
}

object Cyc_length(void *data, object l)
{
  int len = 0;
  while ((l != NULL)) {
    if (is_value_type(l) || ((list) l)->tag != pair_tag) {
      Cyc_rt_raise2(data, "length - invalid parameter, expected list", l);
    }
    l = cdr(l);
    len++;
  }
  return obj_int2obj(len);
}

object Cyc_length_unsafe(void *data, object l)
{
  int len = 0;
  while (l != NULL) {
    l = cdr(l);
    len++;
  }
  return obj_int2obj(len);
}

char *int_to_binary(char *b, int x)
{
  unsigned int i = 0x80000000, leading_zeros = 1;
  if (x == 0) {
    *b++ = '0';
    *b = '\0';
    return b;
  }

  while (i){
    if (x & i) {
      *b++ = '1';
      leading_zeros = 0;
    } else if (!leading_zeros) {
      *b++ = '0';
    }
    i >>= 1;
  }
  *b = '\0';
  return b;
}

object Cyc_number2string2(void *data, object cont, int argc, object n, ...)
{
  object base = NULL;
  int base_num = 10, val, sz;
  char buffer[1024];
  size_t written;
  va_list ap;
  va_start(ap, n);
  if (argc > 1) {
    base = va_arg(ap, object);
    Cyc_check_num(data, base);
  }
  va_end(ap);
  Cyc_check_num(data, n);
  if (base) {
    base_num = unbox_number(base);
  }

  if (is_object_type(n) && type_of(n) == bignum_tag) {
    if (base_num > 64 || base_num < 2) {
      Cyc_rt_raise2(data, "number->string - invalid radix for bignum", base);
    }
    BIGNUM_CALL(mp_radix_size(&bignum_value(n), base_num, &sz));
    if (sz > 1024) {
      // TODO: just temporary, need to handle this better
      Cyc_rt_raise2(data, "number->string - bignum is too large to convert", n);
    }
    BIGNUM_CALL(mp_to_radix(&bignum_value(n), buffer, 1024, &written, base_num));
  } else {
    if (base_num == 2) {
      val = obj_is_int(n) ?
          obj_obj2int(n) :
          type_of(n) == integer_tag ? integer_value(n) : ((int)double_value(n));
      int_to_binary(buffer, val);
    } else if (base_num == 8) {
      val = obj_is_int(n) ?
          obj_obj2int(n) :
          type_of(n) == integer_tag ? integer_value(n) : ((int)double_value(n));
      snprintf(buffer, 1024, "%o", val);
    } else if (base_num == 16) {
      val = obj_is_int(n) ?
          obj_obj2int(n) :
          type_of(n) == integer_tag ? integer_value(n) : ((int)double_value(n));
      snprintf(buffer, 1024, "%X", val);
    } else {
      if (obj_is_int(n)) {
        snprintf(buffer, 1024, "%ld", obj_obj2int(n));
      } else if (type_of(n) == integer_tag) {
        snprintf(buffer, 1024, "%d", ((integer_type *) n)->value);
      } else if (type_of(n) == double_tag) {
        double2buffer(buffer, 1024, ((double_type *) n)->value);
      } else if (type_of(n) == complex_num_tag) {
        char rbuf[33], ibuf[33];
        const char *plus="+", *empty="";
        double dreal = creal(((complex_num_type *) n)->value);
        double dimag = cimag(((complex_num_type *) n)->value);
        double2buffer(rbuf, 32, dreal);
        double2buffer(ibuf, 32, dimag);
        if (dreal == 0.0) {
          snprintf(buffer, 1024, "%si", ibuf);
        } else {
          snprintf(buffer, 1024, "%s%s%si", 
            rbuf, 
            (dimag < 0.0) ? empty : plus,
            ibuf);
        }
      } else {
        Cyc_rt_raise2(data, "number->string - Unexpected object", n);
      }
    }
  }
  make_string(str, buffer);
  _return_closcall1(data, cont, &str);
}

object Cyc_symbol2string(void *data, object cont, object sym)
{
  Cyc_check_sym(data, sym);
  {
    const char *desc = symbol_desc(sym);
    make_utf8_string(data, str, desc);
    _return_closcall1(data, cont, &str);
}}

object Cyc_string2symbol(void *data, object str)
{
  object sym;
  Cyc_check_str(data, str);
  sym = find_symbol_by_name(string_str(str));
  if (!sym) {
    sym = add_symbol_by_name(string_str(str));
  }
  return sym;
}

object Cyc_list2string(void *data, object cont, object lst)
{
  char *buf, cbuf[5];
  int i = 0, len = 0, num_cp = 0;
  object cbox, tmp = lst;
  char_type ch;

  Cyc_check_pair_or_null(data, lst);

  // Need to walk the list of chars to compute multibyte length
  while (tmp) {
    if (is_value_type(tmp) || ((list) tmp)->tag != pair_tag) {
      Cyc_rt_raise2(data, "length - invalid parameter, expected list", tmp);
    }
    cbox = car(tmp);
    ch = obj_obj2char(cbox);
    if (!obj_is_char(cbox)) {
      Cyc_rt_raise2(data, "Expected character but received", cbox);
    }
    if (!ch) {
      len++;
      num_cp++; // Failsafe?
    } else {
      Cyc_utf8_encode_char(cbuf, 5, ch);
      len += strlen(cbuf);
      num_cp++;
    }
    tmp = cdr(tmp);
  }

  {
    object str;
    alloc_string(data, str, len, num_cp);
    buf = ((string_type *)str)->str;
    while ((lst != NULL)) {
      cbox = car(lst);
      ch = obj_obj2char(cbox); // Already validated, can assume chars now
      if (!ch) {
        i++;
      } else {
        Cyc_utf8_encode_char(&(buf[i]), 5, ch);
        i += strlen(buf+i);
      }
      lst = cdr(lst);
    }
    buf[i] = '\0';
    _return_closcall1(data, cont, str);
  }
}

object Cyc_list(void *data, int argc, object cont, ...) 
{
  load_varargs(objs, cont, argc);
  //Cyc_st_add(data, "Cyc-list");
  _return_closcall1(data, cont, cdr(objs));
}

object Cyc_string2number2_(void *data, object cont, int argc, object str, ...)
{
  object base = NULL;
  int base_num, result;
  va_list ap;
  va_start(ap, str);
  if (argc > 1) {
    base = va_arg(ap, object);
    Cyc_check_num(data, base);
  }
  va_end(ap);
  if (base) {
    base_num = unbox_number(base);
    Cyc_check_str(data, str);
    result = -1;
    if (base_num == 2) {
      result = (int)strtol(string_str(str), NULL, 2);
    } else if (base_num == 8) {
      result = (int)strtol(string_str(str), NULL, 8);
    } else if (base_num == 10) {
      Cyc_string2number_(data, cont, str); // Default processing
    } else if (base_num == 16) {
      result = (int)strtol(string_str(str), NULL, 16);
    }

    if (result <= 0 || result > CYC_FIXNUM_MAX) {
      mp_int tmp;
      alloc_bignum(data, bn);
      if (MP_OKAY != mp_read_radix(&(bignum_value(bn)), string_str(str), base_num)) {
        Cyc_rt_raise2(data, "Error converting string to bignum", str);
      }

      // If result is mp_zero and str does not contain a 0, then fail
      BIGNUM_CALL(mp_init(&tmp));
      mp_zero(&tmp);
      if (MP_EQ == mp_cmp(&(bignum_value(bn)), &tmp) &&
          NULL == strchr(string_str(str), '0')) {
          _return_closcall1(data, cont, boolean_f);
      }

      _return_closcall1(data, cont, Cyc_bignum_normalize(data, bn));
    } else {
      _return_closcall1(data, cont, obj_int2obj(result));
    }
  }
  Cyc_string2number_(data, cont, str);
  return NULL;
}

typedef enum {
    STR2INT_SUCCESS,
    STR2INT_OVERFLOW,
    STR2INT_UNDERFLOW,
    STR2INT_INCONVERTIBLE
} str2int_errno;

/*
Convert string s to int out.

@param[out] out The converted int. Cannot be NULL.

@param[in] s Input string to be converted.

    The format is the same as strtol,
    except that the following are inconvertible:

    - empty string
    - leading whitespace
    - any trailing characters that are not part of the number

    Cannot be NULL.

@param[in] base Base to interpret string in. Same range as strtol (2 to 36).

@return Indicates if the operation succeeded, or why it failed.
*/
static str2int_errno str2int(int *out, char *s, int base) 
{
    char *end;
    if (s[0] == '\0' || isspace((unsigned char) s[0]))
        return STR2INT_INCONVERTIBLE;
    errno = 0;
    long l = strtol(s, &end, base);
    /* Both checks are needed because INT_MAX == LONG_MAX is possible. */
    if (l > CYC_FIXNUM_MAX /*INT_MAX*/ || (errno == ERANGE && l == LONG_MAX))
        return STR2INT_OVERFLOW;
    if (l < CYC_FIXNUM_MIN /*INT_MIN*/ || (errno == ERANGE && l == LONG_MIN))
        return STR2INT_UNDERFLOW;
    if (*end != '\0')
        return STR2INT_INCONVERTIBLE;
    *out = l;
    return STR2INT_SUCCESS;
}

int str_is_bignum(str2int_errno errnum, char *c)
{
  if (errnum == STR2INT_INCONVERTIBLE) return 0; // Unexpected chars for int

  for (;*c; c++) {
    if (!isdigit(*c) && *c != '-') {
      return 0;
    }
  }
  return 1;
}

object Cyc_string2number_(void *data, object cont, object str)
{
  int result, rv;
  double n;
  char *s;
  Cyc_check_str(data, str);
  if (type_of(str) == string_tag && ((string_type *) str)->str) {
    s = ((string_type *) str)->str;

    rv = str2int(&result, s, 10);
    if (rv == STR2INT_SUCCESS) {
      _return_closcall1(data, cont, obj_int2obj(result));
    } else if (str_is_bignum(rv, s)) {
      alloc_bignum(data, bn);
      if (MP_OKAY != mp_read_radix(&(bignum_value(bn)), s, 10)) {
        Cyc_rt_raise2(data, "Error converting string to bignum", str);
      }
      _return_closcall1(data, cont, bn);
    } else {
      char *str_end;
      n = strtold(s, &str_end);
      if (s != str_end && (*str_end == '\0' || isspace(*str_end))) {
        make_double(result, n);
        _return_closcall1(data, cont, &result);
      } else {
        _return_closcall1(data, cont, boolean_f);
      }
    }
  }

  Cyc_rt_raise2(data, "Expected string but received", str);
  return NULL;
}

int binstr2int(const char *str)
{
  int num = 0;
  while (*str) {
    num <<= 1;
    if (*str++ == '1')
      num++;
  }
  return num;
}

int octstr2int(const char *str)
{
  int num = 0;
  while (*str) {
    num <<= 3;
    num += ((*str++) - '0');
  }
  return num;
}

object Cyc_string_cmp(void *data, object str1, object str2)
{
  Cyc_check_str(data, str1);
  Cyc_check_str(data, str2);
  return obj_int2obj(strcmp(((string_type *) str1)->str,
                            ((string_type *) str2)->str));
}

#define Cyc_string_append_va_list(data, argc) { \
    int i = 0, total_cp = 0, total_len = 1; \
    int *len = alloca(sizeof(int) * argc); \
    char *buffer, *bufferp, **str = alloca(sizeof(char *) * argc); \
    object tmp; \
    if (argc > 0) { \
      Cyc_check_str(data, str1); \
      str[i] = ((string_type *)str1)->str; \
      len[i] = string_len((str1)); \
      total_len += len[i]; \
      total_cp += string_num_cp((str1)); \
    } \
    for (i = 1; i < argc; i++) { \
      tmp = va_arg(ap, object); \
      Cyc_check_str(data, tmp); \
      str[i] = ((string_type *)tmp)->str; \
      len[i] = string_len((tmp)); \
      total_len += len[i]; \
      total_cp += string_num_cp((tmp)); \
    } \
    buffer = bufferp = alloca(sizeof(char) * total_len); \
    for (i = 0; i < argc; i++) { \
        memcpy(bufferp, str[i], len[i]); \
        bufferp += len[i]; \
    } \
    *bufferp = '\0'; \
    make_string(result, buffer); \
    string_num_cp((&result)) = total_cp; \
    va_end(ap); \
    _return_closcall1(data, cont, &result); \
}

object dispatch_string_91append(void *data, int _argc, object clo, object cont,
                              object str1, ...)
{
  va_list ap;
  va_start(ap, str1);
  Cyc_string_append_va_list(data, _argc - 1);
}

object Cyc_string_append(void *data, object cont, int _argc, object str1, ...)
{
  va_list ap;
  va_start(ap, str1);
  Cyc_string_append_va_list(data, _argc);
}

object Cyc_string_length(void *data, object str)
{
  Cyc_check_str(data, str);
  return obj_int2obj(string_num_cp(str));
}

object Cyc_string_byte_length(void *data, object str)
{
  Cyc_check_str(data, str);
  return obj_int2obj(string_len(str));
}

object Cyc_string_set(void *data, object str, object k, object chr)
{
  char buf[5];
  char *raw;
  int idx, len, buf_len;
  char_type input_char;

  Cyc_check_str(data, str);
  Cyc_check_fixnum(data, k);
  Cyc_verify_mutable(data, str);

  if (boolean_t != Cyc_is_char(chr)) {
    Cyc_rt_raise2(data, "Expected char but received", chr);
  }

  input_char = obj_obj2char(chr);
  if (!input_char) {
    buf_len = 1;
  } else {
    Cyc_utf8_encode_char(buf, 5, input_char);
    buf_len = strlen(buf);
  }

  raw = string_str(str);
  idx = unbox_number(k);
  len = string_len(str);

  Cyc_check_bounds(data, "string-set!", len, idx);

  if (string_num_cp(str) == string_len(str) && buf_len == 1) {
    // Take fast path if all chars are just 1 byte
    raw[idx] = obj_obj2char(chr);
  } else {
    // Slower path for UTF-8, need to handle replacement differently 
    // depending upon how the new char affects length of the string
    char *tmp = raw, *this_cp = raw;
    char_type codepoint;
    uint32_t state = 0;
    int i = 0, count, prev_cp_bytes = 0, cp_idx;

    // Find index to change, and how many bytes it is
    for (count = 0; *tmp; ++tmp){
      prev_cp_bytes++;
      if (!Cyc_utf8_decode(&state, &codepoint, (uint8_t)*tmp)){
        if (count == idx) {
          break;
        }
        this_cp = tmp + 1;
        count += 1;
        prev_cp_bytes = 0;
      }
      i++;
    }
    cp_idx = i;
    if (state != CYC_UTF8_ACCEPT) {
       Cyc_rt_raise2(data, "string-set! - invalid character at index", k);
    }

    // Perform actual mutation
    //
    // Now we know length of start (both in codepoints and bytes),
    // and we know the codepoint to be replaced. by calculating its length
    // we can compute where the end portion starts, and by using str we can
    // figure out how many remaining bytes/codepoints are in end
    //
    // 3 cases: 
    // - 1) buf_len = prev_cp_bytes, just straight replace
    if (buf_len == prev_cp_bytes) {
      for (i = 0; i < buf_len; i++) {
        this_cp[i] = buf[i];
      }
    }
    // - 2) buf_len < prev_cp_bytes, replace and shift chars down
    else if (buf_len < prev_cp_bytes) {
      // Replace code point with shorter one
      for (i = 0; i < buf_len; i++) {
        this_cp[i] = buf[i];
      }
      // Move string down to eliminate unneeded chars
      memmove(this_cp + buf_len, this_cp + prev_cp_bytes, len - cp_idx);
      // Null terminate the shorter string.
      // Ensure string_len is not reduced because original 
      // value still matters for GC purposes
      raw[len - (prev_cp_bytes - buf_len)] = '\0'; 
    }
    // - 3) TODO: buf_len > prev_cp_bytes, will need to allocate more memory (!!)
    else {
      // TODO: maybe we can try a little harder here, at least in some cases
      Cyc_rt_raise2(data, "string-set! - Unable to allocate memory to store multibyte character", chr);
    }
  }
  return str;
}

object Cyc_string_ref(void *data, object str, object k)
{
  const char *raw;
  int idx, len;

  Cyc_check_str(data, str);
  Cyc_check_fixnum(data, k);

  raw = string_str(str);
  idx = unbox_number(k);
  len = string_num_cp(str);

  if (idx < 0 || idx >= len) {
    Cyc_rt_raise2(data, "string-ref - invalid index", k);
  }

  // Take fast path if all chars are just 1 byte
  if (string_num_cp(str) == string_len(str)) {
    return obj_char2obj(raw[idx]);
  } else {
    char_type codepoint = 0;
    uint32_t state = 0;
    int count;

    for (count = 0; *raw; ++raw){
      if (!Cyc_utf8_decode(&state, &codepoint, (uint8_t)*raw)){
        if (count == idx) break; // Reached requested index
        count += 1;
      }
    }
    if (state != CYC_UTF8_ACCEPT)
       Cyc_rt_raise2(data, "string-ref - invalid character at index", k);
    return obj_char2obj(codepoint);
  }
}

object Cyc_substring(void *data, object cont, object str, object start,
                     object end)
{
  const char *raw;
  int s, e, len;

  Cyc_check_str(data, str);
  Cyc_check_fixnum(data, start);
  Cyc_check_fixnum(data, end);

  raw = string_str(str);
  s = unbox_number(start);
  e = unbox_number(end);
  len = string_num_cp(str);

  if (s > e) {
    Cyc_rt_raise2(data, "substring - start cannot be greater than end", start);
  }
  if (s > len) {
    Cyc_rt_raise2(data,
                  "substring - start cannot be greater than string length",
                  start);
  }
  if (e > len) {
    e = len;
  }

  if (string_num_cp(str) == string_len(str)){ // Fast path for ASCII
    make_string_with_len(sub, raw + s, e - s);
    _return_closcall1(data, cont, &sub);
  } else {
    const char *tmp = raw;
    char_type codepoint;
    uint32_t state = 0;
    int num_ch, cur_ch_bytes = 0, start_i = 0, end_i = 0;
    for (num_ch = 0; *tmp; ++tmp){
      cur_ch_bytes++;
      if (!Cyc_utf8_decode(&state, &codepoint, (uint8_t)*tmp)){
        end_i += cur_ch_bytes;
        num_ch += 1;
        cur_ch_bytes = 0;

        if (num_ch == s) {
          start_i = end_i;
        }
        if (num_ch == e) {
          break;
        }
      }
    }
    if (state != CYC_UTF8_ACCEPT)
       Cyc_rt_raise2(data, "substring - invalid character in string", str);
    make_utf8_string_with_len(sub, raw + start_i, end_i - start_i, e - s);
    _return_closcall1(data, cont, &sub);
  }
}

/**
 * Return directory where cyclone is installed.
 * This is configured via the makefile during a build.
 */
object Cyc_installation_dir(void *data, object cont, object type)
{
  if (Cyc_is_symbol(type) == boolean_t &&
      strncmp(((symbol) type)->desc, "sld", 5) == 0) {
    char buf[1024];
    snprintf(buf, sizeof(buf), "%s", CYC_INSTALL_SLD);
    make_utf8_string(data, str, buf);
    _return_closcall1(data, cont, &str);
  } else if (Cyc_is_symbol(type) == boolean_t &&
             strncmp(((symbol) type)->desc, "lib", 5) == 0) {
    char buf[1024];
    snprintf(buf, sizeof(buf), "%s", CYC_INSTALL_LIB);
    make_utf8_string(data, str, buf);
    _return_closcall1(data, cont, &str);
  } else if (Cyc_is_symbol(type) == boolean_t &&
             strncmp(((symbol) type)->desc, "bin", 5) == 0) {
    char buf[1024];
    snprintf(buf, sizeof(buf), "%s", CYC_INSTALL_BIN);
    make_utf8_string(data, str, buf);
    _return_closcall1(data, cont, &str);
  } else if (Cyc_is_symbol(type) == boolean_t &&
             strncmp(((symbol) type)->desc, "inc", 5) == 0) {
    char buf[1024];
    snprintf(buf, sizeof(buf), "%s", CYC_INSTALL_INC);
    make_utf8_string(data, str, buf);
    _return_closcall1(data, cont, &str);
  } else {
    make_utf8_string(data, str, CYC_INSTALL_DIR);
    _return_closcall1(data, cont, &str);
  }
}

/**
 * Retrieve a value set during Cyclone compilation
 */
object Cyc_compilation_environment(void *data, object cont, object var)
{
  if (Cyc_is_symbol(var) == boolean_t){
    if (strncmp(((symbol) var)->desc, "cc-prog", 8) == 0) {
      char buf[1024];
      snprintf(buf, sizeof(buf), "%s", CYC_CC_PROG);
      make_utf8_string(data, str, buf);
      _return_closcall1(data, cont, &str);
    } else if (strncmp(((symbol) var)->desc, "cc-exec", 8) == 0) {
      char buf[1024];
      snprintf(buf, sizeof(buf), "%s", CYC_CC_EXEC);
      make_utf8_string(data, str, buf);
      _return_closcall1(data, cont, &str);
    } else if (strncmp(((symbol) var)->desc, "cc-lib", 7) == 0) {
      char buf[1024];
      snprintf(buf, sizeof(buf), "%s", CYC_CC_LIB);
      make_utf8_string(data, str, buf);
      _return_closcall1(data, cont, &str);
    } else if (strncmp(((symbol) var)->desc, "cc-so", 6) == 0) {
      char buf[1024];
      snprintf(buf, sizeof(buf), "%s", CYC_CC_SO);
      make_utf8_string(data, str, buf);
      _return_closcall1(data, cont, &str);
    } else if (strncmp(((symbol) var)->desc, "platform", 9) == 0) {
      char buf[1024];
      snprintf(buf, sizeof(buf), "%s", CYC_PLATFORM);
      make_utf8_string(data, str, buf);
      _return_closcall1(data, cont, &str);
    }
  }
  Cyc_rt_raise2(data, 
    "Cyc-compilation-environment - unrecognized symbol", 
    var);
  return NULL;
}

/**
 * Perform same role as the CHICKEN function:
 *
 * Contains the list of arguments passed to this program, with the name 
 * of the program and any runtime options (all options starting with -:) 
 * removed.
 *
 * For now, runtime options are not removed.
 */
object Cyc_command_line_arguments(void *data, object cont)
{
  int i;
  object lis = NULL;
  for (i = _cyc_argc; i > 1; i--) {     // skip program name
    object ps = alloca(sizeof(string_type));
    object pl = alloca(sizeof(pair_type));
    make_utf8_string(data, s, _cyc_argv[i - 1]);
    memcpy(ps, &s, sizeof(string_type));
    ((list) pl)->hdr.mark = gc_color_red;
    ((list) pl)->hdr.grayed = 0;
    ((list) pl)->hdr.immutable = 0;
    ((list) pl)->tag = pair_tag;
    ((list) pl)->pair_car = ps;
    ((list) pl)->pair_cdr = lis;
    lis = pl;
  }
  _return_closcall1(data, cont, lis);
}

object Cyc_make_vector(void *data, object cont, int argc, object len, ...)
{
  object v = NULL;
  object fill = boolean_f;
  int i, ulen;
  size_t element_vec_size;
  va_list ap;
  make_pair(tmp_pair, NULL, NULL);
  make_c_opaque(opq, NULL);
  va_start(ap, len);
  if (argc > 1) {
    fill = va_arg(ap, object);
  }
  va_end(ap);
  Cyc_check_num(data, len);
  ulen = unbox_number(len);
  element_vec_size = sizeof(object) * ulen;

  if (element_vec_size >= MAX_STACK_OBJ) {
    // If vector is too large to allocate on the stack, allocate on heap
    //
    // TODO: mark this thread as potentially blocking before doing
    //       the allocation????
    int heap_grown;
    v = gc_alloc(((gc_thread_data *)data)->heap, 
                 sizeof(vector_type) + element_vec_size,
                 boolean_f, // OK to populate manually over here
                 (gc_thread_data *)data, 
                 &heap_grown);
    ((vector) v)->hdr.mark = ((gc_thread_data *)data)->gc_alloc_color;
    ((vector) v)->hdr.grayed = 0;
    ((vector) v)->hdr.immutable = 0;
    ((vector) v)->tag = vector_tag;
    ((vector) v)->num_elements = ulen;
    ((vector) v)->elements = (object *)(((char *)v) + sizeof(vector_type));
    // Use write barrier to ensure fill is moved to heap if it is on the stack
    // Otherwise if next minor GC misses fill it could be catastrophic
    car(&tmp_pair) = fill;
    add_mutation(data, &tmp_pair, -1, fill);
    // Add a special object to indicate full vector must be scanned by GC
    opaque_ptr(&opq) = v;
    add_mutation(data, &opq, -1, v);
  } else {
    v = alloca(sizeof(vector_type));
    ((vector) v)->hdr.mark = gc_color_red;
    ((vector) v)->hdr.grayed = 0;
    ((vector) v)->hdr.immutable = 0;
    ((vector) v)->tag = vector_tag;
    ((vector) v)->num_elements = ulen;
    ((vector) v)->elements = NULL;

    if (ulen > 0) {
      ((vector) v)->elements = 
        (object *) alloca(sizeof(object) * ((vector) v)->num_elements);
    }
  }

  for (i = 0; i < ((vector) v)->num_elements; i++) {
    ((vector) v)->elements[i] = fill;
  }
  _return_closcall1(data, cont, v);
}

object Cyc_make_bytevector(void *data, object cont, int argc, object len, ...)
{
  object bv = NULL;
  object fill = obj_int2obj(0);
  int length, fill_val;
  va_list ap;
  va_start(ap, len);
  if (argc > 1) {
    fill = va_arg(ap, object);
  }
  va_end(ap);
  Cyc_check_num(data, len);
  length = unbox_number(len);

  if (length >= MAX_STACK_OBJ) {
    int heap_grown;
    bv = gc_alloc(((gc_thread_data *)data)->heap,
                  sizeof(bytevector_type) + length,
                  boolean_f, // OK to populate manually over here
                  (gc_thread_data *)data, 
                  &heap_grown);
    ((bytevector) bv)->hdr.mark = ((gc_thread_data *)data)->gc_alloc_color;
    ((bytevector) bv)->hdr.grayed = 0;
    ((bytevector) bv)->hdr.immutable = 0;
    ((bytevector) bv)->tag = bytevector_tag;
    ((bytevector) bv)->len = length;
    ((bytevector) bv)->data = (char *)(((char *)bv) + sizeof(bytevector_type));
  } else {
    bv = alloca(sizeof(bytevector_type));
    ((bytevector) bv)->hdr.mark = gc_color_red;
    ((bytevector) bv)->hdr.grayed = 0;
    ((bytevector) bv)->hdr.immutable = 0;
    ((bytevector) bv)->tag = bytevector_tag;
    ((bytevector) bv)->len = length;
    ((bytevector) bv)->data = alloca(sizeof(char) * length);
  }

  if (argc > 1) {
    Cyc_check_num(data, fill);
    fill_val = unbox_number(fill);
    memset(((bytevector) bv)->data, (unsigned char)fill_val, length);
  }
  _return_closcall1(data, cont, bv);
}

#define Cyc_bytevector_va_list(argc) { \
  int i = 0, val; \
  va_list ap; \
  object tmp; \
  char *buffer; \
  make_empty_bytevector(bv); \
  if (argc > 0) { \
    Cyc_check_num(data, bval); \
    buffer = alloca(sizeof(char) * argc); \
    val = unbox_number(bval); \
    buffer[i] = val; \
    va_start(ap, bval); \
    for(i = 1; i < argc; i++) { \
      tmp = va_arg(ap, object); \
      Cyc_check_num(data, tmp); \
      val = unbox_number(tmp); \
      buffer[i] = (unsigned char)val; \
    } \
    va_end(ap); \
    bv.len = argc; \
    bv.data = buffer; \
  } \
  _return_closcall1(data, cont, &bv); \
}

object dispatch_bytevector(void *data, int _argc, object clo, object cont,
                         object bval, ...)
{
  Cyc_bytevector_va_list((_argc - 1));
}

object Cyc_bytevector(void *data, object cont, int _argc, object bval, ...)
{
  Cyc_bytevector_va_list(_argc);
}

#define Cyc_bytevector_append_va_list(argc) { \
  int i = 0, buf_idx = 0, total_length = 0; \
  va_list ap; \
  object tmp; \
  char *buffer; \
  char **buffers = NULL; \
  int *lengths = NULL; \
  make_empty_bytevector(result); \
  if (argc > 0) { \
    buffers = alloca(sizeof(char *) * argc); \
    lengths = alloca(sizeof(int) * argc); \
    Cyc_check_bvec(data, bv); \
    total_length = ((bytevector)bv)->len; \
    lengths[0] = ((bytevector)bv)->len; \
    buffers[0] = ((bytevector)bv)->data; \
    va_start(ap, bv); \
    for(i = 1; i < argc; i++) { \
      tmp = va_arg(ap, object); \
      Cyc_check_bvec(data, tmp); \
      total_length += ((bytevector)tmp)->len; \
      lengths[i] = ((bytevector)tmp)->len; \
      buffers[i] = ((bytevector)tmp)->data; \
    } \
    va_end(ap); \
    buffer = alloca(sizeof(char) * total_length); \
    for (i = 0; i < argc; i++) { \
      memcpy(&buffer[buf_idx], buffers[i], lengths[i]); \
      buf_idx += lengths[i]; \
    } \
    result.len = total_length; \
    result.data = buffer; \
  } \
  _return_closcall1(data, cont, &result); \
}

object dispatch_bytevector_91append(void *data, int _argc, object clo,
                                  object cont, object bv, ...)
{
  Cyc_bytevector_append_va_list((_argc - 1));
}

object Cyc_bytevector_append(void *data, object cont, int _argc, object bv, ...)
{
  Cyc_bytevector_append_va_list(_argc);
}

object Cyc_bytevector_copy(void *data, object cont, object bv, object start,
                           object end)
{
  int s, e;
  int len;

  Cyc_check_bvec(data, bv);
  Cyc_check_num(data, start);
  Cyc_check_num(data, end);

  s = unbox_number(start);
  e = unbox_number(end);
  len = e - s;

  if (s < 0 || s >= ((bytevector) bv)->len) {
    Cyc_rt_raise2(data, "bytevector-copy - invalid start", start);
  }

  if (e < 0 || e < s || e > ((bytevector) bv)->len) {
    Cyc_rt_raise2(data, "bytevector-copy - invalid end", end);
  }

  if (len >= MAX_STACK_OBJ) {
    int heap_grown;
    object result = gc_alloc(((gc_thread_data *)data)->heap,
                  sizeof(bytevector_type) + len,
                  boolean_f, // OK to populate manually over here
                  (gc_thread_data *)data, 
                  &heap_grown);
    ((bytevector) result)->hdr.mark = ((gc_thread_data *)data)->gc_alloc_color;
    ((bytevector) result)->hdr.grayed = 0;
    ((bytevector) result)->hdr.immutable = 0;
    ((bytevector) result)->tag = bytevector_tag;
    ((bytevector) result)->len = len;
    ((bytevector) result)->data = (char *)(((char *)result) + sizeof(bytevector_type));
    memcpy(&(((bytevector) result)->data[0]), &(((bytevector) bv)->data)[s], len);
    _return_closcall1(data, cont, result);
  } else {
    make_empty_bytevector(result);
    result.len = len;
    result.data = alloca(sizeof(char) * len);
    memcpy(&result.data[0], &(((bytevector) bv)->data)[s], len);
    _return_closcall1(data, cont, &result);
  }
}

object Cyc_utf82string(void *data, object cont, object bv, object start,
                       object end)
{
  const char *buf;
  int s, e;
  int len;

  Cyc_check_bvec(data, bv);
  Cyc_check_num(data, start);
  Cyc_check_num(data, end);

  buf = ((bytevector) bv)->data;
  s = unbox_number(start);
  e = unbox_number(end);
  len = e - s;

  if (s < 0 || (s >= ((bytevector) bv)->len && len > 0)) {
    Cyc_rt_raise2(data, "utf8->string - invalid start", start);
  }

  if (e < 0 || e < s || e > ((bytevector) bv)->len) {
    Cyc_rt_raise2(data, "utf8->string - invalid end", end);
  }

  {
    object st;
    alloc_string(data, st, len, len);
    memcpy(((string_type *)st)->str, &buf[s], len);
    ((string_type *)st)->str[len] = '\0';
    ((string_type *)st)->num_cp = Cyc_utf8_count_code_points((uint8_t *)(((string_type *)st)->str));
    _return_closcall1(data, cont, st);
  }
}

object Cyc_string2utf8(void *data, object cont, object str, object start,
                       object end)
{
  int s, e;
  int len;

  Cyc_check_str(data, str);
  Cyc_check_fixnum(data, start);
  Cyc_check_fixnum(data, end);

  s = unbox_number(start);
  e = unbox_number(end);
  len = e - s;

  if (s < 0 || (s >= string_num_cp(str) && len > 0)) {
    Cyc_rt_raise2(data, "string->utf8 - invalid start", start);
  }

  if (e < 0 || e < s || e > string_num_cp(str)) {
    Cyc_rt_raise2(data, "string->utf8 - invalid end", end);
  }

  // Fast path
  if (string_num_cp(str) == string_len(str)) {
    if (len >= MAX_STACK_OBJ) {
      int heap_grown;
      object bv = gc_alloc(((gc_thread_data *)data)->heap,
                    sizeof(bytevector_type) + len,
                    boolean_f, // OK to populate manually over here
                    (gc_thread_data *)data, 
                    &heap_grown);
      ((bytevector) bv)->hdr.mark = ((gc_thread_data *)data)->gc_alloc_color;
      ((bytevector) bv)->hdr.grayed = 0;
      ((bytevector) bv)->hdr.immutable = 0;
      ((bytevector) bv)->tag = bytevector_tag;
      ((bytevector) bv)->len = len;
      ((bytevector) bv)->data = (char *)(((char *)bv) + sizeof(bytevector_type));
      memcpy(&(((bytevector) bv)->data[0]), &(string_str(str))[s], len);
      _return_closcall1(data, cont, bv);
    } else {
      make_empty_bytevector(result);
      result.len = len;
      result.data = alloca(sizeof(char) * len);
      memcpy(&result.data[0], &(string_str(str))[s], len);
      _return_closcall1(data, cont, &result);
    }
  } else {
    const char *tmp = string_str(str);
    char_type codepoint;
    uint32_t state = 0;
    int num_ch, cur_ch_bytes = 0, start_i = 0, end_i = 0;
    // Figure out start / end indices
    for (num_ch = 0; *tmp; ++tmp){
      cur_ch_bytes++;
      if (!Cyc_utf8_decode(&state, &codepoint, (uint8_t)*tmp)){
        end_i += cur_ch_bytes;
        num_ch += 1;
        cur_ch_bytes = 0;

        if (num_ch == s) {
          start_i = end_i;
        }
        if (num_ch == e) {
          break;
        }
      }
    }
    len = end_i - start_i;
    if (len >= MAX_STACK_OBJ) {
      int heap_grown;
      object bv = gc_alloc(((gc_thread_data *)data)->heap,
                    sizeof(bytevector_type) + len,
                    boolean_f, // OK to populate manually over here
                    (gc_thread_data *)data, 
                    &heap_grown);
      ((bytevector) bv)->hdr.mark = ((gc_thread_data *)data)->gc_alloc_color;
      ((bytevector) bv)->hdr.grayed = 0;
      ((bytevector) bv)->hdr.immutable = 0;
      ((bytevector) bv)->tag = bytevector_tag;
      ((bytevector) bv)->len = len;
      ((bytevector) bv)->data = (char *)(((char *)bv) + sizeof(bytevector_type));
      memcpy(&(((bytevector) bv)->data[0]), &(string_str(str))[start_i], len);
      _return_closcall1(data, cont, bv);
    } else {
      make_empty_bytevector(result);
      result.len = len;
      result.data = alloca(sizeof(char) * result.len);
      memcpy(&result.data[0], &(string_str(str))[start_i], result.len);
      _return_closcall1(data, cont, &result);
    }
  }
}

object Cyc_bytevector_u8_ref(void *data, object bv, object k)
{
  const char *buf;
  int idx;
  int val;

  Cyc_check_bvec(data, bv);
  Cyc_check_fixnum(data, k);

  buf = ((bytevector) bv)->data;
  idx = unbox_number(k);

  if (idx < 0 || idx >= ((bytevector) bv)->len) {
    Cyc_rt_raise2(data, "bytevector-u8-ref - invalid index", k);
  }

  val = (unsigned char)(buf[idx]);
  return obj_int2obj(val);
}

object Cyc_bytevector_u8_set(void *data, object bv, object k, object b)
{
  char *buf;
  int idx, len, val;

  Cyc_check_bvec(data, bv);
  Cyc_check_fixnum(data, k);
  Cyc_check_fixnum(data, b);
  Cyc_verify_mutable(data, bv);

  buf = ((bytevector) bv)->data;
  idx = unbox_number(k);
  val = unbox_number(b);
  len = ((bytevector) bv)->len;

  Cyc_check_bounds(data, "bytevector-u8-set!", len, idx);
  buf[idx] = (unsigned char)val;
  return bv;
}

object Cyc_bytevector_length(void *data, object bv)
{
  if ((bv != NULL) && !is_value_type(bv) && ((list) bv)->tag == bytevector_tag) {
    return obj_int2obj(((bytevector) bv)->len);
  }
  Cyc_rt_raise_msg(data,
                   "bytevector-length - invalid parameter, expected bytevector\n");
  return NULL;
}

object Cyc_list2vector(void *data, object cont, object l)
{
  object v = NULL;
  object len_obj;
  object lst = l;
  int len, i = 0;
  size_t element_vec_size;

  make_c_opaque(opq, NULL);
  Cyc_check_pair_or_null(data, l);
  len_obj = Cyc_length(data, l);
  len = obj_obj2int(len_obj);
  element_vec_size = sizeof(object) * len;
  if (element_vec_size >= MAX_STACK_OBJ) {
    int heap_grown;
    v = gc_alloc(((gc_thread_data *)data)->heap, 
                 sizeof(vector_type) + element_vec_size,
                 boolean_f, // OK to populate manually over here
                 (gc_thread_data *)data, 
                 &heap_grown);
    ((vector) v)->hdr.mark = ((gc_thread_data *)data)->gc_alloc_color;
    ((vector) v)->hdr.grayed = 0;
    ((vector) v)->hdr.immutable = 0;
    ((vector) v)->tag = vector_tag;
    ((vector) v)->num_elements = len;
    ((vector) v)->elements = (object *)(((char *)v) + sizeof(vector_type));
    // TODO: do we need to worry about stack object in the list????
    //// Use write barrier to ensure fill is moved to heap if it is on the stack
    //// Otherwise if next minor GC misses fill it could be catastrophic
    //car(&tmp_pair) = fill;
    //add_mutation(data, &tmp_pair, -1, fill);
    // Add a special object to indicate full vector must be scanned by GC
    opaque_ptr(&opq) = v;
    add_mutation(data, &opq, -1, v);
  } else {
    v = alloca(sizeof(vector_type));
    ((vector) v)->hdr.mark = gc_color_red;
    ((vector) v)->hdr.grayed = 0;
    ((vector) v)->hdr.immutable = 0;
    ((vector) v)->tag = vector_tag;
    ((vector) v)->num_elements = len;
    ((vector) v)->elements =
        (((vector) v)->num_elements > 0) ?
        (object *) alloca(element_vec_size) : NULL;
  }
  while ((lst != NULL)) {
    ((vector) v)->elements[i++] = car(lst);
    lst = cdr(lst);
  }
  _return_closcall1(data, cont, v);
}

object Cyc_system(object cmd)
{
  if ((cmd == NULL) || is_value_type(cmd) || type_of(cmd) != string_tag) {
    return obj_int2obj(-1);
  }
  return obj_int2obj(system(((string_type *) cmd)->str));
}

#define declare_char_comp(FUNC, OP) \
object FUNC(void *data, object a, object b) \
{ \
  if (obj_obj2char(a) OP obj_obj2char(b)) \
    return boolean_t; \
  return boolean_f; \
}
declare_char_comp(Cyc_char_eq_op,  ==);
declare_char_comp(Cyc_char_gt_op,  > );
declare_char_comp(Cyc_char_lt_op,  < );
declare_char_comp(Cyc_char_gte_op, >=);
declare_char_comp(Cyc_char_lte_op, <=);

object Cyc_char2integer(object chr)
{
  return obj_int2obj(obj_obj2char(chr));
}

object Cyc_integer2char(void *data, object n)
{
  char_type val = 0;

  Cyc_check_num(data, n);
  val = unbox_number(n);
  return obj_char2obj(val);
}

void Cyc_halt(object obj)
{
#if DEBUG_SHOW_DIAG
  gc_print_stats(Cyc_heap);
#endif
  if (obj_is_int(obj)) {
    exit(obj_obj2int(obj));
  }

  if (obj == boolean_f) {
    exit(1);
  }

  exit(0);
}

object __halt(object obj)
{
  Cyc_halt(obj);
  return NULL;
}

// Signed arithmetic overflow checks, based on code from CHICKEN:

static int Cyc_checked_add(int x, int y, int *result)
{
  *result = x + y;
  return ((((*result ^ x) & (*result ^ y)) >> 30) != 0);
}

static int Cyc_checked_sub(int x, int y, int *result)
{
  *result = x - y;
  return ((((*result ^ x) & ~(*result ^ y)) >> 30) != 0);
}

static int Cyc_checked_mul(int x, int y, int *result)
{
  // Avoid undefined behavior by detecting overflow prior to multiplication
  // Based on code from Hacker's Delight and CHICKEN scheme
  uint xu, yu, c;
  c = (1UL<<30UL) - 1;
  xu = x < 0 ? -x : x;
  yu = y < 0 ? -y : y;

  if (yu != 0 && xu > (c / yu)) return 1; // Overflow

  *result = x * y;

  return (*result > CYC_FIXNUM_MAX) ||
         (*result < CYC_FIXNUM_MIN);
}

#define declare_num_op(FUNC, FUNC_OP, FUNC_APPLY, OP, INT_OP, BN_OP, NO_ARG, ONE_ARG, DIV) \
object FUNC_OP(void *data, common_type *x, object y) { \
    mp_int bn_tmp, bn_tmp2; \
    int tx, ty; \
    tx = type_of(x); \
    if (obj_is_int(y)) { \
      ty = -1; \
    } else if (is_object_type(y)) { \
      ty = type_of(y); \
    } else { \
      goto bad_arg_type_error; \
    } \
    if (DIV &&  \
        ((ty == -1 && (obj_obj2int(y) == 0)) || \
         (ty == integer_tag && integer_value(y) == 0) || \
         (ty == double_tag && double_value(y) == 0.0))) { \
      Cyc_rt_raise_msg(data, "Divide by zero"); \
    } \
    if (tx == integer_tag && ty == -1) { \
        int result; \
        if (INT_OP(x->integer_t.value, obj_obj2int(y), &result) == 0) { \
          x->integer_t.value = result; \
        } else { \
          BIGNUM_CALL(mp_init(&bn_tmp)); \
          BIGNUM_CALL(mp_init(&bn_tmp2)); \
          Cyc_int2bignum(x->integer_t.value, &bn_tmp); \
          Cyc_int2bignum(obj_obj2int(y), &bn_tmp2); \
          x->bignum_t.hdr.mark = gc_color_red; \
          x->bignum_t.hdr.grayed = 0; \
          x->bignum_t.tag = bignum_tag; \
          BIGNUM_CALL(mp_init(&(x->bignum_t.bn))); \
          BIGNUM_CALL(BN_OP(&bn_tmp, &bn_tmp2, &(x->bignum_t.bn))); \
          mp_clear(&bn_tmp); \
          mp_clear(&bn_tmp2); \
        } \
    } else if (tx == double_tag && ty == -1) { \
        x->double_t.value = x->double_t.value OP (obj_obj2int(y)); \
    } else if (tx == integer_tag && ty == integer_tag) { \
        x->integer_t.value = (x->integer_t.value) OP ((integer_type *)y)->value; \
    } else if (tx == double_tag && ty == integer_tag) { \
        x->double_t.value = x->double_t.value OP ((integer_type *)y)->value; \
    } else if (tx == integer_tag && ty == double_tag) { \
        x->double_t.hdr.mark = gc_color_red; \
        x->double_t.hdr.grayed = 0; \
        x->double_t.tag = double_tag; \
        x->double_t.value = x->integer_t.value OP ((double_type *)y)->value; \
    } else if (tx == double_tag && ty == double_tag) { \
        x->double_t.value = x->double_t.value OP ((double_type *)y)->value; \
    } else if (tx == integer_tag && ty == bignum_tag) { \
        BIGNUM_CALL(mp_init(&bn_tmp2)); \
        Cyc_int2bignum(x->integer_t.value, &bn_tmp2); \
        x->bignum_t.hdr.mark = gc_color_red; \
        x->bignum_t.hdr.grayed = 0; \
        x->bignum_t.tag = bignum_tag; \
        BIGNUM_CALL(mp_init(&(x->bignum_t.bn))); \
        BIGNUM_CALL(BN_OP(&bn_tmp2, &bignum_value(y), &(x->bignum_t.bn))); \
        mp_clear(&bn_tmp2); \
    } else if (tx == double_tag && ty == bignum_tag) { \
        x->double_t.value = x->double_t.value OP mp_get_double(&bignum_value(y)); \
    } else if (tx == bignum_tag && ty == -1) { \
        BIGNUM_CALL(mp_init(&bn_tmp2)); \
        Cyc_int2bignum(obj_obj2int(y), &bn_tmp2); \
        BIGNUM_CALL(BN_OP(&(x->bignum_t.bn), &bn_tmp2, &(x->bignum_t.bn))); \
        mp_clear(&bn_tmp2); \
    } else if (tx == bignum_tag && ty == double_tag) { \
        double d = mp_get_double(&(x->bignum_t.bn)); \
        mp_clear(&(x->bignum_t.bn)); \
        x->double_t.hdr.mark = gc_color_red; \
        x->double_t.hdr.grayed = 0; \
        x->double_t.tag = double_tag; \
        x->double_t.value = d OP ((double_type *)y)->value; \
    } else if (tx == bignum_tag && ty == bignum_tag) { \
        BIGNUM_CALL(BN_OP(&(x->bignum_t.bn), &bignum_value(y), &(x->bignum_t.bn))); \
    } else if (tx == complex_num_tag && ty == complex_num_tag) { \
        x->complex_num_t.value = x->complex_num_t.value OP ((complex_num_type *)y)->value; \
    } else if (tx == complex_num_tag && ty == -1) { \
        x->complex_num_t.value = x->complex_num_t.value OP (obj_obj2int(y)); \
    } else if (tx == complex_num_tag && ty == integer_tag) { \
        x->complex_num_t.value = x->complex_num_t.value OP ((integer_type *)y)->value; \
    } else if (tx == complex_num_tag && ty == bignum_tag) { \
        x->complex_num_t.value = x->complex_num_t.value OP mp_get_double(&bignum_value(y)); \
    } else if (tx == complex_num_tag && ty == double_tag) { \
        x->complex_num_t.value = x->complex_num_t.value OP complex_num_value(y); \
    } else if (tx == integer_tag && ty == complex_num_tag) { \
        x->complex_num_t.hdr.mark = gc_color_red; \
        x->complex_num_t.hdr.grayed = 0; \
        x->complex_num_t.tag = complex_num_tag; \
        x->complex_num_t.value = x->integer_t.value OP ((complex_num_type *)y)->value; \
    } else if (tx == bignum_tag && ty == complex_num_tag) { \
        double d = mp_get_double(&(x->bignum_t.bn)); \
        mp_clear(&(x->bignum_t.bn)); \
        x->complex_num_t.hdr.mark = gc_color_red; \
        x->complex_num_t.hdr.grayed = 0; \
        x->complex_num_t.tag = complex_num_tag; \
        x->complex_num_t.value = d OP ((complex_num_type *)y)->value; \
    } else if (tx == double_tag && ty == complex_num_tag) { \
        x->complex_num_t.hdr.mark = gc_color_red; \
        x->complex_num_t.hdr.grayed = 0; \
        x->complex_num_t.tag = complex_num_tag; \
        x->complex_num_t.value = x->double_t.value OP complex_num_value(y); \
    } else { \
        goto bad_arg_type_error; \
    } \
    return x; \
bad_arg_type_error: \
    { \
        make_string(s, "Bad argument type"); \
        make_pair(c1, y, NULL); \
        make_pair(c0, &s, &c1); \
        Cyc_rt_raise(data, &c0); \
        return NULL; \
    } \
} \
object FUNC(void *data, object cont, int argc, object n, ...) { \
    common_type buffer; \
    object result; \
    va_list ap; \
    va_start(ap, n); \
    result = Cyc_num_op_va_list(data, argc, FUNC_OP, NO_ARG, ONE_ARG, n, ap, &buffer); \
    va_end(ap); \
    _return_closcall1(data, cont, result); \
} \
void FUNC_APPLY(void *data, int argc, object clo, object cont, object n, ...) { \
    common_type buffer; \
    object result; \
    va_list ap; \
    va_start(ap, n); \
    result = Cyc_num_op_va_list(data, argc - 1, FUNC_OP, NO_ARG, ONE_ARG, n, ap, &buffer); \
    va_end(ap); \
    return_closcall1(data, cont, result); \
}

object Cyc_fast_sum(void *data, object ptr, object x, object y) {
  // x is int (assume value types for integers)
  if (obj_is_int(x)){
    if (obj_is_int(y)){
      int xx = obj_obj2int(x),
          yy = obj_obj2int(y),
          z;

      if (Cyc_checked_add(xx, yy, &z) == 0) {
        return obj_int2obj(z);
      } else {
        mp_int bnx, bny;
        BIGNUM_CALL(mp_init(&bnx));
        BIGNUM_CALL(mp_init(&bny));
        Cyc_int2bignum(xx, &bnx);
        Cyc_int2bignum(yy, &bny);
        alloc_bignum(data, bn);
        BIGNUM_CALL(mp_add(&bnx, &bny, &bignum_value(bn)));
        mp_clear(&bnx);
        mp_clear(&bny);
        return bn;
      }
    } else if (is_object_type(y) && type_of(y) == double_tag) {
      assign_double(ptr, (double)(obj_obj2int(x)) + double_value(y));
      return ptr;
    } else if (is_object_type(y) && type_of(y) == bignum_tag) {
        mp_int bnx;
        BIGNUM_CALL(mp_init(&bnx));
        Cyc_int2bignum(obj_obj2int(x), &bnx);
        alloc_bignum(data, bn);
        BIGNUM_CALL(mp_add(&bnx, &bignum_value(y), &bignum_value(bn)));
        mp_clear(&bnx);
        return bn;
    } else if (is_object_type(y) && type_of(y) == complex_num_tag) {
      assign_complex_num(ptr, ((obj_obj2int(x)) + complex_num_value(y)));
      return ptr;
    }
  }
  // x is double
  if (is_object_type(x) && type_of(x) == double_tag) {
    if (obj_is_int(y)){
      assign_double(ptr, double_value(x) + (double)(obj_obj2int(y)));
      return ptr;
    } else if (is_object_type(y) && type_of(y) == double_tag) {
      assign_double(ptr, double_value(x) + double_value(y));
      return ptr;
    } else if (is_object_type(y) && type_of(y) == bignum_tag) {
      assign_double(ptr, double_value(x) + mp_get_double(&bignum_value(y)));
      return ptr;
    } else if (is_object_type(y) && type_of(y) == complex_num_tag) {
      assign_complex_num(ptr, double_value(x) + complex_num_value(y));
      return ptr;
    }
  }
  // x is bignum
  if (is_object_type(x) && type_of(x) == bignum_tag) {
    if (obj_is_int(y)){
      mp_int bny;
      BIGNUM_CALL(mp_init(&bny));
      Cyc_int2bignum(obj_obj2int(y), &bny);
      alloc_bignum(data, bn);
      BIGNUM_CALL(mp_add(&bignum_value(x), &bny, &bignum_value(bn)));
      mp_clear(&bny);
      return bn;
    } else if (is_object_type(y) && type_of(y) == double_tag) {
      assign_double(ptr, mp_get_double(&bignum_value(x)) + double_value(y));
      return ptr;
    } else if (is_object_type(y) && type_of(y) == bignum_tag) {
      alloc_bignum(data, bn);
      BIGNUM_CALL(mp_add(&bignum_value(x), &bignum_value(y), &bignum_value(bn)));
      return bn;
    } else if (is_object_type(y) && type_of(y) == complex_num_tag) {
      assign_complex_num(ptr, mp_get_double(&bignum_value(x)) + complex_num_value(y));
      return ptr;
    }
  }
  // x is complex
  if (is_object_type(x) && type_of(x) == complex_num_tag) {
    if (obj_is_int(y)){
      assign_complex_num(ptr, complex_num_value(x) + (double)(obj_obj2int(y)));
      return ptr;
    } else if (is_object_type(y) && type_of(y) == double_tag) {
      assign_complex_num(ptr, complex_num_value(x) + double_value(y));
      return ptr;
    } else if (is_object_type(y) && type_of(y) == complex_num_tag) {
      assign_complex_num(ptr, complex_num_value(x) + complex_num_value(y));
      return ptr;
    } else if (is_object_type(y) && type_of(y) == bignum_tag) {
      assign_complex_num(ptr, complex_num_value(x) + mp_get_double(&bignum_value(y)));
      return ptr;
    }
  }
  // still here, raise an error 
  make_string(s, "Bad argument type");
  make_pair(c2, y, NULL);
  make_pair(c1, x, &c2);
  make_pair(c0, &s, &c1);
  Cyc_rt_raise(data, &c0);
  return NULL;
}

object Cyc_fast_sub(void *data, object ptr, object x, object y) {
  // x is int (assume value types for integers)
  if (obj_is_int(x)){
    if (obj_is_int(y)){
      int xx = obj_obj2int(x),
          yy = obj_obj2int(y),
          z;
      if (Cyc_checked_sub(xx, yy, &z) == 0) {
        return obj_int2obj(z);
      } else {
        mp_int bnx, bny;
        BIGNUM_CALL(mp_init(&bnx));
        BIGNUM_CALL(mp_init(&bny));
        Cyc_int2bignum(xx, &bnx);
        Cyc_int2bignum(yy, &bny);
        alloc_bignum(data, bn);
        BIGNUM_CALL(mp_sub(&bnx, &bny, &bignum_value(bn)));
        mp_clear(&bnx);
        mp_clear(&bny);
        return bn;
      }
    } else if (is_object_type(y) && type_of(y) == double_tag) {
      assign_double(ptr, (double)(obj_obj2int(x)) - double_value(y));
      return ptr;
    } else if (is_object_type(y) && type_of(y) == bignum_tag) {
        mp_int bnx;
        BIGNUM_CALL(mp_init(&bnx));
        Cyc_int2bignum(obj_obj2int(x), &bnx);
        alloc_bignum(data, bn);
        BIGNUM_CALL(mp_sub(&bnx, &bignum_value(y), &bignum_value(bn)));
        mp_clear(&bnx);
        return bn;
    } else if (is_object_type(y) && type_of(y) == complex_num_tag) {
      assign_complex_num(ptr, ((obj_obj2int(x)) - complex_num_value(y)));
      return ptr;
    }
  }
  // x is double
  if (is_object_type(x) && type_of(x) == double_tag) {
    if (obj_is_int(y)){
      assign_double(ptr, double_value(x) - (double)(obj_obj2int(y)));
      return ptr;
    } else if (is_object_type(y) && type_of(y) == double_tag) {
      assign_double(ptr, double_value(x) - double_value(y));
      return ptr;
    } else if (is_object_type(y) && type_of(y) == bignum_tag) {
      assign_double(ptr, double_value(x) - mp_get_double(&bignum_value(y)));
      return ptr;
    } else if (is_object_type(y) && type_of(y) == complex_num_tag) {
      assign_complex_num(ptr, double_value(x) - complex_num_value(y));
      return ptr;
    }
  }
  // x is bignum
  if (is_object_type(x) && type_of(x) == bignum_tag) {
    if (obj_is_int(y)){
      mp_int bny;
      BIGNUM_CALL(mp_init(&bny));
      Cyc_int2bignum(obj_obj2int(y), &bny);
      alloc_bignum(data, bn);
      BIGNUM_CALL(mp_sub(&bignum_value(x), &bny, &bignum_value(bn)));
      mp_clear(&bny);
      return bn;
    } else if (is_object_type(y) && type_of(y) == double_tag) {
      assign_double(ptr, mp_get_double(&bignum_value(x)) - double_value(y));
      return ptr;
    } else if (is_object_type(y) && type_of(y) == bignum_tag) {
      alloc_bignum(data, bn);
      BIGNUM_CALL(mp_sub(&bignum_value(x), &bignum_value(y), &bignum_value(bn)));
      return bn;
    } else if (is_object_type(y) && type_of(y) == complex_num_tag) {
      assign_complex_num(ptr, mp_get_double(&bignum_value(x)) - complex_num_value(y));
      return ptr;
    }
  }
  // x is complex
  if (is_object_type(x) && type_of(x) == complex_num_tag) {
    if (obj_is_int(y)){
      assign_complex_num(ptr, complex_num_value(x) - (double)(obj_obj2int(y)));
      return ptr;
    } else if (is_object_type(y) && type_of(y) == double_tag) {
      assign_complex_num(ptr, complex_num_value(x) - double_value(y));
      return ptr;
    } else if (is_object_type(y) && type_of(y) == complex_num_tag) {
      assign_complex_num(ptr, complex_num_value(x) - complex_num_value(y));
      return ptr;
    } else if (is_object_type(y) && type_of(y) == bignum_tag) {
      assign_complex_num(ptr, complex_num_value(x) - mp_get_double(&bignum_value(y)));
      return ptr;
    }
  }
  // still here, raise an error 
  make_string(s, "Bad argument type");
  make_pair(c2, y, NULL);
  make_pair(c1, x, &c2);
  make_pair(c0, &s, &c1);
  Cyc_rt_raise(data, &c0);
  return NULL;
}

object Cyc_fast_mul(void *data, object ptr, object x, object y) {
  // x is int (assume value types for integers)
  if (obj_is_int(x)){
    if (obj_is_int(y)){
      int xx = obj_obj2int(x),
          yy = obj_obj2int(y),
          z;
      if (Cyc_checked_mul(xx, yy, &z) == 0) {
        return obj_int2obj(z);
      } else {
        mp_int bnx, bny;
        BIGNUM_CALL(mp_init(&bnx));
        BIGNUM_CALL(mp_init(&bny));
        Cyc_int2bignum(xx, &bnx);
        Cyc_int2bignum(yy, &bny);
        alloc_bignum(data, bn);
        BIGNUM_CALL(mp_mul(&bnx, &bny, &bignum_value(bn)));
        mp_clear(&bnx);
        mp_clear(&bny);
        return bn;
      }
    } else if (is_object_type(y) && type_of(y) == double_tag) {
      assign_double(ptr, (double)(obj_obj2int(x)) * double_value(y));
      return ptr;
    } else if (is_object_type(y) && type_of(y) == bignum_tag) {
        mp_int bnx;
        BIGNUM_CALL(mp_init(&bnx));
        Cyc_int2bignum(obj_obj2int(x), &bnx);
        alloc_bignum(data, bn);
        BIGNUM_CALL(mp_mul(&bnx, &bignum_value(y), &bignum_value(bn)));
        mp_clear(&bnx);
        return bn;
    } else if (is_object_type(y) && type_of(y) == complex_num_tag) {
      assign_complex_num(ptr, ((obj_obj2int(x)) * complex_num_value(y)));
      return ptr;
    }
  }
  // x is double
  if (is_object_type(x) && type_of(x) == double_tag) {
    if (obj_is_int(y)){
      assign_double(ptr, double_value(x) * (double)(obj_obj2int(y)));
      return ptr;
    } else if (is_object_type(y) && type_of(y) == double_tag) {
      assign_double(ptr, double_value(x) * double_value(y));
      return ptr;
    } else if (is_object_type(y) && type_of(y) == bignum_tag) {
      assign_double(ptr, double_value(x) * mp_get_double(&bignum_value(y)));
      return ptr;
    } else if (is_object_type(y) && type_of(y) == complex_num_tag) {
      assign_complex_num(ptr, double_value(x) * complex_num_value(y));
      return ptr;
    }
  }
  // x is bignum
  if (is_object_type(x) && type_of(x) == bignum_tag) {
    if (obj_is_int(y)){
      mp_int bny;
      BIGNUM_CALL(mp_init(&bny));
      Cyc_int2bignum(obj_obj2int(y), &bny);
      alloc_bignum(data, bn);
      BIGNUM_CALL(mp_mul(&bignum_value(x), &bny, &bignum_value(bn)));
      mp_clear(&bny);
      return bn;
    } else if (is_object_type(y) && type_of(y) == double_tag) {
      assign_double(ptr, mp_get_double(&bignum_value(x)) * double_value(y));
      return ptr;
    } else if (is_object_type(y) && type_of(y) == bignum_tag) {
      alloc_bignum(data, bn);
      BIGNUM_CALL(mp_mul(&bignum_value(x), &bignum_value(y), &bignum_value(bn)));
      return bn;
    } else if (is_object_type(y) && type_of(y) == complex_num_tag) {
      assign_complex_num(ptr, mp_get_double(&bignum_value(x)) * complex_num_value(y));
      return ptr;
    }
  }
  // x is complex
  if (is_object_type(x) && type_of(x) == complex_num_tag) {
    if (obj_is_int(y)){
      assign_complex_num(ptr, complex_num_value(x) * (double)(obj_obj2int(y)));
      return ptr;
    } else if (is_object_type(y) && type_of(y) == double_tag) {
      assign_complex_num(ptr, complex_num_value(x) * double_value(y));
      return ptr;
    } else if (is_object_type(y) && type_of(y) == complex_num_tag) {
      assign_complex_num(ptr, complex_num_value(x) * complex_num_value(y));
      return ptr;
    } else if (is_object_type(y) && type_of(y) == bignum_tag) {
      assign_complex_num(ptr, complex_num_value(x) * mp_get_double(&bignum_value(y)));
      return ptr;
    }
  }
  // still here, raise an error 
  make_string(s, "Bad argument type");
  make_pair(c2, y, NULL);
  make_pair(c1, x, &c2);
  make_pair(c0, &s, &c1);
  Cyc_rt_raise(data, &c0);
  return NULL;
}

object Cyc_fast_div(void *data, object ptr, object x, object y) {
  // x is int (assume value types for integers)
  if (obj_is_int(x)){
    if (obj_is_int(y)){
      if (obj_obj2int(y) == 0) { goto divbyzero; }
      // Overflow can occur if y = 0 || (x = 0x80000000 && y = -1)
      // We already check for 0 above and the value of x above is a
      // bignum, so no futher checks are required.
      assign_double(ptr, (double)(obj_obj2int(x)) / obj_obj2int(y));
      return ptr;
    } else if (is_object_type(y) && type_of(y) == double_tag) {
      assign_double(ptr, (double)(obj_obj2int(x)) / double_value(y));
      return ptr;
    } else if (is_object_type(y) && type_of(y) == bignum_tag) {
        mp_int bnx;
        BIGNUM_CALL(mp_init(&bnx));
        Cyc_int2bignum(obj_obj2int(x), &bnx);
        alloc_bignum(data, bn);
        BIGNUM_CALL(mp_div(&bnx, &bignum_value(y), &bignum_value(bn), NULL));
        mp_clear(&bnx);
        return bn;
    } else if (is_object_type(y) && type_of(y) == complex_num_tag) {
      assign_complex_num(ptr, ((obj_obj2int(x)) / complex_num_value(y)));
      return ptr;
    }
  }
  // x is double
  if (is_object_type(x) && type_of(x) == double_tag) {
    if (obj_is_int(y)){
      assign_double(ptr, double_value(x) / (double)(obj_obj2int(y)));
      return ptr;
    } else if (is_object_type(y) && type_of(y) == double_tag) {
      assign_double(ptr, double_value(x) / double_value(y));
      return ptr;
    } else if (is_object_type(y) && type_of(y) == bignum_tag) {
      assign_double(ptr, double_value(x) / mp_get_double(&bignum_value(y)));
      return ptr;
    } else if (is_object_type(y) && type_of(y) == complex_num_tag) {
      assign_complex_num(ptr, double_value(x) / complex_num_value(y));
      return ptr;
    }
  }
  // x is bignum
  if (is_object_type(x) && type_of(x) == bignum_tag) {
    if (obj_is_int(y)){
      mp_int bny;
      BIGNUM_CALL(mp_init(&bny));
      Cyc_int2bignum(obj_obj2int(y), &bny);
      alloc_bignum(data, bn);
      BIGNUM_CALL(mp_div(&bignum_value(x), &bny, &bignum_value(bn), NULL));
      mp_clear(&bny);
      return bn;
    } else if (is_object_type(y) && type_of(y) == double_tag) {
      assign_double(ptr, mp_get_double(&bignum_value(x)) / double_value(y));
      return ptr;
    } else if (is_object_type(y) && type_of(y) == bignum_tag) {
      alloc_bignum(data, bn);
      BIGNUM_CALL(mp_div(&bignum_value(x), &bignum_value(y), &bignum_value(bn), NULL));
      return bn;
    } else if (is_object_type(y) && type_of(y) == complex_num_tag) {
      assign_complex_num(ptr, mp_get_double(&bignum_value(x)) / complex_num_value(y));
      return ptr;
    }
  }
  // x is complex
  if (is_object_type(x) && type_of(x) == complex_num_tag) {
    if (obj_is_int(y)){
      assign_complex_num(ptr, complex_num_value(x) / (double)(obj_obj2int(y)));
      return ptr;
    } else if (is_object_type(y) && type_of(y) == double_tag) {
      assign_complex_num(ptr, complex_num_value(x) / double_value(y));
      return ptr;
    } else if (is_object_type(y) && type_of(y) == complex_num_tag) {
      assign_complex_num(ptr, complex_num_value(x) / complex_num_value(y));
      return ptr;
    } else if (is_object_type(y) && type_of(y) == bignum_tag) {
      assign_complex_num(ptr, complex_num_value(x) / mp_get_double(&bignum_value(y)));
      return ptr;
    }
  }
  // still here, raise an error 
  make_string(s, "Bad argument type");
  make_pair(c2, y, NULL);
  make_pair(c1, x, &c2);
  make_pair(c0, &s, &c1);
  Cyc_rt_raise(data, &c0);
divbyzero:
  Cyc_rt_raise_msg(data, "Divide by zero");
  return NULL;
}

object Cyc_div_op(void *data, common_type * x, object y)
{
  mp_int bn_tmp2;
  int tx = type_of(x), ty;
  if (obj_is_int(y)) {
    ty = -1;
  } else if (is_object_type(y)) {
    ty = type_of(y);
  } else {
    goto bad_arg_type_error;
  }
  if (tx == integer_tag && ty == -1) {
    if (obj_obj2int(y) == 0) {
      Cyc_rt_raise_msg(data, "Divide by zero");
    }
    x->double_t.tag = double_tag;
    x->double_t.value = ((double)x->integer_t.value) / (obj_obj2int(y));
  } else if (tx == double_tag && ty == -1) {
    x->double_t.value = x->double_t.value / (obj_obj2int(y));
  } else if (tx == integer_tag && ty == integer_tag) {
    x->double_t.tag = double_tag;
    x->double_t.value =
        ((double)x->integer_t.value) / ((integer_type *) y)->value;
  } else if (tx == double_tag && ty == integer_tag) {
    x->double_t.value = x->double_t.value / ((integer_type *) y)->value;
  } else if (tx == integer_tag && ty == double_tag) {
    x->double_t.hdr.mark = gc_color_red;
    x->double_t.hdr.grayed = 0;
    x->double_t.tag = double_tag;
    x->double_t.value = x->integer_t.value / ((double_type *) y)->value;
  } else if (tx == double_tag && ty == double_tag) {
    x->double_t.value = x->double_t.value / ((double_type *) y)->value;
  } else if (tx == integer_tag && ty == bignum_tag) {
    BIGNUM_CALL(mp_init(&bn_tmp2));
    Cyc_int2bignum(x->integer_t.value, &bn_tmp2);
    x->bignum_t.hdr.mark = gc_color_red;
    x->bignum_t.hdr.grayed = 0;
    x->bignum_t.tag = bignum_tag;
    BIGNUM_CALL(mp_init(&(x->bignum_t.bn)));
    BIGNUM_CALL(mp_div(&bn_tmp2, &bignum_value(y), &(x->bignum_t.bn), NULL));
    mp_clear(&bn_tmp2);
  } else if (tx == double_tag && ty == bignum_tag) {
     x->double_t.value = x->double_t.value / mp_get_double(&bignum_value(y));
  } else if (tx == bignum_tag && ty == -1) {
    BIGNUM_CALL(mp_init(&bn_tmp2));
    Cyc_int2bignum(obj_obj2int(y), &bn_tmp2);
    BIGNUM_CALL(mp_div(&(x->bignum_t.bn), &bn_tmp2, &(x->bignum_t.bn), NULL));
    mp_clear(&bn_tmp2);
  } else if (tx == bignum_tag && ty == double_tag) {
    double d = mp_get_double(&(x->bignum_t.bn));
    mp_clear(&(x->bignum_t.bn));
    x->double_t.hdr.mark = gc_color_red;
    x->double_t.hdr.grayed = 0;
    x->double_t.tag = double_tag;
    x->double_t.value = d / ((double_type *)y)->value;
  } else if (tx == bignum_tag && ty == bignum_tag) {
    BIGNUM_CALL(mp_div(&(x->bignum_t.bn), &bignum_value(y), &(x->bignum_t.bn), NULL));
  } else if (tx == complex_num_tag && ty == complex_num_tag) {
      x->complex_num_t.value = x->complex_num_t.value / ((complex_num_type *)y)->value;
  } else if (tx == complex_num_tag && ty == -1) {
      x->complex_num_t.value = x->complex_num_t.value / (obj_obj2int(y));
  } else if (tx == complex_num_tag && ty == integer_tag) {
      x->complex_num_t.value = x->complex_num_t.value / ((integer_type *)y)->value;
  } else if (tx == complex_num_tag && ty == bignum_tag) {
      x->complex_num_t.value = x->complex_num_t.value / mp_get_double(&bignum_value(y));
  } else if (tx == complex_num_tag && ty == double_tag) {
      x->complex_num_t.value = x->complex_num_t.value / complex_num_value(y);
  } else if (tx == integer_tag && ty == complex_num_tag) {
      x->complex_num_t.hdr.mark = gc_color_red;
      x->complex_num_t.hdr.grayed = 0;
      x->complex_num_t.tag = complex_num_tag;
      x->complex_num_t.value = x->integer_t.value / ((complex_num_type *)y)->value;
  } else if (tx == bignum_tag && ty == complex_num_tag) {
      double d = mp_get_double(&(x->bignum_t.bn));
      mp_clear(&(x->bignum_t.bn));
      x->complex_num_t.hdr.mark = gc_color_red;
      x->complex_num_t.hdr.grayed = 0;
      x->complex_num_t.tag = complex_num_tag;
      x->complex_num_t.value = d / ((complex_num_type *)y)->value;
  } else if (tx == double_tag && ty == complex_num_tag) {
      x->complex_num_t.hdr.mark = gc_color_red;
      x->complex_num_t.hdr.grayed = 0;
      x->complex_num_t.tag = complex_num_tag;
      x->complex_num_t.value = x->double_t.value / complex_num_value(y);
  } else {
    goto bad_arg_type_error;
  }
  return x;
bad_arg_type_error:
  {
    make_string(s, "Bad argument type");
    make_pair(c1, y, NULL);
    make_pair(c0, &s, &c1);
    Cyc_rt_raise(data, &c0);
    return NULL;
  }
}

object Cyc_div(void *data, object cont, int argc, object n, ...)
{
  common_type buffer;
  object result;
  va_list ap;
  va_start(ap, n);
  result = Cyc_num_op_va_list(data, argc, Cyc_div_op, -1, 1, n, ap, &buffer);
  va_end(ap);
  _return_closcall1(data, cont, result);
}

void dispatch_div(void *data, int argc, object clo, object cont, object n, ...)
{
  common_type buffer;
  object result;
  va_list ap;
  va_start(ap, n);
  result =
      Cyc_num_op_va_list(data, argc - 1, Cyc_div_op, -1, 1, n, ap, &buffer);
  va_end(ap);
  return_closcall1(data, cont, result);
}

declare_num_op(Cyc_sum, Cyc_sum_op, dispatch_sum, +, Cyc_checked_add, mp_add, 0, 0, 0);
declare_num_op(Cyc_sub, Cyc_sub_op, dispatch_sub, -, Cyc_checked_sub, mp_sub, -1, 0, 0);
declare_num_op(Cyc_mul, Cyc_mul_op, dispatch_mul, *, Cyc_checked_mul, mp_mul, 1, 1, 0);

object Cyc_num_op_va_list(void *data, int argc,
                          object(fn_op(void *, common_type *, object)),
                          int default_no_args, int default_one_arg, object n,
                          va_list ns, common_type * buf)
{
  int i;
  if (argc == 0) {
    if (default_no_args < 0) {
      Cyc_rt_raise_msg(data, "No arguments for numeric operation");
    }
    buf->integer_t.hdr.mark = gc_color_red;
    buf->integer_t.hdr.grayed = 0;
    buf->integer_t.tag = integer_tag;
    buf->integer_t.value = default_no_args;
    return buf;
  }

  if (obj_is_int(n)) {
    buf->integer_t.hdr.mark = gc_color_red;
    buf->integer_t.hdr.grayed = 0;
    buf->integer_t.tag = integer_tag;
    buf->integer_t.value = obj_obj2int(n);
  } else if (!is_object_type(n)) {
    goto bad_arg_type_error;
  } else if (type_of(n) == integer_tag) {
    buf->integer_t.hdr.mark = gc_color_red;
    buf->integer_t.hdr.grayed = 0;
    buf->integer_t.tag = integer_tag;
    buf->integer_t.value = ((integer_type *) n)->value;
  } else if (type_of(n) == double_tag) {
    buf->double_t.hdr.mark = gc_color_red;
    buf->double_t.hdr.grayed = 0;
    buf->double_t.tag = double_tag;
    buf->double_t.value = ((double_type *) n)->value;
  } else if (type_of(n) == bignum_tag) {
    buf->bignum_t.hdr.mark = gc_color_red;
    buf->bignum_t.hdr.grayed = 0;
    buf->bignum_t.tag = bignum_tag;
    BIGNUM_CALL(mp_init_copy(&(buf->bignum_t.bn), &bignum_value(n)));
  } else if (type_of(n) == complex_num_tag) {
    buf->complex_num_t.hdr.mark = gc_color_red;
    buf->complex_num_t.hdr.grayed = 0;
    buf->complex_num_t.tag = complex_num_tag;
    buf->complex_num_t.value = ((complex_num_type *) n)->value;
  } else {
    goto bad_arg_type_error;
  }

  if (argc == 1) {
    common_type tmp;
    tmp.integer_t.hdr.mark = gc_color_red;
    tmp.integer_t.hdr.grayed = 0;
    tmp.integer_t.tag = integer_tag;
    tmp.integer_t.value = default_one_arg;

    fn_op(data, &tmp, (object) buf);
    if (type_of(&tmp) == integer_tag) {
      buf->integer_t.tag = integer_tag;
      buf->integer_t.value = integer_value(&tmp);
    } else if (type_of(&tmp) == double_tag){
      buf->double_t.tag = double_tag;
      buf->double_t.value = double_value(&tmp);
    } else if (type_of(&tmp) == complex_num_tag){
      buf->complex_num_t.tag = complex_num_tag;
      buf->complex_num_t.value = complex_num_value(&tmp);
    } else {
      buf->bignum_t.tag = bignum_tag;
      buf->bignum_t.bn.used = tmp.bignum_t.bn.used;
      buf->bignum_t.bn.alloc = tmp.bignum_t.bn.alloc;
      buf->bignum_t.bn.sign = tmp.bignum_t.bn.sign;
      buf->bignum_t.bn.dp = tmp.bignum_t.bn.dp;
    }
  } else {
    for (i = 1; i < argc; i++) {
      fn_op(data, buf, va_arg(ns, object));
    }
  }

  // Convert to immediate int
  if (type_of(buf) == integer_tag) {
    return obj_int2obj(buf->integer_t.value);
  } else if (type_of(buf) == bignum_tag) {
    buf = gc_alloc_from_bignum(data, &(buf->bignum_t));
  }

  return buf;
bad_arg_type_error:
  {
    make_string(s, "Bad argument type");
    make_pair(c1, n, NULL);
    make_pair(c0, &s, &c1);
    Cyc_rt_raise(data, &c0);
    return NULL;
  }
}

void Cyc_expt_double(void *data, object cont, double x, double y)
{
  make_double(d, pow(x, y));
  return_closcall1(data, cont, &d);
}

void Cyc_expt(void *data, object cont, object x, object y)
{
  if (obj_is_int(x)){
    if (obj_is_int(y)){
      if (obj_obj2int(y) < 0) {
        Cyc_expt_double(data, cont, (double)obj_obj2int(x), (double)obj_obj2int(y));
      } else {
        alloc_bignum(data, bn);
        Cyc_int2bignum(obj_obj2int(x), &(bn->bn));
        BIGNUM_CALL(mp_expt_u32(&bignum_value(bn), obj_obj2int(y), &bignum_value(bn)));
        return_closcall1(data, cont, Cyc_bignum_normalize(data, bn));
      }
    } else if (is_object_type(y) && type_of(y) == double_tag) {
      Cyc_expt_double(data, cont, (double)obj_obj2int(x), double_value(y));
    } else if (is_object_type(y) && type_of(y) == bignum_tag) {
      // Not handled at this time
    }
  }
  if (is_object_type(x) && type_of(x) == double_tag) {
    make_double(d, 0.0);
    if (obj_is_int(y)){
      d.value = (double)obj_obj2int(y);
    } else if (is_object_type(y) && type_of(y) == double_tag) {
      d.value = double_value(y);
    } else if (is_object_type(y) && type_of(y) == bignum_tag) {
      d.value = mp_get_double(&bignum_value(y));
    }
    d.value = pow(double_value(x), d.value);
    return_closcall1(data, cont, &d);
  }
  if (is_object_type(x) && type_of(x) == bignum_tag) {
    if (obj_is_int(y)){
      if (obj_obj2int(y) < 0) {
        Cyc_expt_double(data, cont, mp_get_double(&bignum_value(x)), (double)obj_obj2int(y));
      } else {
        alloc_bignum(data, bn);
        BIGNUM_CALL(mp_expt_u32(&bignum_value(x), obj_obj2int(y), &bignum_value(bn)));
        return_closcall1(data, cont, Cyc_bignum_normalize(data, bn));
      }
    } else if (is_object_type(y) && type_of(y) == double_tag) {
      Cyc_expt_double(data, cont, mp_get_double(&bignum_value(x)), double_value(y));
      //make_double(d, 0.0);
      //d.value = pow(mp_get_double(&bignum_value(x)), double_value(y));
      //return_closcall1(data, cont, &d);
    } else if (is_object_type(y) && type_of(y) == bignum_tag) {
      // Not handled at this time
    }
  }
  // still here, raise an error 
  make_string(s, "Bad argument type");
  make_pair(c2, y, NULL);
  make_pair(c1, x, &c2);
  make_pair(c0, &s, &c1);
  Cyc_rt_raise(data, &c0);
}

void Cyc_bignum_remainder(void *data, object cont, object num1, object num2, object rem)
{
  BIGNUM_CALL(mp_div(&bignum_value(num1), &bignum_value(num2), NULL, &bignum_value(rem)));
  return_closcall1(data, cont, Cyc_bignum_normalize(data, rem));
}

void Cyc_remainder(void *data, object cont, object num1, object num2)
{
  int i = 0, j = 0;
  object result;
  if (obj_is_int(num1)) {
    if (obj_is_int(num2)){
      i = obj_obj2int(num1);
      j = obj_obj2int(num2);
    }
    else if (is_object_type(num2) && type_of(num2) == bignum_tag){
      alloc_bignum(data, bn);
      Cyc_int2bignum(obj_obj2int(num1), &(bn->bn));
      Cyc_bignum_remainder(data, cont, bn, num2, bn);
    }
    else if (is_object_type(num2) && type_of(num2) == double_tag){
      i = obj_obj2int(num1);
      j = ((double_type *)num2)->value; 
    }
    else {
      goto typeerror;
    }
  } else if (is_object_type(num1) && type_of(num1) == bignum_tag) {
    if (obj_is_int(num2)){
      alloc_bignum(data, bn);
      Cyc_int2bignum(obj_obj2int(num2), &(bn->bn));
      Cyc_bignum_remainder(data, cont, num1, bn, bn);
    }
    else if (is_object_type(num2) && type_of(num2) == bignum_tag){
      alloc_bignum(data, rem);
      Cyc_bignum_remainder(data, cont, num1, num2, rem);
    }
    else if (is_object_type(num2) && type_of(num2) == double_tag){
      j = ((double_type *)num2)->value; 
      alloc_bignum(data, bn);
      Cyc_int2bignum(obj_obj2int(j), &(bn->bn));
      Cyc_bignum_remainder(data, cont, num1, bn, bn);
    }
    else {
      goto typeerror;
    }
  } else if (is_object_type(num1) && type_of(num1) == double_tag){
    if (obj_is_int(num2)){
      i = ((double_type *)num1)->value; 
      j = obj_obj2int(num2);
    }
    else if (is_object_type(num2) && type_of(num2) == bignum_tag){
      i = ((double_type *)num1)->value; 
      alloc_bignum(data, bn);
      Cyc_int2bignum(obj_obj2int(i), &(bn->bn));
      Cyc_bignum_remainder(data, cont, bn, num2, bn);
    }
    else if (is_object_type(num2) && type_of(num2) == double_tag){
      i = ((double_type *)num1)->value; 
      j = ((double_type *)num2)->value; 
    } 
    else {
      goto typeerror;
    }
  } else {
    goto typeerror;
  }
  if (j == 0) { Cyc_rt_raise_msg(data, "Divide by zero"); }
  result = obj_int2obj(i % j);
  return_closcall1(data, cont, result); 
typeerror:
  {
    make_string(s, "Bad argument type");
    make_pair(c2, num2, NULL);
    make_pair(c1, num1, &c2);
    make_pair(c0, &s, &c1);
    Cyc_rt_raise(data, &c0);
  }
}

/* I/O functions */

port_type Cyc_stdout()
{
  make_port(_stdout, stdout, 0);
  return _stdout;
}

port_type Cyc_stdin()
{
  make_input_port(p, stdin, 1);
  return p;
}

port_type Cyc_stderr()
{
  make_port(p, stderr, 0);
  return p;
}

port_type _Cyc_io_open_input_file(void *data, object str, const char *mode)
{
  const char *fname;
  Cyc_check_str(data, str);
  fname = ((string_type *) str)->str;
  make_input_port(p, NULL, CYC_IO_BUF_LEN);
  p.fp = fopen(fname, mode);
  if (p.fp == NULL) {
    Cyc_rt_raise2(data, "Unable to open file", str);
  }
  return p;
}

port_type _Cyc_io_open_output_file(void *data, object str, const char *mode)
{
  const char *fname;
  Cyc_check_str(data, str);
  fname = ((string_type *) str)->str;
  make_port(p, NULL, 0);
  p.fp = fopen(fname, mode);
  if (p.fp == NULL) {
    Cyc_rt_raise2(data, "Unable to open file", str);
  }
  return p;
}

port_type Cyc_io_open_input_file(void *data, object str)
{
  return _Cyc_io_open_input_file(data, str, "r");
}

port_type Cyc_io_open_output_file(void *data, object str)
{
  return _Cyc_io_open_output_file(data, str, "w");
}

port_type Cyc_io_open_binary_input_file(void *data, object str)
{
  port_type p = _Cyc_io_open_input_file(data, str, "rb");
  p.flags |= CYC_BINARY_PORT_FLAG;
  return p;
}

port_type Cyc_io_open_binary_output_file(void *data, object str)
{
  port_type p = _Cyc_io_open_output_file(data, str, "wb");
  p.flags |= CYC_BINARY_PORT_FLAG;
  return p;
}

object Cyc_io_close_input_port(void *data, object port)
{
  return Cyc_io_close_port(data, port);
}

object Cyc_io_close_output_port(void *data, object port)
{
  return Cyc_io_close_port(data, port);
}

object Cyc_io_close_port(void *data, object port)
{
  Cyc_check_port(data, port);
  {
    FILE *stream = ((port_type *) port)->fp;
    if (stream)
      fclose(stream);
    ((port_type *) port)->fp = NULL;
   
    if (((port_type *)port)->mem_buf != NULL){
      free( ((port_type *)port)->mem_buf );
      ((port_type *)port)->mem_buf = NULL;
      ((port_type *)port)->mem_buf_len = 0;
    }
    if (((port_type *)port)->str_bv_in_mem_buf != NULL){
      free( ((port_type *)port)->str_bv_in_mem_buf );
      ((port_type *)port)->str_bv_in_mem_buf = NULL;
      ((port_type *)port)->str_bv_in_mem_buf_len = 0;
    }
    if (((port_type *)port)->tok_buf != NULL){
      free( ((port_type *)port)->tok_buf );
      ((port_type *)port)->tok_buf = NULL;
      ((port_type *)port)->tok_buf_len = 0;
    }
  }
  return port;
}

object Cyc_io_flush_output_port(void *data, object port)
{
  Cyc_check_port(data, port);
  {
    FILE *stream = ((port_type *) port)->fp;
    if (stream) {
      //int rv = 
      fflush(stream);
      // TODO: handle error if non-zero value returned
    }
  }
  return port;
}

object Cyc_io_delete_file(void *data, object filename)
{
  const char *fname;
  Cyc_check_str(data, filename);
  fname = ((string_type *) filename)->str;
  if (remove(fname) == 0)
    return boolean_t;           // Success
  return boolean_f;
}

object Cyc_io_file_exists(void *data, object filename)
{
  const char *fname;
  Cyc_check_str(data, filename);
  fname = ((string_type *) filename)->str;
  FILE *file;
  // Possibly overkill, but portable
  if ((file = fopen(fname, "r"))) {
    fclose(file);
    return boolean_t;
  }
  return boolean_f;
}

time_t Cyc_file_last_modified_time(char *path) {
    struct stat attr;
    stat(path, &attr);
    return(attr.st_mtime);
}


// Functions internal to the runtime that use malloc
list malloc_make_pair(object a, object d)
{
  pair_type *c = malloc(sizeof(pair_type));
  c->hdr.mark = gc_color_red;
  c->hdr.grayed = 0;
  c->hdr.immutable = 0;
  c->tag = pair_tag;
  c->pair_car = a;
  c->pair_cdr = d;
  return c;
}

cvar_type *mcvar(object * var)
{
  cvar_type *c = malloc(sizeof(cvar_type));
  c->hdr.mark = gc_color_red;
  c->hdr.grayed = 0;
  c->hdr.immutable = 0;
  c->tag = cvar_tag;
  c->pvar = var;
  return c;
}

void _Cyc_91global_91vars(void *data, object cont, object args)
{
  return_closcall1(data, cont, Cyc_global_variables);
}

void _car(void *data, object cont, object args)
{
  Cyc_check_num_args(data, "car", 1, args);
  {
    object var = car(args);
    Cyc_check_pair(data, var);
    return_closcall1(data, cont, car(var));
}}

void _cdr(void *data, object cont, object args)
{
  Cyc_check_num_args(data, "cdr", 1, args);
  Cyc_check_pair(data, car(args));
  return_closcall1(data, cont, cdr(car(args)));
}

void _caar(void *data, object cont, object args)
{
  Cyc_check_num_args(data, "caar", 1, args);
  Cyc_check_pair(data, car(args));
  return_closcall1(data, cont, Cyc_caar(data, car(args)));
}

void _cadr(void *data, object cont, object args)
{
  Cyc_check_num_args(data, "cadr", 1, args);
  Cyc_check_pair(data, car(args));
  return_closcall1(data, cont, Cyc_cadr(data, car(args)));
}

void _cdar(void *data, object cont, object args)
{
  Cyc_check_num_args(data, "cdar", 1, args);
  Cyc_check_pair(data, car(args));
  return_closcall1(data, cont, Cyc_cdar(data, car(args)));
}

void _cddr(void *data, object cont, object args)
{
  Cyc_check_num_args(data, "cddr", 1, args);
  Cyc_check_pair(data, car(args));
  return_closcall1(data, cont, Cyc_cddr(data, car(args)));
}

void _caaar(void *data, object cont, object args)
{
  Cyc_check_num_args(data, "caaar", 1, args);
  Cyc_check_pair(data, car(args));
  return_closcall1(data, cont, Cyc_caaar(data, car(args)));
}

void _caadr(void *data, object cont, object args)
{
  Cyc_check_num_args(data, "caadr", 1, args);
  Cyc_check_pair(data, car(args));
  return_closcall1(data, cont, Cyc_caadr(data, car(args)));
}

void _cadar(void *data, object cont, object args)
{
  Cyc_check_num_args(data, "cadar", 1, args);
  Cyc_check_pair(data, car(args));
  return_closcall1(data, cont, Cyc_cadar(data, car(args)));
}

void _caddr(void *data, object cont, object args)
{
  Cyc_check_num_args(data, "caddr", 1, args);
  Cyc_check_pair(data, car(args));
  return_closcall1(data, cont, Cyc_caddr(data, car(args)));
}

void _cdaar(void *data, object cont, object args)
{
  Cyc_check_num_args(data, "cdaar", 1, args);
  Cyc_check_pair(data, car(args));
  return_closcall1(data, cont, Cyc_cdaar(data, car(args)));
}

void _cdadr(void *data, object cont, object args)
{
  Cyc_check_num_args(data, "cdadr", 1, args);
  Cyc_check_pair(data, car(args));
  return_closcall1(data, cont, Cyc_cdadr(data, car(args)));
}

void _cddar(void *data, object cont, object args)
{
  Cyc_check_num_args(data, "cddar", 1, args);
  Cyc_check_pair(data, car(args));
  return_closcall1(data, cont, Cyc_cddar(data, car(args)));
}

void _cdddr(void *data, object cont, object args)
{
  Cyc_check_num_args(data, "cdddr", 1, args);
  Cyc_check_pair(data, car(args));
  return_closcall1(data, cont, Cyc_cdddr(data, car(args)));
}

void _caaaar(void *data, object cont, object args)
{
  Cyc_check_num_args(data, "caaaar", 1, args);
  Cyc_check_pair(data, car(args));
  return_closcall1(data, cont, Cyc_caaaar(data, car(args)));
}

void _caaadr(void *data, object cont, object args)
{
  Cyc_check_num_args(data, "caaadr", 1, args);
  Cyc_check_pair(data, car(args));
  return_closcall1(data, cont, Cyc_caaadr(data, car(args)));
}

void _caadar(void *data, object cont, object args)
{
  Cyc_check_num_args(data, "caadar", 1, args);
  Cyc_check_pair(data, car(args));
  return_closcall1(data, cont, Cyc_caadar(data, car(args)));
}

void _caaddr(void *data, object cont, object args)
{
  Cyc_check_num_args(data, "caaddr", 1, args);
  Cyc_check_pair(data, car(args));
  return_closcall1(data, cont, Cyc_caaddr(data, car(args)));
}

void _cadaar(void *data, object cont, object args)
{
  Cyc_check_num_args(data, "cadaar", 1, args);
  Cyc_check_pair(data, car(args));
  return_closcall1(data, cont, Cyc_cadaar(data, car(args)));
}

void _cadadr(void *data, object cont, object args)
{
  Cyc_check_num_args(data, "cadadr", 1, args);
  Cyc_check_pair(data, car(args));
  return_closcall1(data, cont, Cyc_cadadr(data, car(args)));
}

void _caddar(void *data, object cont, object args)
{
  Cyc_check_num_args(data, "caddar", 1, args);
  Cyc_check_pair(data, car(args));
  return_closcall1(data, cont, Cyc_caddar(data, car(args)));
}

void _cadddr(void *data, object cont, object args)
{
  Cyc_check_num_args(data, "cadddr", 1, args);
  Cyc_check_pair(data, car(args));
  return_closcall1(data, cont, Cyc_cadddr(data, car(args)));
}

void _cdaaar(void *data, object cont, object args)
{
  Cyc_check_num_args(data, "cdaaar", 1, args);
  Cyc_check_pair(data, car(args));
  return_closcall1(data, cont, Cyc_cdaaar(data, car(args)));
}

void _cdaadr(void *data, object cont, object args)
{
  Cyc_check_num_args(data, "cdaadr", 1, args);
  Cyc_check_pair(data, car(args));
  return_closcall1(data, cont, Cyc_cdaadr(data, car(args)));
}

void _cdadar(void *data, object cont, object args)
{
  Cyc_check_num_args(data, "cdadar", 1, args);
  Cyc_check_pair(data, car(args));
  return_closcall1(data, cont, Cyc_cdadar(data, car(args)));
}

void _cdaddr(void *data, object cont, object args)
{
  Cyc_check_num_args(data, "cdaddr", 1, args);
  Cyc_check_pair(data, car(args));
  return_closcall1(data, cont, Cyc_cdaddr(data, car(args)));
}

void _cddaar(void *data, object cont, object args)
{
  Cyc_check_num_args(data, "cddaar", 1, args);
  Cyc_check_pair(data, car(args));
  return_closcall1(data, cont, Cyc_cddaar(data, car(args)));
}

void _cddadr(void *data, object cont, object args)
{
  Cyc_check_num_args(data, "cddadr", 1, args);
  Cyc_check_pair(data, car(args));
  return_closcall1(data, cont, Cyc_cddadr(data, car(args)));
}

void _cdddar(void *data, object cont, object args)
{
  Cyc_check_num_args(data, "cdddar", 1, args);
  Cyc_check_pair(data, car(args));
  return_closcall1(data, cont, Cyc_cdddar(data, car(args)));
}

void _cddddr(void *data, object cont, object args)
{
  Cyc_check_num_args(data, "cddddr", 1, args);
  Cyc_check_pair(data, car(args));
  return_closcall1(data, cont, Cyc_cddddr(data, car(args)));
}

void _cons(void *data, object cont, object args)
{
  Cyc_check_num_args(data, "cons", 2, args);
  {
    make_pair(c, car(args), cadr(args));
    return_closcall1(data, cont, &c);
}}

void _eq_127(void *data, object cont, object args)
{
  Cyc_check_num_args(data, "eq?", 2, args);
  return_closcall1(data, cont, Cyc_eq(car(args), cadr(args)));
}

void _eqv_127(void *data, object cont, object args)
{
  Cyc_check_num_args(data, "eqv?", 2, args);
  return_closcall1(data, cont, Cyc_eqv(car(args), cadr(args)));
}

void _equal_127(void *data, object cont, object args)
{
  Cyc_check_num_args(data, "equal?", 2, args);
  return_closcall1(data, cont, equalp(car(args), cadr(args)));
}

void _length(void *data, object cont, object args)
{
  Cyc_check_num_args(data, "length", 1, args);
  {
    object obj = Cyc_length(data, car(args));
    return_closcall1(data, cont, obj);
}}

void _bytevector_91length(void *data, object cont, object args)
{
  Cyc_check_num_args(data, "bytevector-length", 1, args);
  {
    object obj = Cyc_bytevector_length(data, car(args));
    return_closcall1(data, cont, obj);
}}

void _bytevector_91u8_91ref(void *data, object cont, object args)
{
  Cyc_check_num_args(data, "bytevector-u8-ref", 2, args);
  {
    object c = Cyc_bytevector_u8_ref(data, car(args), cadr(args));
    return_closcall1(data, cont, c);
}}

void _bytevector_91u8_91set_67(void *data, object cont, object args)
{
  Cyc_check_num_args(data, "bytevector-u8-set!", 3, args);
  {
    object bv = Cyc_bytevector_u8_set(data, car(args), cadr(args), caddr(args));
    return_closcall1(data, cont, bv);
}}

void _bytevector(void *data, object cont, object args)
{
  object argc = Cyc_length(data, args);
  dispatch(data, obj_obj2int(argc), (function_type) dispatch_bytevector, cont,
           cont, args);
}

void _bytevector_91append(void *data, object cont, object args)
{
  object argc = Cyc_length(data, args);
  dispatch(data, obj_obj2int(argc),
           (function_type) dispatch_bytevector_91append, cont, cont, args);
}

void _Cyc_91bytevector_91copy(void *data, object cont, object args)
{
  Cyc_check_num_args(data, "Cyc-bytevector-copy", 3, args);
  Cyc_bytevector_copy(data, cont, car(args), cadr(args), caddr(args));
}

void _Cyc_91string_91_125utf8(void *data, object cont, object args)
{
  Cyc_check_num_args(data, "Cyc-string->utf8", 3, args);
  Cyc_string2utf8(data, cont, car(args), cadr(args), caddr(args));
}

void _Cyc_91utf8_91_125string(void *data, object cont, object args)
{
  Cyc_check_num_args(data, "Cyc-utf8->string", 3, args);
  Cyc_utf82string(data, cont, car(args), cadr(args), caddr(args));
}

void _vector_91length(void *data, object cont, object args)
{
  Cyc_check_num_args(data, "vector-length", 1, args);
  {
    object obj = Cyc_vector_length(data, car(args));
    return_closcall1(data, cont, obj);
}}

void _null_127(void *data, object cont, object args)
{
  Cyc_check_num_args(data, "null?", 1, args);
  return_closcall1(data, cont, Cyc_is_null(car(args)));
}

void _set_91car_67(void *data, object cont, object args)
{
  //Cyc_check_num_args(data, "set-car!", 2, args);
  //return_closcall1(data, cont, Cyc_set_car(data, car(args), cadr(args)));
  Cyc_check_num_args(data, "set-car!", 2, args);
  //Cyc_set_car2(data, cont, car(args), cadr(args));
  return_closcall1(data, cont, Cyc_set_car_cps(data, cont, car(args), cadr(args)));
}

void _set_91cdr_67(void *data, object cont, object args)
{
  //Cyc_check_num_args(data, "set-cdr!", 2, args);
  //return_closcall1(data, cont, Cyc_set_cdr(data, car(args), cadr(args)));
  Cyc_check_num_args(data, "set-cdr!", 2, args);
  //Cyc_set_cdr2(data, cont, car(args), cadr(args));
  return_closcall1(data, cont, Cyc_set_cdr_cps(data, cont, car(args), cadr(args)));
}

void _Cyc_91has_91cycle_127(void *data, object cont, object args)
{
  Cyc_check_num_args(data, "Cyc-has-cycle?", 1, args);
  return_closcall1(data, cont, Cyc_has_cycle(car(args)));
}

void _Cyc_91spawn_91thread_67(void *data, object cont, object args)
{
  Cyc_check_num_args(data, "Cyc-spawn-thread!", 1, args);
  // TODO: validate argument type?
  return_closcall1(data, cont, Cyc_spawn_thread(car(args)));
}

void _Cyc_91end_91thread_67(void *data, object cont, object args)
{
  Cyc_end_thread((gc_thread_data *) data);
  return_closcall1(data, cont, boolean_f);
}

void __87(void *data, object cont, object args)
{
  int argc = obj_obj2int(Cyc_length(data, args));
  dispatch(data, argc, (function_type) dispatch_sum, cont, cont, args);
}

void __91(void *data, object cont, object args)
{
  Cyc_check_num_args(data, "-", 1, args);
  {
    int argc = obj_obj2int(Cyc_length(data, args));
    dispatch(data, argc, (function_type) dispatch_sub, cont, cont, args);
}}

void __85(void *data, object cont, object args)
{
  int argc = obj_obj2int(Cyc_length(data, args));
  dispatch(data, argc, (function_type) dispatch_mul, cont, cont, args);
}

void __95(void *data, object cont, object args)
{
  Cyc_check_num_args(data, "/", 1, args);
  {
    int argc = obj_obj2int(Cyc_length(data, args));
    dispatch(data, argc, (function_type) dispatch_div, cont, cont, args);
}}

void _Cyc_91cvar_127(void *data, object cont, object args)
{
  Cyc_check_num_args(data, "Cyc-cvar?", 1, args);
  return_closcall1(data, cont, Cyc_is_cvar(car(args)));
}

void _Cyc_91opaque_127(void *data, object cont, object args)
{
  Cyc_check_num_args(data, "Cyc-opaque?", 1, args);
  return_closcall1(data, cont, Cyc_is_opaque(car(args)));
}

void _boolean_127(void *data, object cont, object args)
{
  Cyc_check_num_args(data, "boolean?", 1, args);
  return_closcall1(data, cont, Cyc_is_boolean(car(args)));
}

void _char_127(void *data, object cont, object args)
{
  Cyc_check_num_args(data, "char?", 1, args);
  return_closcall1(data, cont, Cyc_is_char(car(args)));
}

void _eof_91object_127(void *data, object cont, object args)
{
  Cyc_check_num_args(data, "eof_91object?", 1, args);
  return_closcall1(data, cont, Cyc_is_eof_object(car(args)));
}

void _number_127(void *data, object cont, object args)
{
  Cyc_check_num_args(data, "number?", 1, args);
  return_closcall1(data, cont, Cyc_is_number(car(args)));
}

void _real_127(void *data, object cont, object args)
{
  Cyc_check_num_args(data, "real?", 1, args);
  return_closcall1(data, cont, Cyc_is_real(car(args)));
}

void _integer_127(void *data, object cont, object args)
{
  Cyc_check_num_args(data, "integer?", 1, args);
  return_closcall1(data, cont, Cyc_is_integer(car(args)));
}

void _pair_127(void *data, object cont, object args)
{
  Cyc_check_num_args(data, "pair?", 1, args);
  return_closcall1(data, cont, Cyc_is_pair(car(args)));
}

void _procedure_127(void *data, object cont, object args)
{
  Cyc_check_num_args(data, "procedure?", 1, args);
  return_closcall1(data, cont, Cyc_is_procedure(data, car(args)));
}

void _macro_127(void *data, object cont, object args)
{
  Cyc_check_num_args(data, "macro?", 1, args);
  return_closcall1(data, cont, Cyc_is_macro(car(args)));
}

void _Cyc_91macro_127(void *data, object cont, object args)
{
  Cyc_check_num_args(data, "Cyc-macro?", 1, args);
  return_closcall1(data, cont, Cyc_is_macro(car(args)));
}

void _port_127(void *data, object cont, object args)
{
  Cyc_check_num_args(data, "port?", 1, args);
  return_closcall1(data, cont, Cyc_is_port(car(args)));
}

void _bytevector_127(void *data, object cont, object args)
{
  Cyc_check_num_args(data, "bytevector?", 1, args);
  return_closcall1(data, cont, Cyc_is_bytevector(car(args)));
}

void _vector_127(void *data, object cont, object args)
{
  Cyc_check_num_args(data, "vector?", 1, args);
  return_closcall1(data, cont, Cyc_is_vector(car(args)));
}

void _string_127(void *data, object cont, object args)
{
  Cyc_check_num_args(data, "string?", 1, args);
  return_closcall1(data, cont, Cyc_is_string(car(args)));
}

void _symbol_127(void *data, object cont, object args)
{
  Cyc_check_num_args(data, "symbol?", 1, args);
  return_closcall1(data, cont, Cyc_is_symbol(car(args)));
}

void _Cyc_91get_91cvar(void *data, object cont, object args)
{
  Cyc_check_num_args(data, "Cyc-get-cvar", 1, args);
  return_closcall1(data, cont, Cyc_get_cvar((car(args))));
}

void _Cyc_91set_91cvar_67(void *data, object cont, object args)
{
  printf("not implemented\n");
  exit(1);
}

/* Note we cannot use _exit (per convention) because it is reserved by C */
void _cyc_exit(void *data, object cont, object args)
{
  if (args == NULL)
    __halt(NULL);
  __halt(car(args));
}

void __75halt(void *data, object cont, object args)
{
#if DEBUG_SHOW_DIAG
  gc_print_stats(Cyc_heap);
#endif
  exit(0);
}

void _cell_91get(void *data, object cont, object args)
{
  printf("not implemented\n");
  exit(1);
}

void _set_91global_67(void *data, object cont, object args)
{
  printf("not implemented\n");
  exit(1);
}

void _set_91cell_67(void *data, object cont, object args)
{
  printf("not implemented\n");
  exit(1);
}

void _cell(void *data, object cont, object args)
{
  printf("not implemented\n");
  exit(1);
}

void __123(void *data, object cont, object args)
{
  int argc = obj_obj2int(Cyc_length(data, args));
  dispatch(data, argc, (function_type) dispatch_num_eq, cont, cont, args);
}

void __125(void *data, object cont, object args)
{
  int argc = obj_obj2int(Cyc_length(data, args));
  dispatch(data, argc, (function_type) dispatch_num_gt, cont, cont, args);
}

void __121(void *data, object cont, object args)
{
  int argc = obj_obj2int(Cyc_length(data, args));
  dispatch(data, argc, (function_type) dispatch_num_lt, cont, cont, args);
}

void __125_123(void *data, object cont, object args)
{
  int argc = obj_obj2int(Cyc_length(data, args));
  dispatch(data, argc, (function_type) dispatch_num_gte, cont, cont,
           args);
}

void __121_123(void *data, object cont, object args)
{
  int argc = obj_obj2int(Cyc_length(data, args));
  dispatch(data, argc, (function_type) dispatch_num_lte, cont, cont,
           args);
}

void _apply(void *data, object cont, object args)
{
//  Cyc_check_num_args(data, "apply", 2, args);
//  apply(data, cont, car(args), cadr(args));
  object argc = Cyc_length(data, args);

  //fprintf(stdout, "_apply received args: ");
  //Cyc_display(data, args, stdout);
  //fprintf(stdout, "\n");
  dispatch(data, obj_obj2int(argc), (function_type)dispatch_apply_va, cont, cont, args);
}

void _assq(void *data, object cont, object args)
{
  Cyc_check_num_args(data, "assq  ", 2, args);
  return_closcall1(data, cont, assq(data, car(args), cadr(args)));
}

void _assv(void *data, object cont, object args)
{
  Cyc_check_num_args(data, "assv  ", 2, args);
  return_closcall1(data, cont, assq(data, car(args), cadr(args)));
}

void _memq(void *data, object cont, object args)
{
  Cyc_check_num_args(data, "memq", 2, args);
  return_closcall1(data, cont, memqp(data, car(args), cadr(args)));
}

void _memv(void *data, object cont, object args)
{
  Cyc_check_num_args(data, "memv", 2, args);
  return_closcall1(data, cont, memqp(data, car(args), cadr(args)));
}

void _char_91_125integer(void *data, object cont, object args)
{
  Cyc_check_num_args(data, "char->integer", 1, args);
  {
    object obj = Cyc_char2integer(car(args));
    return_closcall1(data, cont, obj);
}}

void _integer_91_125char(void *data, object cont, object args)
{
  Cyc_check_num_args(data, "integer->char", 1, args);
  return_closcall1(data, cont, Cyc_integer2char(data, car(args)));
}

void _string_91_125number(void *data, object cont, object args)
{
  Cyc_check_num_args(data, "string->number", 1, args);
  {
    object tail = cdr(args);
    if (tail) {
      Cyc_string2number2_(data, cont, 2, car(args), cadr(args));
    } else {
      Cyc_string2number_(data, cont, car(args));
    }
  }
}

void _string_91length(void *data, object cont, object args)
{
  Cyc_check_num_args(data, "string-length", 1, args);
  {
    object obj = Cyc_string_length(data, car(args));
    return_closcall1(data, cont, obj);
}}

void _cyc_substring(void *data, object cont, object args)
{
  Cyc_check_num_args(data, "substring", 3, args);
  Cyc_substring(data, cont, car(args), cadr(args), caddr(args));
}

void _cyc_string_91set_67(void *data, object cont, object args)
{
  Cyc_check_num_args(data, "string-set!", 3, args);
  {
    object s = Cyc_string_set(data, car(args), cadr(args), caddr(args));
    return_closcall1(data, cont, s);
}}

void _cyc_string_91ref(void *data, object cont, object args)
{
  Cyc_check_num_args(data, "string-ref", 2, args);
  {
    object c = Cyc_string_ref(data, car(args), cadr(args));
    return_closcall1(data, cont, c);
}}

void _Cyc_91installation_91dir(void *data, object cont, object args)
{
  Cyc_check_num_args(data, "Cyc-installation-dir", 1, args);
  Cyc_installation_dir(data, cont, car(args));
}

void _Cyc_91compilation_91environment(void *data, object cont, object args)
{
  Cyc_check_num_args(data, "Cyc-compilation-environment", 1, args);
  Cyc_compilation_environment(data, cont, car(args));
}

void _command_91line_91arguments(void *data, object cont, object args)
{
  object cmdline = Cyc_command_line_arguments(data, cont);
  return_closcall1(data, cont, cmdline);
}

void _cyc_system(void *data, object cont, object args)
{
  Cyc_check_num_args(data, "system", 1, args);
  {
    object obj = Cyc_system(car(args));
    return_closcall1(data, cont, obj);
}}

void _Cyc_91current_91exception_91handler(void *data, object cont, object args)
{
  object handler = Cyc_current_exception_handler(data);
  return_closcall1(data, cont, handler);
}

void _Cyc_91default_91exception_91handler(void *data, object cont, object args)
{
  // TODO: this is a quick-and-dirty implementation, may be a better way to write this
  Cyc_default_exception_handler(data, 1, args, car(args));
}

void _string_91cmp(void *data, object cont, object args)
{
  Cyc_check_num_args(data, "string-cmp", 2, args);
  {
    object obj = Cyc_string_cmp(data, car(args), cadr(args));
    return_closcall1(data, cont, obj);
}}

void _string_91append(void *data, object cont, object args)
{
  object argc = Cyc_length(data, args);
  dispatch(data, obj_obj2int(argc), (function_type) dispatch_string_91append,
           cont, cont, args);
}

void _make_91vector(void *data, object cont, object args)
{
  Cyc_check_num_args(data, "make-vector", 1, args);
  {
    object argc = Cyc_length(data, args);
    if (obj_obj2int(argc) >= 2) {
      Cyc_make_vector(data, cont, 2, car(args), cadr(args));
    } else {
      Cyc_make_vector(data, cont, 2, car(args), boolean_f);
    }
  }
}

void _make_91bytevector(void *data, object cont, object args)
{
  Cyc_check_num_args(data, "make-bytevector", 1, args);
  {
    object argc = Cyc_length(data, args);
    if (obj_obj2int(argc) >= 2) {
      Cyc_make_bytevector(data, cont, 2, car(args), cadr(args));
    } else {
      Cyc_make_bytevector(data, cont, 1, car(args));
    }
  }
}

void _vector_91ref(void *data, object cont, object args)
{
  Cyc_check_num_args(data, "vector-ref", 2, args);
  {
    object ref = Cyc_vector_ref(data, car(args), cadr(args));
    return_closcall1(data, cont, ref);
}}

void _vector_91set_67(void *data, object cont, object args)
{
  Cyc_check_num_args(data, "vector-set!", 3, args);
  {
    object ref = Cyc_vector_set_cps(data, cont, car(args), cadr(args), caddr(args));
    return_closcall1(data, cont, ref);
}}

void _list_91_125vector(void *data, object cont, object args)
{
  Cyc_check_num_args(data, "list->vector", 1, args);
  Cyc_list2vector(data, cont, car(args));
}

void _list_91_125string(void *data, object cont, object args)
{
  Cyc_check_num_args(data, "list->string", 1, args);
  Cyc_list2string(data, cont, car(args));
}

void _string_91_125symbol(void *data, object cont, object args)
{
  Cyc_check_num_args(data, "string->symbol", 1, args);
  return_closcall1(data, cont, Cyc_string2symbol(data, car(args)));
}

void _symbol_91_125string(void *data, object cont, object args)
{
  Cyc_check_num_args(data, "symbol->string", 1, args);
  Cyc_symbol2string(data, cont, car(args));
}

void _number_91_125string(void *data, object cont, object args)
{
  Cyc_check_num_args(data, "number->string", 1, args);
  {
    object tail = cdr(args);
    if (tail) {
      Cyc_number2string2(data, cont, 2, car(args), cadr(args));
    } else {
      Cyc_number2string2(data, cont, 1, car(args));
    }
  }
}

void _open_91input_91file(void *data, object cont, object args)
{
  Cyc_check_num_args(data, "open-input-file", 1, args);
  {
    port_type p = Cyc_io_open_input_file(data, car(args));
    return_closcall1(data, cont, &p);
}}

void _open_91output_91file(void *data, object cont, object args)
{
  Cyc_check_num_args(data, "open-output-file", 1, args);
  {
    port_type p = Cyc_io_open_output_file(data, car(args));
    return_closcall1(data, cont, &p);
}}

void _open_91binary_91input_91file(void *data, object cont, object args)
{
  Cyc_check_num_args(data, "open-binary-input-file", 1, args);
  {
    port_type p = Cyc_io_open_binary_input_file(data, car(args));
    return_closcall1(data, cont, &p);
}}

void _open_91binary_91output_91file(void *data, object cont, object args)
{
  Cyc_check_num_args(data, "open-binary-output-file", 1, args);
  {
    port_type p = Cyc_io_open_binary_output_file(data, car(args));
    return_closcall1(data, cont, &p);
}}

void _close_91port(void *data, object cont, object args)
{
  Cyc_check_num_args(data, "close-port", 1, args);
  return_closcall1(data, cont, Cyc_io_close_port(data, car(args)));
}

void _close_91input_91port(void *data, object cont, object args)
{
  Cyc_check_num_args(data, "close-input-port", 1, args);
  return_closcall1(data, cont, Cyc_io_close_input_port(data, car(args)));
}

void _close_91output_91port(void *data, object cont, object args)
{
  Cyc_check_num_args(data, "close-output-port", 1, args);
  return_closcall1(data, cont, Cyc_io_close_output_port(data, car(args)));
}

void _Cyc_91flush_91output_91port(void *data, object cont, object args)
{
  Cyc_check_num_args(data, "Cyc-flush-output-port", 1, args);
  return_closcall1(data, cont, Cyc_io_flush_output_port(data, car(args)));
}

void _file_91exists_127(void *data, object cont, object args)
{
  Cyc_check_num_args(data, "file-exists?", 1, args);
  return_closcall1(data, cont, Cyc_io_file_exists(data, car(args)));
}

void _delete_91file(void *data, object cont, object args)
{
  Cyc_check_num_args(data, "delete-file", 1, args);
  return_closcall1(data, cont, Cyc_io_delete_file(data, car(args)));
}

void _read_91char(void *data, object cont, object args)
{
  Cyc_check_num_args(data, "read-char", 1, args);
  return_closcall1(data, cont, Cyc_io_read_char(data, cont, car(args)));
}

void _peek_91char(void *data, object cont, object args)
{
  Cyc_check_num_args(data, "peek-char", 1, args);
  return_closcall1(data, cont, Cyc_io_peek_char(data, cont, car(args)));
}

void _Cyc_91read_91line(void *data, object cont, object args)
{
  Cyc_check_num_args(data, "Cyc-read-line", 1, args);
  Cyc_io_read_line(data, cont, car(args));
}

void _Cyc_91write_91char(void *data, object cont, object args)
{
  Cyc_check_num_args(data, "write-char", 2, args);
  return_closcall1(data, cont, Cyc_write_char(data, car(args), cadr(args)));
}

void _Cyc_91write(void *data, object cont, object args)
{
  Cyc_check_num_args(data, "write", 1, args);
  {
    object argc = Cyc_length(data, args);
    dispatch(data, obj_obj2int(argc), (function_type) dispatch_write_va, cont,
             cont, args);
}}

void _display(void *data, object cont, object args)
{
  Cyc_check_num_args(data, "display", 1, args);
  {
    object argc = Cyc_length(data, args);
    dispatch(data, obj_obj2int(argc), (function_type) dispatch_display_va, cont,
             cont, args);
}}

void _call_95cc(void *data, object cont, object args)
{
  Cyc_check_num_args(data, "call/cc", 1, args);
  if ((boolean_f == Cyc_is_procedure(data, car(args)))) {
    Cyc_invalid_type_error(data, closure1_tag, car(args));
  }
  return_closcall2(data, __glo_call_95cc_scheme_base, cont, car(args));
}

// Front-end to apply
//
// Core of va processing is done here, because we need different
// functions for apply_va and dispatch_apply_va, and those functions
// need to start and end va. BUT, we need to allocate new objects
// so this stuff can't be returned, so a workaround is to put it in
// this macro.
//
// Fast path is just to take list, if we only have func and 1 arg.
// Otherwise append all args together into a single list, per r7rs.
#define do_apply_va \
  va_start(ap, func); \
  if (argc == 2) { \
    lis = va_arg(ap, object); \
    Cyc_check_pair_or_null(data, lis); \
  } else { \
    lis = alloca(sizeof(pair_type)); \
    tmp = va_arg(ap, object); \
    set_pair(lis, tmp, NULL); \
    prev = lis; \
    for (i = 2; i < argc - 1; i++) { \
      pair_type *next = alloca(sizeof(pair_type)); \
      tmp = va_arg(ap, object); \
      set_pair(next, tmp, NULL); \
      cdr(prev) = next; \
      prev = next; \
    } \
    tmp = va_arg(ap, object); \
    cdr(prev) = tmp; \
  } \
  va_end(ap);

void dispatch_apply_va(void *data, int argc, object clo, object cont, object func, ...)
{
  list lis = NULL, prev = NULL;
  object tmp;
  int i;
  va_list ap;
  argc = argc - 1; // Required for "dispatch" function
  do_apply_va
  apply(data, cont, func, lis);
}

object apply_va(void *data, object cont, int argc, object func, ...)
{
  list lis = NULL, prev = NULL;
  object tmp;
  int i;
  va_list ap;
  do_apply_va
  return apply(data, cont, func, lis); // Never actually returns
}

/*
 * @param cont - Continuation for the function to call into
 * @param func - Function to execute
 * @param args - A list of arguments to the function
 */
object apply(void *data, object cont, object func, object args)
{
  object count;

//printf("DEBUG apply: ");
//Cyc_display(data, args);
//printf("\n");
  if (!is_object_type(func)) {
    Cyc_rt_raise2(data, "Call of non-procedure: ", func);
  }
  // Causes problems...
  //Cyc_check_pair_or_null(args);

  switch (type_of(func)) {
  case primitive_tag:
    // TODO: should probably check arg counts and error out if needed
    ((primitive_type *) func)->fn(data, cont, args);
    break;
  case macro_tag:
  case closure0_tag:
  case closure1_tag:
  case closureN_tag:
    if (func == Cyc_glo_call_cc) {
//      make_pair(c, cont, args);
//Cyc_display(data, args, stderr);
//        args = &c;
//Cyc_display(data, &c, stderr);
      count = Cyc_length(data, args);
      Cyc_check_num_args(data, "<procedure>", 1, args);
      dispatch(data, obj_obj2int(count), ((closure) func)->fn, func, cont,
               args);
    }
    count = Cyc_length(data, args);
    // TODO: validate number of args provided:
    Cyc_check_num_args(data, "<procedure>", ((closure) func)->num_args, args);  // TODO: could be more efficient, eg: cyc_length(args) is called twice.
    dispatch(data, obj_obj2int(count), ((closure) func)->fn, func, cont, args);
    break;

  case pair_tag:
    {
      // TODO: should add more error checking here, make sure car(func) is a symbol
      object fobj = car(func);

      if (!is_object_type(fobj) || type_of(fobj) != symbol_tag) {
        Cyc_rt_raise2(data, "Call of non-procedure: ", func);
      } else if (strncmp(((symbol) fobj)->desc, "lambda", 7) == 0) {
        make_pair(c, func, args);
        //printf("JAE DEBUG, sending to eval: ");
        //Cyc_display(data, &c, stderr);
        ((closure) Cyc_glo_eval_from_c)->fn(data, 2, Cyc_glo_eval_from_c, cont,
                                            &c, NULL);

        // TODO: would be better to compare directly against symbols here,
        //       but need a way of looking them up ahead of time.
        //       maybe a libinit() or such is required.
      } else if (strncmp(((symbol) fobj)->desc, "primitive", 10) == 0) {
        make_pair(c, cadr(func), args);
        ((closure) Cyc_glo_eval_from_c)->fn(data, 3, Cyc_glo_eval_from_c, cont,
                                            &c, NULL);
      } else if (strncmp(((symbol) fobj)->desc, "procedure", 10) == 0) {
        make_pair(c, func, args);
        ((closure) Cyc_glo_eval_from_c)->fn(data, 3, Cyc_glo_eval_from_c, cont,
                                            &c, NULL);
      } else {
        make_pair(c, func, args);
        Cyc_rt_raise2(data, "Unable to evaluate: ", &c);
      }
    }

  default: {
      Cyc_rt_raise2(data, "Call of non-procedure: ", func);
    }
  }
  return NULL;                  // Never reached
}

// Version of apply meant to be called from within compiled code
void Cyc_apply(void *data, int argc, closure cont, object prim, ...)
{
  va_list ap;
  object tmp;
  int i;
  list args = alloca(sizeof(pair_type) * argc);

  va_start(ap, prim);

  for (i = 0; i < argc; i++) {
    tmp = va_arg(ap, object);
    args[i].hdr.mark = gc_color_red;
    args[i].hdr.grayed = 0;
    args[i].hdr.immutable = 0;
    args[i].tag = pair_tag;
    args[i].pair_car = tmp;
    args[i].pair_cdr = (i == (argc - 1)) ? NULL : &args[i + 1];
  }
  //printf("DEBUG applying primitive to ");
  //Cyc_display(data, (object)&args[0]);
  //printf("\n");

  va_end(ap);
  apply(data, cont, prim, (argc > 0)
        ? (object) & args[0]
        : NULL);
}

// END apply

/* Extract args from given array, assuming cont is the first arg in buf */
void Cyc_apply_from_buf(void *data, int argc, object prim, object * buf)
{
  list args;
  object cont;
  int i;

  if (argc == 0) {
    printf("Internal error in Cyc_apply_from_buf, argc is 0\n");
    exit(1);
  }

  args = alloca(sizeof(pair_type) * (argc - 1));
  cont = buf[0];

  for (i = 1; i < argc; i++) {
    args[i - 1].hdr.mark = gc_color_red;
    args[i - 1].hdr.grayed = 0;
    args[i - 1].hdr.immutable = 0;
    args[i - 1].tag = pair_tag;
    args[i - 1].pair_car = buf[i];
    args[i - 1].pair_cdr = (i == (argc - 1)) ? NULL : &args[i];
  }

  apply(data, cont, prim, (object) & args[0]);
}

/**
 * Start a thread's trampoline
 */
void Cyc_start_trampoline(gc_thread_data * thd)
{
  // Tank, load the jump program
  setjmp(*(thd->jmp_start));

#if DEBUG_GC
  printf("Done with GC\n");
#endif

  if (obj_is_not_closure(thd->gc_cont)) {
    Cyc_apply_from_buf(thd, thd->gc_num_args, thd->gc_cont, thd->gc_args);
  } else {
    do_dispatch(thd, thd->gc_num_args, ((closure) (thd->gc_cont))->fn,
                thd->gc_cont, thd->gc_args);
  }

  fprintf(stderr, "Internal error: should never have reached this line\n");
  exit(1);
}

/**
 * @brief A helper function for calling `gc_mark_globals`.
 */
void gc_request_mark_globals(void)
{
  gc_mark_globals(Cyc_global_variables, global_table);
}

/**
 * @brief Add an object to the move buffer
 * @param d Mutator data object containing the buffer
 * @param alloci  Pointer to the next open slot in the buffer
 * @param obj     Object to add
 */
static void gc_thr_add_to_move_buffer(gc_thread_data * d, int *alloci, object obj)
{
  if (*alloci == d->moveBufLen) {
    gc_thr_grow_move_buffer(d);
  }

  d->moveBuf[*alloci] = obj;
  (*alloci)++;
}

static char *gc_fixup_moved_obj(gc_thread_data * thd, int *alloci, char *obj,
                         object hp)
{
  int acquired_lock = 0;
  if (grayed(obj)) {
    // Try to acquire the lock, because we are already locked if
    // the collector is cooperating on behalf of the mutator
    if (pthread_mutex_trylock(&(thd->lock)) == 0) {
      acquired_lock = 1;
    }
    gc_mark_gray2(thd, hp);
    if (acquired_lock) {
      pthread_mutex_unlock(&(thd->lock));
    }
  }
  // hp ==> new heap object, point to it from old stack object
  forward(obj) = hp;
  type_of(obj) = forward_tag;
  // keep track of each allocation so we can scan/move 
  // the whole live object 'tree'
  gc_thr_add_to_move_buffer(thd, alloci, hp);
  return (char *)hp;
}

static char *gc_move(char *obj, gc_thread_data * thd, int *alloci, int *heap_grown)
{
  gc_heap_root *heap = thd->heap;
  if (!is_object_type(obj))
    return obj;
  switch (type_of(obj)) {
  case closureN_tag:{
      closureN_type *hp = gc_alloc(heap,
                                   sizeof(closureN_type) +
                                   sizeof(object) *
                                   (((closureN) obj)->num_elements),
                                   obj, thd, heap_grown);
      return gc_fixup_moved_obj(thd, alloci, obj, hp);
    }
  case pair_tag:{
      list hp = gc_alloc(heap, sizeof(pair_type), obj, thd, heap_grown);
      return gc_fixup_moved_obj(thd, alloci, obj, hp);
    }
  case string_tag:{
      string_type *hp = gc_alloc(heap,
                                 sizeof(string_type) + ((string_len(obj) + 1)),
                                 obj, thd, heap_grown);
      return gc_fixup_moved_obj(thd, alloci, obj, hp);
    }
  case double_tag:{
      double_type *hp =
          gc_alloc(heap, sizeof(double_type), obj, thd, heap_grown);
      return gc_fixup_moved_obj(thd, alloci, obj, hp);
    }
  case vector_tag:{
      vector_type *hp = gc_alloc(heap,
                                 sizeof(vector_type) +
                                 sizeof(object) *
                                 (((vector) obj)->num_elements),
                                 obj, thd, heap_grown);
      return gc_fixup_moved_obj(thd, alloci, obj, hp);
    }
  case bytevector_tag:{
      bytevector_type *hp = gc_alloc(heap,
                                     sizeof(bytevector_type) +
                                     sizeof(char) * (((bytevector) obj)->len),
                                     obj, thd, heap_grown);
      return gc_fixup_moved_obj(thd, alloci, obj, hp);
    }
  case port_tag:{
      port_type *hp =
          gc_alloc(heap, sizeof(port_type), obj, thd, heap_grown);
      return gc_fixup_moved_obj(thd, alloci, obj, hp);
    }
  case bignum_tag:{
      bignum_type *hp = 
          gc_alloc(heap, sizeof(bignum_type), obj, thd, heap_grown);
      return gc_fixup_moved_obj(thd, alloci, obj, hp);
  }
  case cvar_tag:{
      cvar_type *hp =
          gc_alloc(heap, sizeof(cvar_type), obj, thd, heap_grown);
      return gc_fixup_moved_obj(thd, alloci, obj, hp);
    }
  case macro_tag:{
      macro_type *hp =
          gc_alloc(heap, sizeof(macro_type), obj, thd, heap_grown);
      return gc_fixup_moved_obj(thd, alloci, obj, hp);
    }
  case closure1_tag:{
      closure1_type *hp =
          gc_alloc(heap, sizeof(closure1_type), obj, thd, heap_grown);
      return gc_fixup_moved_obj(thd, alloci, obj, hp);
    }
  case c_opaque_tag:{
      c_opaque_type *hp =
          gc_alloc(heap, sizeof(c_opaque_type), obj, thd, heap_grown);
      return gc_fixup_moved_obj(thd, alloci, obj, hp);
    }
  case closure0_tag:
    break;
  case forward_tag:
    return (char *)forward(obj);
  case eof_tag:
  case void_tag:
    break;
  case primitive_tag:
    break;
  case boolean_tag:
    break;
  case symbol_tag:
    break;                      // JAE TODO: raise an error here? Should not be possible in real code, though (IE, without GC DEBUG flag)
  case integer_tag:{
      integer_type *hp =
          gc_alloc(heap, sizeof(integer_type), obj, thd, heap_grown);
      return gc_fixup_moved_obj(thd, alloci, obj, hp);
    }
  case complex_num_tag:{
      complex_num_type *hp =
          gc_alloc(heap, sizeof(complex_num_type), obj, thd, heap_grown);
      return gc_fixup_moved_obj(thd, alloci, obj, hp);
    }
  default:
    fprintf(stderr, "gc_move: bad tag obj=%p obj.tag=%d\n", (object) obj,
            type_of(obj));
    exit(1);
  }
  return (char *)obj;
}

#define gc_move2heap(obj) { \
  temp = obj; \
  if (stack_overflow(low_limit, temp) && \
      stack_overflow(temp, high_limit)){ \
    (obj) = (object) gc_move(temp, (gc_thread_data *)data, &alloci, &heap_grown); \
  } \
}

/**
 * @brief Trigger a minor GC for the calling thread.
 * @param data Thread data object for the caller.
 * @param cont Continuation to invoke after GC.
 */
object Cyc_trigger_minor_gc(void *data, object cont)
{
  gc_thread_data *thd = (gc_thread_data *) data;
  thd->gc_args[0] = boolean_t;
  GC(data, cont, thd->gc_args, 1);
  return NULL;
}

/**
 * Do a minor GC, tracing all of the live objects from the calling thread's
 * stack and moving them to the heap.
 * \ingroup gc_minor
 */
int gc_minor(void *data, object low_limit, object high_limit, closure cont,
             object * args, int num_args)
{
  object temp;
  int i;
  int scani = 0, alloci = 0;
  int heap_grown = 0;

#if GC_DEBUG_VERBOSE
  fprintf(stderr, "started minor GC\n");
#endif

//fprintf(stdout, "DEBUG, started minor GC\n"); // JAE DEBUG
  // Prevent overrunning buffer
  if (num_args > NUM_GC_ARGS) {
    printf("Fatal error - too many arguments (%d) to GC\n", num_args);
    exit(1);
  }

  gc_move2heap(cont);
  ((gc_thread_data *) data)->gc_cont = cont;
  ((gc_thread_data *) data)->gc_num_args = num_args;

  for (i = 0; i < num_args; i++) {
    gc_move2heap(args[i]);
    ((gc_thread_data *) data)->gc_args[i] = args[i];
  }

  // Transport exception stack
  gc_move2heap(((gc_thread_data *) data)->exception_handler_stack);
  gc_move2heap(((gc_thread_data *) data)->param_objs);
  gc_move2heap(((gc_thread_data *) data)->scm_thread_obj);

  // Transport mutations
  {
    int l = 0;
    while (l < ((gc_thread_data *) data)->mutation_count) {
      object o = ((gc_thread_data *) data)->mutations[l++];
      if (is_value_type(o)) {
        // Can happen if a vector element was already
        // moved and we found an index. Just ignore it
      } else if (type_of(o) == pair_tag) {
        gc_move2heap(car(o));
        gc_move2heap(cdr(o));
      } else if (type_of(o) == vector_tag) {
        int i;
        object idx;
        // For vectors, index is encoded as the next mutation
        idx = ((gc_thread_data *) data)->mutations[l++];
        i = obj_obj2int(idx);
        gc_move2heap(((vector) o)->elements[i]);
      } else if (type_of(o) == forward_tag) {
        // Already transported, skip
      } else if (type_of(o) == c_opaque_tag) {
        // Special case, pull out vector and inspect each element
        vector_type *v = opaque_ptr(o);
        int i;
        for (i = 0; i < ((vector) v)->num_elements; i++) {
          gc_move2heap(((vector) v)->elements[i]);
        }
      } else if (type_of(o) == cvar_tag) {
        cvar_type *c = (cvar_type *) o;
        gc_move2heap(*(c->pvar)); // Transport underlying global, not the pvar
      } else {
        printf("Unexpected type %d transporting mutation\n", type_of(o));
        exit(1);
      }
    }
  }
  clear_mutations(data);        // Reset for next time

  // Collect globals but only if a change was made. This avoids traversing a
  // long list of objects unless absolutely necessary.
  if (((gc_thread_data *) data)->globals_changed) {
      ((gc_thread_data *) data)->globals_changed = 0;
    // Transport globals
    gc_move2heap(Cyc_global_variables);   // Internal global used by the runtime
    {
      list l = global_table;
      for (; l != NULL; l = cdr(l)) {
        cvar_type *c = (cvar_type *) car(l);
        gc_move2heap(*(c->pvar)); // Transport underlying global, not the pvar
      }
    }
  }

  // Check allocated objects, moving additional objects as needed
  while (scani < alloci) {
    object obj = ((gc_thread_data *) data)->moveBuf[scani];
    switch (type_of(obj)) {
    case pair_tag:{
        gc_move2heap(car(obj));
        gc_move2heap(cdr(obj));
        break;
      }
    case closure1_tag:
      gc_move2heap(((closure1) obj)->element);
      break;
    case closureN_tag:{
        int i, n = ((closureN) obj)->num_elements;
        for (i = 0; i < n; i++) {
          gc_move2heap(((closureN) obj)->elements[i]);
        }
        break;
      }
    case vector_tag:{
        int i, n = ((vector) obj)->num_elements;
        for (i = 0; i < n; i++) {
          gc_move2heap(((vector) obj)->elements[i]);
        }
        break;
      }
      // No child objects to move
    case macro_tag:
    case bytevector_tag:
    case string_tag:
    case integer_tag:
    case bignum_tag:
    case double_tag:
    case port_tag:
    case cvar_tag:
    case c_opaque_tag:
    case complex_num_tag:
      break;
      // These types are not heap-allocated
    case eof_tag:
    case void_tag:
    case primitive_tag:
    case symbol_tag:
    case boolean_tag:
    case closure0_tag:
    default:
      fprintf(stderr,
              "GC: unexpected object type %d for object %p\n", type_of(obj),
              obj);
      exit(1);
    }
    scani++;
  }
#if GC_DEBUG_VERBOSE
  fprintf(stderr, "done with minor GC\n");
#endif
  return alloci;
}

/**
 * Run a minor GC from a mutator thread.
 * This function runs the core GC algorithm, cooperates with
 * the collector, and then calls its continuation.
 */
void GC(void *data, closure cont, object * args, int num_args)
{
  char tmp;
  object low_limit = &tmp;      // This is one end of the stack...
  object high_limit = ((gc_thread_data *) data)->stack_start;
#ifdef CYC_HIGH_RES_TIMERS
long long tstamp = hrt_get_current();
#endif
  int alloci = gc_minor(data, low_limit, high_limit, cont, args, num_args);
  // Cooperate with the collector thread
  gc_mut_cooperate((gc_thread_data *) data, alloci);
#ifdef CYC_HIGH_RES_TIMERS
hrt_log_delta("minor gc", tstamp);
#endif
  // Let it all go, Neo...
  longjmp(*(((gc_thread_data *) data)->jmp_start), 1);
}

/**
 * Move a thread-local object to the heap
 */

void Cyc_make_shared_object(void *data, object k, object obj)
{
  gc_thread_data *thd = (gc_thread_data *)data;
  gc_heap_root *heap = thd->heap;
  object buf[1];
  int tmp, *heap_grown = &tmp;
  if (!is_object_type(obj) || // Immediates do not have to be moved
      !gc_is_stack_obj(&tmp, data, obj)) { // Not thread-local, assume already on heap
    return_closcall1(data, k, obj);
  }

  switch(type_of(obj)) {
  // These are never on the stack, ignore them
  //  cond_var_tag    = 6
  //  mutex_tag       = 14
  //  atomic_tag      = 22
  //  boolean_tag     = 0
  //  bignum_tag      = 12
  //  symbol_tag      = 19
  //  closure0_tag    = 3
  //  eof_tag         = 9
  //  void_tag
  //  macro_tag       = 13
  //  primitive_tag   = 17

  // Copy stack-allocated objects with no children to the heap:
  case string_tag:
  case double_tag:
  case bytevector_tag:
  case port_tag:
  case c_opaque_tag:
  case complex_num_tag: {
    object hp = gc_alloc(heap, gc_allocated_bytes(obj, NULL, NULL), obj, thd, heap_grown);
    return_closcall1(data, k, hp);
  }
  // Objs w/children force minor GC to guarantee everything is relocated:
  case cvar_tag:
  case closure1_tag:
  case closureN_tag:
  case pair_tag:
  case vector_tag:
    buf[0] = obj;
    GC(data, k, buf, 1);
    break;
  default:
    printf("Invalid shared object type %d\n", type_of(obj));
    exit(1);
  }
}

/**
 * Receive a list of arguments and apply them to the given function
 */
void dispatch(void *data, int argc, function_type func, object clo, object cont,
              object args)
{
  object b[argc + 1];           // OK to do this? Is this portable?
  int i;

  argc++;
  b[0] = cont;
  for (i = 1; i < argc; i++) {
    b[i] = car(args);
    args = cdr(args);
  }

  do_dispatch(data, argc, func, clo, b);
}

/**
 * Same as above but for a varargs C function
 */
void dispatch_va(void *data, int argc, function_type_va func, object clo,
                 object cont, object args)
{
  object b[argc + 1];           // OK to do this? Is this portable?
  int i;

  argc++;
  b[0] = cont;
  for (i = 1; i < argc; i++) {
    b[i] = car(args);
    args = cdr(args);
  }

  do_dispatch(data, argc, (function_type) func, clo, b);
}

static primitive_type Cyc_91global_91vars_primitive =
    { {0}, primitive_tag, "Cyc-global-vars", &_Cyc_91global_91vars };
static primitive_type Cyc_91get_91cvar_primitive =
    { {0}, primitive_tag, "Cyc-get-cvar", &_Cyc_91get_91cvar };
static primitive_type Cyc_91set_91cvar_67_primitive =
    { {0}, primitive_tag, "Cyc-set-cvar!", &_Cyc_91set_91cvar_67 };
static primitive_type Cyc_91cvar_127_primitive =
    { {0}, primitive_tag, "Cyc-cvar?", &_Cyc_91cvar_127 };
static primitive_type Cyc_91opaque_127_primitive =
    { {0}, primitive_tag, "Cyc-opaque?", &_Cyc_91opaque_127 };
static primitive_type Cyc_91has_91cycle_127_primitive =
    { {0}, primitive_tag, "Cyc-has-cycle?", &_Cyc_91has_91cycle_127 };
static primitive_type Cyc_91spawn_91thread_67_primitive =
    { {0}, primitive_tag, "Cyc-spawn-thread!", &_Cyc_91spawn_91thread_67 };
static primitive_type Cyc_91end_91thread_67_primitive =
    { {0}, primitive_tag, "Cyc-end-thread!", &_Cyc_91end_91thread_67 };
static primitive_type _87_primitive = { {0}, primitive_tag, "+", &__87 };
static primitive_type _91_primitive = { {0}, primitive_tag, "-", &__91 };
static primitive_type _85_primitive = { {0}, primitive_tag, "*", &__85 };
static primitive_type _95_primitive = { {0}, primitive_tag, "/", &__95 };
static primitive_type _123_primitive = { {0}, primitive_tag, "=", &__123 };
static primitive_type _125_primitive = { {0}, primitive_tag, ">", &__125 };
static primitive_type _121_primitive = { {0}, primitive_tag, "<", &__121 };
static primitive_type _125_123_primitive =
    { {0}, primitive_tag, ">=", &__125_123 };
static primitive_type _121_123_primitive =
    { {0}, primitive_tag, "<=", &__121_123 };
static primitive_type apply_primitive =
    { {0}, primitive_tag, "apply", &_apply };
static primitive_type _75halt_primitive =
    { {0}, primitive_tag, "%halt", &__75halt };
static primitive_type exit_primitive =
    { {0}, primitive_tag, "exit", &_cyc_exit };
static primitive_type Cyc_91current_91exception_91handler_primitive =
    { {0}, primitive_tag, "Cyc_current_exception_handler",
&_Cyc_91current_91exception_91handler
};
static primitive_type Cyc_91default_91exception_91handler_primitive =
    { {0}, primitive_tag, "Cyc_default_exception_handler",
&_Cyc_91default_91exception_91handler
};
static primitive_type cons_primitive = { {0}, primitive_tag, "cons", &_cons };
static primitive_type cell_91get_primitive =
    { {0}, primitive_tag, "cell-get", &_cell_91get };
static primitive_type set_91global_67_primitive =
    { {0}, primitive_tag, "set-global!", &_set_91global_67 };
static primitive_type set_91cell_67_primitive =
    { {0}, primitive_tag, "set-cell!", &_set_91cell_67 };
static primitive_type cell_primitive = { {0}, primitive_tag, "cell", &_cell };
static primitive_type eq_127_primitive =
    { {0}, primitive_tag, "eq?", &_eq_127 };
static primitive_type eqv_127_primitive =
    { {0}, primitive_tag, "eqv?", &_eqv_127 };
static primitive_type equal_127_primitive =
    { {0}, primitive_tag, "equal?", &_equal_127 };
static primitive_type assq_primitive = { {0}, primitive_tag, "assq", &_assq };
static primitive_type assv_primitive = { {0}, primitive_tag, "assv", &_assv };
static primitive_type memq_primitive = { {0}, primitive_tag, "memq", &_memq };
static primitive_type memv_primitive = { {0}, primitive_tag, "memv", &_memv };
static primitive_type length_primitive =
    { {0}, primitive_tag, "length", &_length };
static primitive_type bytevector_91length_primitive =
    { {0}, primitive_tag, "bytevector-length", &_bytevector_91length };
static primitive_type vector_91length_primitive =
    { {0}, primitive_tag, "vector-length", &_vector_91length };
static primitive_type set_91car_67_primitive =
    { {0}, primitive_tag, "set-car!", &_set_91car_67 };
static primitive_type set_91cdr_67_primitive =
    { {0}, primitive_tag, "set-cdr!", &_set_91cdr_67 };
static primitive_type car_primitive = { {0}, primitive_tag, "car", &_car };
static primitive_type cdr_primitive = { {0}, primitive_tag, "cdr", &_cdr };
static primitive_type caar_primitive = { {0}, primitive_tag, "caar", &_caar };
static primitive_type cadr_primitive = { {0}, primitive_tag, "cadr", &_cadr };
static primitive_type cdar_primitive = { {0}, primitive_tag, "cdar", &_cdar };
static primitive_type cddr_primitive = { {0}, primitive_tag, "cddr", &_cddr };
static primitive_type caaar_primitive =
    { {0}, primitive_tag, "caaar", &_caaar };
static primitive_type caadr_primitive =
    { {0}, primitive_tag, "caadr", &_caadr };
static primitive_type cadar_primitive =
    { {0}, primitive_tag, "cadar", &_cadar };
static primitive_type caddr_primitive =
    { {0}, primitive_tag, "caddr", &_caddr };
static primitive_type cdaar_primitive =
    { {0}, primitive_tag, "cdaar", &_cdaar };
static primitive_type cdadr_primitive =
    { {0}, primitive_tag, "cdadr", &_cdadr };
static primitive_type cddar_primitive =
    { {0}, primitive_tag, "cddar", &_cddar };
static primitive_type cdddr_primitive =
    { {0}, primitive_tag, "cdddr", &_cdddr };
static primitive_type caaaar_primitive =
    { {0}, primitive_tag, "caaaar", &_caaaar };
static primitive_type caaadr_primitive =
    { {0}, primitive_tag, "caaadr", &_caaadr };
static primitive_type caadar_primitive =
    { {0}, primitive_tag, "caadar", &_caadar };
static primitive_type caaddr_primitive =
    { {0}, primitive_tag, "caaddr", &_caaddr };
static primitive_type cadaar_primitive =
    { {0}, primitive_tag, "cadaar", &_cadaar };
static primitive_type cadadr_primitive =
    { {0}, primitive_tag, "cadadr", &_cadadr };
static primitive_type caddar_primitive =
    { {0}, primitive_tag, "caddar", &_caddar };
static primitive_type cadddr_primitive =
    { {0}, primitive_tag, "cadddr", &_cadddr };
static primitive_type cdaaar_primitive =
    { {0}, primitive_tag, "cdaaar", &_cdaaar };
static primitive_type cdaadr_primitive =
    { {0}, primitive_tag, "cdaadr", &_cdaadr };
static primitive_type cdadar_primitive =
    { {0}, primitive_tag, "cdadar", &_cdadar };
static primitive_type cdaddr_primitive =
    { {0}, primitive_tag, "cdaddr", &_cdaddr };
static primitive_type cddaar_primitive =
    { {0}, primitive_tag, "cddaar", &_cddaar };
static primitive_type cddadr_primitive =
    { {0}, primitive_tag, "cddadr", &_cddadr };
static primitive_type cdddar_primitive =
    { {0}, primitive_tag, "cdddar", &_cdddar };
static primitive_type cddddr_primitive =
    { {0}, primitive_tag, "cddddr", &_cddddr };
static primitive_type char_91_125integer_primitive =
    { {0}, primitive_tag, "char->integer", &_char_91_125integer };
static primitive_type integer_91_125char_primitive =
    { {0}, primitive_tag, "integer->char", &_integer_91_125char };
static primitive_type string_91_125number_primitive =
    { {0}, primitive_tag, "string->number", &_string_91_125number };
static primitive_type string_91length_primitive =
    { {0}, primitive_tag, "string-length", &_string_91length };
static primitive_type substring_primitive =
    { {0}, primitive_tag, "substring", &_cyc_substring };
static primitive_type string_91ref_primitive =
    { {0}, primitive_tag, "string-ref", &_cyc_string_91ref };
static primitive_type string_91set_67_primitive =
    { {0}, primitive_tag, "string-set!", &_cyc_string_91set_67 };
static primitive_type Cyc_91installation_91dir_primitive =
    { {0}, primitive_tag, "Cyc-installation-dir", &_Cyc_91installation_91dir };
static primitive_type Cyc_91compilation_91environment_primitive =
    { {0}, primitive_tag, "Cyc-compilation-environment", &_Cyc_91compilation_91environment };
static primitive_type command_91line_91arguments_primitive =
    { {0}, primitive_tag, "command-line-arguments",
&_command_91line_91arguments
};
static primitive_type system_primitive =
    { {0}, primitive_tag, "system", &_cyc_system };
static primitive_type string_91cmp_primitive =
    { {0}, primitive_tag, "string-cmp", &_string_91cmp };
static primitive_type string_91append_primitive =
    { {0}, primitive_tag, "string-append", &_string_91append };
static primitive_type list_91_125string_primitive =
    { {0}, primitive_tag, "list->string", &_list_91_125string };
static primitive_type string_91_125symbol_primitive =
    { {0}, primitive_tag, "string->symbol", &_string_91_125symbol };
static primitive_type symbol_91_125string_primitive =
    { {0}, primitive_tag, "symbol->string", &_symbol_91_125string };
static primitive_type number_91_125string_primitive =
    { {0}, primitive_tag, "number->string", &_number_91_125string };
static primitive_type list_91_125vector_primitive =
    { {0}, primitive_tag, "list-vector", &_list_91_125vector };
static primitive_type make_91bytevector_primitive =
    { {0}, primitive_tag, "make-bytevector", &_make_91bytevector };

static primitive_type bytevector_primitive =
    { {0}, primitive_tag, "bytevector", &_bytevector };
static primitive_type bytevector_91append_primitive =
    { {0}, primitive_tag, "bytevector-append", &_bytevector_91append };
static primitive_type Cyc_91bytevector_91copy_primitive =
    { {0}, primitive_tag, "Cyc-bytevector-copy", &_Cyc_91bytevector_91copy };
static primitive_type bytevector_91u8_91ref_primitive =
    { {0}, primitive_tag, "bytevector-u8-ref", &_bytevector_91u8_91ref };
static primitive_type bytevector_91u8_91set_67_primitive =
    { {0}, primitive_tag, "bytevector-u8-set!", &_bytevector_91u8_91set_67 };
static primitive_type Cyc_91string_91_125utf8_primitive =
    { {0}, primitive_tag, "Cyc-string->utf8", &_Cyc_91string_91_125utf8 };
static primitive_type Cyc_91utf8_91_125string_primitive =
    { {0}, primitive_tag, "Cyc-utf8->string", &_Cyc_91utf8_91_125string };
static primitive_type make_91vector_primitive =
    { {0}, primitive_tag, "make-vector", &_make_91vector };
static primitive_type vector_91ref_primitive =
    { {0}, primitive_tag, "vector-ref", &_vector_91ref };
static primitive_type vector_91set_67_primitive =
    { {0}, primitive_tag, "vector-set!", &_vector_91set_67 };
static primitive_type boolean_127_primitive =
    { {0}, primitive_tag, "boolean?", &_boolean_127 };
static primitive_type char_127_primitive =
    { {0}, primitive_tag, "char?", &_char_127 };
static primitive_type eof_91object_127_primitive =
    { {0}, primitive_tag, "eof-object?", &_eof_91object_127 };
static primitive_type null_127_primitive =
    { {0}, primitive_tag, "null?", &_null_127 };
static primitive_type number_127_primitive =
    { {0}, primitive_tag, "number?", &_number_127 };
static primitive_type real_127_primitive =
    { {0}, primitive_tag, "real?", &_real_127 };
static primitive_type integer_127_primitive =
    { {0}, primitive_tag, "integer?", &_integer_127 };
static primitive_type pair_127_primitive =
    { {0}, primitive_tag, "pair?", &_pair_127 };
static primitive_type procedure_127_primitive =
    { {0}, primitive_tag, "procedure?", &_procedure_127 };
static primitive_type macro_127_primitive =
    { {0}, primitive_tag, "macro?", &_macro_127 };
static primitive_type Cyc_91macro_127_primitive =
    { {0}, primitive_tag, "Cyc-macro?", &_Cyc_91macro_127 };
static primitive_type port_127_primitive =
    { {0}, primitive_tag, "port?", &_port_127 };
static primitive_type bytevector_127_primitive =
    { {0}, primitive_tag, "bytevector?", &_bytevector_127 };
static primitive_type vector_127_primitive =
    { {0}, primitive_tag, "vector?", &_vector_127 };
static primitive_type string_127_primitive =
    { {0}, primitive_tag, "string?", &_string_127 };
static primitive_type symbol_127_primitive =
    { {0}, primitive_tag, "symbol?", &_symbol_127 };
static primitive_type open_91input_91file_primitive =
    { {0}, primitive_tag, "open-input-file", &_open_91input_91file };
static primitive_type open_91output_91file_primitive =
    { {0}, primitive_tag, "open-output-file", &_open_91output_91file };
static primitive_type open_91binary_91input_91file_primitive =
    { {0}, primitive_tag, "open-binary-input-file", &_open_91binary_91input_91file };
static primitive_type open_91binary_91output_91file_primitive =
    { {0}, primitive_tag, "open-binary-output-file", &_open_91binary_91output_91file };
static primitive_type close_91port_primitive =
    { {0}, primitive_tag, "close-port", &_close_91port };
static primitive_type close_91input_91port_primitive =
    { {0}, primitive_tag, "close-input-port", &_close_91input_91port };
static primitive_type close_91output_91port_primitive =
    { {0}, primitive_tag, "close-output-port", &_close_91output_91port };
static primitive_type Cyc_91flush_91output_91port_primitive =
    { {0}, primitive_tag, "Cyc-flush-output-port",
&_Cyc_91flush_91output_91port
};
static primitive_type file_91exists_127_primitive =
    { {0}, primitive_tag, "file-exists?", &_file_91exists_127 };
static primitive_type delete_91file_primitive =
    { {0}, primitive_tag, "delete-file", &_delete_91file };
static primitive_type read_91char_primitive =
    { {0}, primitive_tag, "read-char", &_read_91char };
static primitive_type peek_91char_primitive =
    { {0}, primitive_tag, "peek-char", &_peek_91char };
static primitive_type Cyc_91read_91line_primitive =
    { {0}, primitive_tag, "Cyc-read-line", &_Cyc_91read_91line };
static primitive_type Cyc_91write_primitive =
    { {0}, primitive_tag, "Cyc-write", &_Cyc_91write };
static primitive_type Cyc_91write_91char_primitive =
    { {0}, primitive_tag, "Cyc-write-char", &_Cyc_91write_91char };
static primitive_type Cyc_91display_primitive =
    { {0}, primitive_tag, "Cyc-display", &_display };
static primitive_type call_95cc_primitive =
    { {0}, primitive_tag, "call/cc", &_call_95cc };

const object primitive_Cyc_91global_91vars = &Cyc_91global_91vars_primitive;
const object primitive_Cyc_91get_91cvar = &Cyc_91get_91cvar_primitive;
const object primitive_Cyc_91set_91cvar_67 = &Cyc_91set_91cvar_67_primitive;
const object primitive_Cyc_91cvar_127 = &Cyc_91cvar_127_primitive;
const object primitive_Cyc_91opaque_127 = &Cyc_91opaque_127_primitive;
const object primitive_Cyc_91has_91cycle_127 = &Cyc_91has_91cycle_127_primitive;
const object primitive_Cyc_91spawn_91thread_67 =
    &Cyc_91spawn_91thread_67_primitive;
const object primitive_Cyc_91end_91thread_67 = &Cyc_91end_91thread_67_primitive;
const object primitive__87 = &_87_primitive;
const object primitive__91 = &_91_primitive;
const object primitive__85 = &_85_primitive;
const object primitive__95 = &_95_primitive;
const object primitive__123 = &_123_primitive;
const object primitive__125 = &_125_primitive;
const object primitive__121 = &_121_primitive;
const object primitive__125_123 = &_125_123_primitive;
const object primitive__121_123 = &_121_123_primitive;
const object primitive_apply = &apply_primitive;
const object primitive__75halt = &_75halt_primitive;
const object primitive_exit = &exit_primitive;
const object primitive_Cyc_91current_91exception_91handler =
    &Cyc_91current_91exception_91handler_primitive;
const object primitive_Cyc_91default_91exception_91handler =
    &Cyc_91default_91exception_91handler_primitive;
const object primitive_cons = &cons_primitive;
const object primitive_cell_91get = &cell_91get_primitive;
const object primitive_set_91global_67 = &set_91global_67_primitive;
const object primitive_set_91cell_67 = &set_91cell_67_primitive;
const object primitive_cell = &cell_primitive;
const object primitive_eq_127 = &eq_127_primitive;
const object primitive_eqv_127 = &eqv_127_primitive;
const object primitive_equal_127 = &equal_127_primitive;
const object primitive_assq = &assq_primitive;
const object primitive_assv = &assv_primitive;
const object primitive_memq = &memq_primitive;
const object primitive_memv = &memv_primitive;
const object primitive_length = &length_primitive;
const object primitive_bytevector_91length = &bytevector_91length_primitive;
const object primitive_vector_91length = &vector_91length_primitive;
const object primitive_vector_91ref = &vector_91ref_primitive;
const object primitive_vector_91set_67 = &vector_91set_67_primitive;
const object primitive_set_91car_67 = &set_91car_67_primitive;
const object primitive_set_91cdr_67 = &set_91cdr_67_primitive;
const object primitive_car = &car_primitive;
const object primitive_cdr = &cdr_primitive;
const object primitive_caar = &caar_primitive;
const object primitive_cadr = &cadr_primitive;
const object primitive_cdar = &cdar_primitive;
const object primitive_cddr = &cddr_primitive;
const object primitive_caaar = &caaar_primitive;
const object primitive_caadr = &caadr_primitive;
const object primitive_cadar = &cadar_primitive;
const object primitive_caddr = &caddr_primitive;
const object primitive_cdaar = &cdaar_primitive;
const object primitive_cdadr = &cdadr_primitive;
const object primitive_cddar = &cddar_primitive;
const object primitive_cdddr = &cdddr_primitive;
const object primitive_caaaar = &caaaar_primitive;
const object primitive_caaadr = &caaadr_primitive;
const object primitive_caadar = &caadar_primitive;
const object primitive_caaddr = &caaddr_primitive;
const object primitive_cadaar = &cadaar_primitive;
const object primitive_cadadr = &cadadr_primitive;
const object primitive_caddar = &caddar_primitive;
const object primitive_cadddr = &cadddr_primitive;
const object primitive_cdaaar = &cdaaar_primitive;
const object primitive_cdaadr = &cdaadr_primitive;
const object primitive_cdadar = &cdadar_primitive;
const object primitive_cdaddr = &cdaddr_primitive;
const object primitive_cddaar = &cddaar_primitive;
const object primitive_cddadr = &cddadr_primitive;
const object primitive_cdddar = &cdddar_primitive;
const object primitive_cddddr = &cddddr_primitive;
const object primitive_char_91_125integer = &char_91_125integer_primitive;
const object primitive_integer_91_125char = &integer_91_125char_primitive;
const object primitive_string_91_125number = &string_91_125number_primitive;
const object primitive_string_91length = &string_91length_primitive;
const object primitive_substring = &substring_primitive;
const object primitive_string_91ref = &string_91ref_primitive;
const object primitive_string_91set_67 = &string_91set_67_primitive;
const object primitive_Cyc_91installation_91dir =
    &Cyc_91installation_91dir_primitive;
const object primitive_Cyc_91compilation_91environment =
    &Cyc_91compilation_91environment_primitive;
const object primitive_command_91line_91arguments =
    &command_91line_91arguments_primitive;
const object primitive_system = &system_primitive;
const object primitive_string_91cmp = &string_91cmp_primitive;
const object primitive_string_91append = &string_91append_primitive;
const object primitive_list_91_125string = &list_91_125string_primitive;
const object primitive_string_91_125symbol = &string_91_125symbol_primitive;
const object primitive_symbol_91_125string = &symbol_91_125string_primitive;
const object primitive_number_91_125string = &number_91_125string_primitive;
const object primitive_make_91bytevector = &make_91bytevector_primitive;
const object primitive_make_91vector = &make_91vector_primitive;
const object primitive_bytevector = &bytevector_primitive;
const object primitive_bytevector_91append = &bytevector_91append_primitive;
const object primitive_Cyc_91bytevector_91copy =
    &Cyc_91bytevector_91copy_primitive;
const object primitive_bytevector_91u8_91ref = &bytevector_91u8_91ref_primitive;
const object primitive_bytevector_91u8_91set_67 =
    &bytevector_91u8_91set_67_primitive;
const object primitive_Cyc_91string_91_125utf8 =
    &Cyc_91string_91_125utf8_primitive;
const object primitive_Cyc_91utf8_91_125string =
    &Cyc_91utf8_91_125string_primitive;
const object primitive_list_91_125vector = &list_91_125vector_primitive;
const object primitive_boolean_127 = &boolean_127_primitive;
const object primitive_char_127 = &char_127_primitive;
const object primitive_eof_91object_127 = &eof_91object_127_primitive;
const object primitive_null_127 = &null_127_primitive;
const object primitive_number_127 = &number_127_primitive;
const object primitive_real_127 = &real_127_primitive;
const object primitive_integer_127 = &integer_127_primitive;
const object primitive_pair_127 = &pair_127_primitive;
const object primitive_procedure_127 = &procedure_127_primitive;
const object primitive_macro_127 = &macro_127_primitive;
const object primitive_Cyc_91macro_127 = &Cyc_91macro_127_primitive;
const object primitive_string_127 = &string_127_primitive;
const object primitive_port_127 = &port_127_primitive;
const object primitive_vector_127 = &vector_127_primitive;
const object primitive_bytevector_127 = &bytevector_127_primitive;
const object primitive_symbol_127 = &symbol_127_primitive;
const object primitive_open_91input_91file = &open_91input_91file_primitive;
const object primitive_open_91output_91file = &open_91output_91file_primitive;
const object primitive_open_91binary_91input_91file = &open_91binary_91input_91file_primitive;
const object primitive_open_91binary_91output_91file = &open_91binary_91output_91file_primitive;
const object primitive_close_91port = &close_91port_primitive;
const object primitive_close_91input_91port = &close_91input_91port_primitive;
const object primitive_close_91output_91port = &close_91output_91port_primitive;
const object primitive_Cyc_91flush_91output_91port =
    &Cyc_91flush_91output_91port_primitive;
const object primitive_file_91exists_127 = &file_91exists_127_primitive;
const object primitive_delete_91file = &delete_91file_primitive;
const object primitive_read_91char = &read_91char_primitive;
const object primitive_peek_91char = &peek_91char_primitive;
const object primitive_Cyc_91read_91line = &Cyc_91read_91line_primitive;
const object primitive_Cyc_91write_91char = &Cyc_91write_91char_primitive;
const object primitive_Cyc_91write = &Cyc_91write_primitive;
const object primitive_Cyc_91display = &Cyc_91display_primitive;
const object primitive_call_95cc = &call_95cc_primitive;

void *gc_alloc_pair(gc_thread_data *data, object head, object tail)
{
  int heap_grown;
  pair_type *p;
  pair_type tmp;
  tmp.hdr.mark = gc_color_red;
  tmp.hdr.grayed = 0;
  tmp.hdr.immutable = 0;
  tmp.tag = pair_tag;
  tmp.pair_car = head;
  tmp.pair_cdr = tail;
  p = gc_alloc(((gc_thread_data *)data)->heap, sizeof(pair_type), (char *)(&tmp), (gc_thread_data *)data, &heap_grown);

  return p;
}

/**
 * Thread initialization function only called from within the runtime
 */
void *Cyc_init_thread(object thread_and_thunk, int argc, object *args)
{
  int i;
  vector_type *t;
  c_opaque_type *o;
  object op, parent, child, tmp;
  long stack_start;
  gc_thread_data *thd;

  // Extract passed-in thread data object
  tmp = car(thread_and_thunk); 
  t = (vector_type *)tmp;
  op = _unsafe_Cyc_vector_ref(t, obj_int2obj(2)); // Field set in thread-start!
  if (op == NULL) {
    // Should never happen
    thd = malloc(sizeof(gc_thread_data));
  } else {
    o = (c_opaque_type *)op;
    thd = (gc_thread_data *)(opaque_ptr(o));
  }
  gc_thread_data_init(thd, 0, (char *)&stack_start, global_stack_size);
  thd->scm_thread_obj = car(thread_and_thunk);
  thd->gc_cont = cdr(thread_and_thunk);
  thd->gc_num_args = 1;
  if (t->num_elements >= 7 && t->elements[6] != boolean_f) {
    thd->gc_args[0] = t->elements[6];
  } else {
    thd->gc_args[0] = &Cyc_91end_91thread_67_primitive;
  }

  if (argc > 0) {
    thd->gc_num_args = argc + 1;
    for (i = 0; i < argc; i++) {
      thd->gc_args[i + 1] = args[i];
    }
  }
  thd->thread_id = pthread_self();

  // Copy thread params from the calling thread
  t = (vector_type *)thd->scm_thread_obj;
  op = Cyc_vector_ref(thd, t, obj_int2obj(5)); // Field set in thread-start!
  o = (c_opaque_type *)op;
  parent = ((gc_thread_data *)o->ptr)->param_objs; // Unbox parent thread's data
  child = NULL;
  thd->param_objs = NULL;
  while (parent) {
    if (thd->param_objs == NULL) {
      thd->param_objs = gc_alloc_pair(thd, NULL, NULL);
      child = thd->param_objs;
    } else {
      pair_type *p = gc_alloc_pair(thd, NULL, NULL);
      cdr(child) = p;
      child = p;
    }
    car(child) = gc_alloc_pair(thd, car(car(parent)), cdr(car(parent)));
    parent = cdr(parent);
  }
  // Done initializing parameter objects

  gc_add_mutator(thd);
  ck_pr_cas_int((int *)&(thd->thread_state), CYC_THREAD_STATE_NEW,
                CYC_THREAD_STATE_RUNNABLE);
  Cyc_start_trampoline(thd);
  return NULL;
}

void *_Cyc_init_thread(object thread_and_thunk)
{
  return Cyc_init_thread(thread_and_thunk, 0, NULL);
}

/**
 * Spawn a new thread to execute the given thunk
 */
object Cyc_spawn_thread(object thread_and_thunk)
{
// TODO: if we want to return mutator number to the caller, we need
// to reserve a number here. need to figure out how we are going to
// synchronize access to GC mutator fields, and then reserve one
// here. will need to pass it, along with thunk, to Cyc_init_thread.
// Then can use a new function up there to add the mutator, since we
// already have the number.
/*
how to manage gc mutators. need to handle:
- need to be able to allocate a thread but not run it yet.
  maybe have a run level, or status
- need to make mutators thread safe, ideally without major performance impacts
- thread terminates
  - should mark mutator as 'done'
  - at an opportune moment, free mutator and set it back
    to null

what is the right data structure? is the array OK? or would it be better
to look at the lock-free structures provided by ck?
*/
  pthread_t thread;
  pthread_attr_t attr;
  pthread_attr_init(&attr);
  pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_DETACHED);
  if (pthread_create(&thread, &attr, _Cyc_init_thread, thread_and_thunk)) {
    fprintf(stderr, "Error creating a new thread\n");
    exit(1);
  }
  pthread_attr_destroy(&attr);
  return boolean_t;
}

/**
 * Terminate a thread
 */
void Cyc_end_thread(gc_thread_data * thd)
{
  // TODO: should we consider passing the current continuation (and args)
  // as an argument? if we don't, will objects be collected that are still
  // being used by active threads??
  mclosure0(clo, Cyc_exit_thread);
  GC(thd, &clo, thd->gc_args, 0);
}

void Cyc_exit_thread(gc_thread_data * thd)
{
  // alternatively could call longjmp with a null continuation, but that seems
  // more complicated than necessary. or does it... see next comment:

  // TODO: what if there are any locals from the thread's stack still being
  // referenced? might want to do one more minor GC to clear the stack before
  // terminating the thread

//printf("DEBUG - exiting thread\n");
  // Remove thread from the list of mutators, and mark its data to be freed
  gc_remove_mutator(thd);
  ck_pr_cas_int((int *)&(thd->thread_state), CYC_THREAD_STATE_RUNNABLE,
                CYC_THREAD_STATE_TERMINATED);
  pthread_exit(NULL);           // For now, just a proof of concept
}

/**
 * @brief Accept a number of seconds to sleep according to SRFI-18
 */
object Cyc_thread_sleep(void *data, object timeout)
{
  struct timespec tim;
  double value;
  Cyc_check_num(data, timeout);
  value = unbox_number(timeout);
  tim.tv_sec = (long)value;
  tim.tv_nsec = (long)((value - tim.tv_sec) * 1000 * NANOSECONDS_PER_MILLISECOND);
  nanosleep(&tim, NULL);
  return boolean_t;
}

/**
 * @brief Copy given object to the heap, if it is from the stack.
 *        This function is intended to be called directly from application code.
 *        Note that only a shallow-copy is performed! For example, a pair object
 *        would be copied to the heap but its `car` and `cdr` objects would not.
 * @param data Thread data object for the caller.
 * @param obj Object to copy.
 */
object copy2heap(void *data, object obj)
{
  char stack_pos;
  gc_thread_data *thd = (gc_thread_data *) data;
  int on_stack = stack_overflow((object) (&stack_pos), obj) &&
      stack_overflow(obj, (object) thd->stack_start);
  if (!is_object_type(obj) || !on_stack) {
    return obj;
  }

  return gc_alloc(((gc_thread_data *)data)->heap, gc_allocated_bytes(obj, NULL, NULL), obj, data,
                  &on_stack);
}

// TODO: version of above that will perform a deep copy (via GC) if necessary

// Generic buffer functions
void **vpbuffer_realloc(void **buf, int *len)
{
  return realloc(buf, (*len) * sizeof(void *));
}

void **vpbuffer_add(void **buf, int *len, int i, void *obj)
{
  if (i == *len) {
    *len *= 2;
    buf = vpbuffer_realloc(buf, len);
  }
  buf[i] = obj;
  return buf;
}

void vpbuffer_free(void **buf)
{
  free(buf);
}

vpbuffer *vp_create(void)
{
  vpbuffer *v = malloc(sizeof(vpbuffer));
  v->len = 128;
  v->count = 0;
  v->buf = NULL;
  v->buf = vpbuffer_realloc(v->buf, &(v->len));
  return v;
}

void vp_add(vpbuffer *v, void *obj)
{
  v->buf = vpbuffer_add(v->buf, &(v->len), v->count++, obj);
}

object Cyc_bit_unset(void *data, object n1, object n2) 
{
  Cyc_check_int(data, n1);
  Cyc_check_int(data, n2);
  return (obj_int2obj( 
            obj_obj2int(n1) & ~(obj_obj2int(n2))));
}

object Cyc_bit_set(void *data, object n1, object n2) 
{
  Cyc_check_int(data, n1);
  Cyc_check_int(data, n2);
  return (obj_int2obj( 
            obj_obj2int(n1) | obj_obj2int(n2)));
}

object Cyc_num2double(void *data, object ptr, object z)
{
 return_inexact_double_op_no_cps(data, ptr, (double), z);
}

void Cyc_make_rectangular(void *data, object k, object r, object i) 
{
  double_type dr, di;
  Cyc_num2double(data, &dr, r);
  Cyc_num2double(data, &di, i);
  make_complex_num(num, double_value(&dr), double_value(&di));
  return_closcall1(data, k, &num);
}

/* RNG section */
#define norm 2.328306549295728e-10
#define m1   4294967087.0
#define m2   4294944443.0
#define a12     1403580.0
#define a13n     810728.0
#define a21      527612.0
#define a23n    1370589.0

/***
The seeds for s10, s11, s12 must be integers in [0, m1 - 1] and not all 0. 
The seeds for s20, s21, s22 must be integers in [0, m2 - 1] and not all 0. 
***/

//#define SEED 12345

// JAE TODO: OK not to have these static?
//static double s10 = SEED, s11 = SEED, s12 = SEED,
//              s20 = SEED, s21 = SEED, s22 = SEED;


double MRG32k3a (double seed)
{
   double s10 = seed, s11 = seed, s12 = seed,
          s20 = seed, s21 = seed, s22 = seed;
   long k;
   double p1, p2;
   /* Component 1 */
   p1 = a12 * s11 - a13n * s10;
   k = p1 / m1;
   p1 -= k * m1;
   if (p1 < 0.0)
      p1 += m1;
   s10 = s11;
   s11 = s12;
   s12 = p1;

   /* Component 2 */
   p2 = a21 * s22 - a23n * s20;
   k = p2 / m2;
   p2 -= k * m2;
   if (p2 < 0.0)
      p2 += m2;
   s20 = s21;
   s21 = s22;
   s22 = p2;

   /* Combination */
   if (p1 <= p2)
      return ((p1 - p2 + m1) * norm);
   else
      return ((p1 - p2) * norm);
}
/* END RNG */


/** Dynamic loading */
void Cyc_import_shared_object(void *data, object cont, object filename, object entry_pt_fnc)
{
  char buffer[256];
  void *handle;
  function_type entry_pt;
  Cyc_check_str(data, filename);
  Cyc_check_str(data, entry_pt_fnc);
  handle = dlopen(string_str(filename), RTLD_GLOBAL | RTLD_LAZY);
  if (handle == NULL) {
    snprintf(buffer, 256, "%s", dlerror());
    make_utf8_string(data, s, buffer);
    Cyc_rt_raise2(data, "Unable to import library", &s);
  }
  dlerror();    /* Clear any existing error */

  entry_pt = (function_type) dlsym(handle, string_str(entry_pt_fnc));
  if (entry_pt == NULL) {
    snprintf(buffer, 256, "%s, %s, %s", string_str(filename), string_str(entry_pt_fnc), dlerror());
    make_utf8_string(data, s, buffer);
    Cyc_rt_raise2(data, "Unable to load symbol", &s);
  }
  mclosure1(clo, entry_pt, cont);
  entry_pt(data, 0, &clo, &clo);
}

/** Read */

/**
 * @brief Helper function to perform a buffered read from an input port
 * @param p Input port
 * @return Number of characters read, or 0 for EOF/error
 */
int read_from_port(port_type *p)
{
  size_t rv = 0;
  FILE *fp = p->fp;
  char *buf = p->mem_buf;

  while(1) {
    errno = 0;
    rv = fread(buf, sizeof(char), p->read_len, fp);

    if (rv != 0 || !ferror(fp) || errno != EINTR) {
      break;
    }
  }

  p->mem_buf_len = rv;
  p->buf_idx = 0;
  return rv;
}

/**
 * @brief Helper function to raise an error from (read)
 * @param data Thread data object
 * @param p Input port
 * @param msg Error message
 */
static void _read_error(void *data, port_type *p, const char *msg) 
{
  char buf[1024];
  snprintf(buf, 1023, "(line %d, column %d): %s", 
           p->line_num, p->col_num, msg);
  // TODO: can't do this because thread is blocked, need to return a value to cont.
  // the cont could receive an error and raise it though
  //Cyc_rt_raise_msg(data, buf);
  make_string(str, buf);
  str.num_cp = Cyc_utf8_count_code_points((uint8_t *)buf);
  make_empty_vector(vec);
  vec.num_elements = 1;
  vec.elements = (object *) alloca(sizeof(object) * vec.num_elements);
  vec.elements[0] = &str;
  return_thread_runnable_with_obj(data, &vec, p);
}

/**
 * @brief Helper function to read past a comment
 * @param p Input port
 */
static void _read_line_comment(port_type *p)
{
  while(1) {
    // Read more data into buffer, if needed
    if (p->buf_idx == p->mem_buf_len) {
      if (!read_from_port(p)){
        break; // Return if buf is empty
      }
    }
    if (p->mem_buf[p->buf_idx++] == '\n') {
      p->line_num++; // Ignore col_num since we are just skipping over chars
      p->col_num = 1;
      break;
    }
  }
}

/**
 * @brief Helper function to read past a block comment
 * @param p Input port
 */
static void _read_multiline_comment(port_type *p)
{
  int maybe_end = 0;

  while(1) {
    // Read more data into buffer, if needed
    if (p->buf_idx == p->mem_buf_len) {
      if (!read_from_port(p)){
        break; // Return if buf is empty
      }
    }

    if (p->mem_buf[p->buf_idx] == '#' && maybe_end) {
      p->buf_idx++;
      break;
    }

    if (p->mem_buf[p->buf_idx] == '|') {
      maybe_end = 1;
    } else {
      maybe_end = 0;
    }

    if (p->mem_buf[p->buf_idx] == '\n') {
      p->line_num++;
      p->col_num = 1;
    } else {
      p->col_num++;
    }
    p->buf_idx++;
  }
}

/**
 * @brief Helper function to read past whitespace characters
 * @param p Input port
 */
static void _read_whitespace(port_type *p) 
{
  while(1) {
    // Read more data into buffer, if needed
    if (p->buf_idx == p->mem_buf_len) {
      if (!read_from_port(p)){
        break; // Return if buf is empty
      }
    }
    if (p->mem_buf[p->buf_idx] == '\n') {
      p->buf_idx++;
      p->line_num++; // Ignore col_num since we are just skipping over chars
      p->col_num = 1;
      break;
    } else if (isspace(p->mem_buf[p->buf_idx])) {
      p->buf_idx++;
      p->col_num++;
    } else {
      break; // Terminate on non-whitespace char
    }
  }
}

/**
 * @brief Helper function to add a character to the port's token buffer
 * @param p Input port
 * @param c Character to add
 */
static void _read_add_to_tok_buf(port_type *p, char c)
{
  // FUTURE: more efficient to try and use mem_buf directly??
  //         complicates things with more edge cases though
  if ((p->tok_end + 1) == p->tok_buf_len) { // +1 for trailing \0 later on
    p->tok_buf_len *= 2;
    p->tok_buf = realloc(p->tok_buf, p->tok_buf_len);
    if (!p->tok_buf) {
      fprintf(stderr, "Unable to grow token buffer!\n");
      exit(1);
    }
  }
  p->tok_buf[p->tok_end++] = c;
}

/**
 * @brief Determine if given string is numeric
 */
static int _read_is_numeric(const char *tok, int len)
{
  return (len &&
          ((isdigit(tok[0])) ||
           ((len > 1) && tok[0] == '.' && isdigit(tok[1])) ||
           ((len > 1) && (tok[1] == '.' || isdigit(tok[1])) && (tok[0] == '-' || tok[0] == '+'))));
}

/**
 * @brief Determine if given string is a complex number
 */
static int _read_is_complex_number(const char *tok, int len)
{
  // Assumption: tok already passed checks from _read_is_numeric
  return (tok[len - 1] == 'i' ||
          tok[len - 1] == 'I');
}

/**
 * @brief Helper function, determine if given number is a hex digit
 * @param c Character to check
 */
static int _read_is_hex_digit(char c)
{
  return (c >= 'a' && c <= 'f') ||
         (c >= 'A' && c <= 'F');
}

/**
 * @brief Helper function to read a string
 * @param data Thread data object
 * @param cont Current continuation
 * @param p Input port
 */
static void _read_string(void *data, object cont, port_type *p) 
{
  char c;
  int escaped = 0, escaped_whitespace = 0, 
      ewrn = 0; // esc whitespace read newline
  while(1) {
    // Read more data into buffer, if needed
    if (p->buf_idx == p->mem_buf_len) {
      if (!read_from_port(p)){
        _read_error(data, p, "Missing closing double-quote");
      }
    }
    c = p->mem_buf[p->buf_idx++];
    p->col_num++;

    if (escaped_whitespace) {
      switch (c) {
      case '\r':
      case '\t':
      case ' ':
        p->col_num++;
        continue;
        break;
      case '\n':
        if (ewrn == 0) {
          ewrn = 1;
          p->line_num++;
          p->col_num = 1;
          continue;
        }
        break;
      default:
        escaped_whitespace = 0;
        ewrn = 0;
        break;
      }
    }

    if (escaped) {
      escaped = 0;
      switch (c) {
      case '"':
      case '\'':
      case '?':
      case '|':
      case '\\':
        _read_add_to_tok_buf(p, c);
        break;
      case 'a':
        _read_add_to_tok_buf(p, '\a');
        break;
      case 'b':
        _read_add_to_tok_buf(p, '\b');
        break;
      case 'n':
        _read_add_to_tok_buf(p, '\n');
        break;
      case 'r':
        _read_add_to_tok_buf(p, '\r');
        break;
      case 't':
        _read_add_to_tok_buf(p, '\t');
        break;
      case 'x': {
        char buf[32];
        int i = 0;
        while (i < 31){
          if (p->mem_buf_len == 0 || p->mem_buf_len == p->buf_idx) { 
            int rv = read_from_port(p); 
            if (!rv) { 
              break;
            }
          }
          if (p->mem_buf[p->buf_idx] == ';'){
            p->buf_idx++;
            break;
          }
          // Verify if hex digit is valid
          if (!isdigit(p->mem_buf[p->buf_idx]) && 
              !_read_is_hex_digit(p->mem_buf[p->buf_idx])) {
            p->buf_idx++;
            _read_error(data, p, "invalid hex digit in string");
          }
          buf[i] = p->mem_buf[p->buf_idx];
          p->buf_idx++;
          p->col_num++;
          i++;
        }
        buf[i] = '\0';
        {
          char_type result = strtol(buf, NULL, 16);
          char cbuf[5];
          int i;
          Cyc_utf8_encode_char(cbuf, 5, result);
          for (i = 0; cbuf[i] != 0; i++) {
            _read_add_to_tok_buf(p, cbuf[i]);
          }
          //p->tok_buf[p->tok_end++] = (char)result;
        }
        break;
      }
      case '\r':
      case '\t':
      case ' ':
        escaped_whitespace = 1;
        ewrn = 0;
        p->col_num++;
        break;
      case '\n':
        escaped_whitespace = 1;
        ewrn = 1;
        p->line_num++;
        p->col_num = 1;
        break;
      default:
        _read_error(data, p, "invalid escape character in string"); // TODO: char
        break;
      }
    } else if (c == '"') {
      p->tok_buf[p->tok_end] = '\0'; // TODO: what if buffer is full?
      p->tok_end = 0; // Reset for next atom
      {
        make_utf8_string(data, str, p->tok_buf);
        return_thread_runnable_with_obj(data, &str, p);
      }
    } else if (c == '\\') {
      escaped = 1;
    } else if (c == '\n') {
      p->line_num++;
      p->col_num = 1;
      _read_add_to_tok_buf(p, c);
    } else {
      _read_add_to_tok_buf(p, c);
    }
  }
}

/**
 * @brief Helper function to read a literal identifier
 * @param data Thread data object
 * @param p Input port
 */
static void _read_literal_identifier(void *data, port_type *p) 
{
  char c;
  int escaped = 0;
  while(1) {
    // Read more data into buffer, if needed
    if (p->buf_idx == p->mem_buf_len) {
      if (!read_from_port(p)){
        _read_error(data, p, "EOF encountered parsing literal identifier");
      }
    }
    c = p->mem_buf[p->buf_idx++];
    p->col_num++;

    if (escaped) {
      escaped = 0;
      switch (c) {
      case '"':
      case '\'':
      case '?':
      case '|':
      case '\\':
        _read_add_to_tok_buf(p, c);
        break;
      case 'a':
        _read_add_to_tok_buf(p, '\a');
        break;
      case 'b':
        _read_add_to_tok_buf(p, '\b');
        break;
      case 'n':
        _read_add_to_tok_buf(p, '\n');
        break;
      case 'r':
        _read_add_to_tok_buf(p, '\r');
        break;
      case 't':
        _read_add_to_tok_buf(p, '\t');
        break;
      case 'x': {
        char buf[32];
        int i = 0;
        while (i < 31){
          if (p->mem_buf_len == 0 || p->mem_buf_len == p->buf_idx) { 
            int rv = read_from_port(p); 
            if (!rv) { 
              break;
            }
          }
          if (p->mem_buf[p->buf_idx] == ';'){
            p->buf_idx++;
            break;
          }
          // Verify if hex digit is valid
          if (!isdigit(p->mem_buf[p->buf_idx]) && 
              !_read_is_hex_digit(p->mem_buf[p->buf_idx])) {
            p->buf_idx++;
            _read_error(data, p, "invalid hex digit in literal identifier");
          }
          buf[i] = p->mem_buf[p->buf_idx];
          p->buf_idx++;
          p->col_num++;
          i++;
        }
        buf[i] = '\0';
        {
          char_type result = strtol(buf, NULL, 16);
          char cbuf[5];
          int i;
          Cyc_utf8_encode_char(cbuf, 5, result);
          for (i = 0; cbuf[i] != 0; i++) {
            _read_add_to_tok_buf(p, cbuf[i]);
          }
          //p->tok_buf[p->tok_end++] = (char)result;
        }
        break;
      }
      default:
        _read_error(data, p, "invalid escape character in literal identifier"); // TODO: char
        break;
      }
    } else if (c == '|') {
      p->tok_buf[p->tok_end] = '\0'; // TODO: what if buffer is full?
      p->tok_end = 0; // Reset for next atom
      {
        object sym = find_or_add_symbol(p->tok_buf);
        return_thread_runnable_with_obj(data, sym, p);
      }
    } else if (c == '\\') {
      escaped = 1;
    } else if (c == '\n') {
      p->line_num++;
      p->col_num = 1;
      _read_add_to_tok_buf(p, c);
    } else {
      _read_add_to_tok_buf(p, c);
    }
  }
}

/**
 * @brief Helper function to read a character token
 * @param data Thread data object
 * @param p Input port
 */
static void _read_return_character(void *data, port_type *p)
{
  p->tok_buf[p->tok_end] = '\0'; // TODO: what if buffer is full?
  p->tok_end = 0; // Reset for next atom
  if (strlen(p->tok_buf) == 1) {
    // ASCII char, consider merging with below?
    return_thread_runnable_with_obj(data, obj_char2obj(p->tok_buf[0]), p);
  } else if(strncmp(p->tok_buf, "alarm", 5) == 0) {
    return_thread_runnable_with_obj(data, obj_char2obj('\a'), p);
  } else if(strncmp(p->tok_buf, "backspace", 9) == 0) {
    return_thread_runnable_with_obj(data, obj_char2obj('\b'), p);
  } else if(strncmp(p->tok_buf, "delete", 6) == 0) {
    return_thread_runnable_with_obj(data, obj_char2obj(127), p);
  } else if(strncmp(p->tok_buf, "escape", 6) == 0) {
    return_thread_runnable_with_obj(data, obj_char2obj(27), p);
  } else if(strncmp(p->tok_buf, "newline", 7) == 0) {
    return_thread_runnable_with_obj(data, obj_char2obj('\n'), p);
  } else if(strncmp(p->tok_buf, "null", 4) == 0) {
    return_thread_runnable_with_obj(data, obj_char2obj('\0'), p);
  } else if(strncmp(p->tok_buf, "return", 6) == 0) {
    return_thread_runnable_with_obj(data, obj_char2obj('\r'), p);
  } else if(strncmp(p->tok_buf, "space", 5) == 0) {
    return_thread_runnable_with_obj(data, obj_char2obj(' '), p);
  } else if(strncmp(p->tok_buf, "tab", 3) == 0) {
    return_thread_runnable_with_obj(data, obj_char2obj('\t'), p);
  } else if(strlen(p->tok_buf) > 1 && p->tok_buf[0] == 'x') {
    const char *buf = p->tok_buf + 1;
    char_type result = strtol(buf, NULL, 16);
    return_thread_runnable_with_obj(data, obj_char2obj(result), p);
  } else {
    // Try to read a UTF-8 char and if so return it, otherwise throw an error
    uint32_t state = CYC_UTF8_ACCEPT;
    char_type codepoint;
    uint8_t *s = (uint8_t *)p->tok_buf;
    while(s) {
      if (!Cyc_utf8_decode(&state, &codepoint, *s)) {
        s++;
        break;
      }
      s++;
    }
    if (state == CYC_UTF8_ACCEPT && *s == '\0') {
      return_thread_runnable_with_obj(data, obj_char2obj(codepoint), p);
    } else {
      char buf[31];
      snprintf(buf, 30, "Unable to parse character %s", p->tok_buf);
      _read_error(data, p, buf);
    }
  }
}

/**
 * @brief Helper function to read a character token
 * @param data Thread data object
 * @param p Input port
 */
static void _read_character(void *data, port_type *p) 
{
  char c;
  while(1) {
    // Read more data into buffer, if needed
    if (p->buf_idx == p->mem_buf_len) {
      if (!read_from_port(p)){
        _read_return_character(data, p);
      }
    }
    c = p->mem_buf[p->buf_idx++];
    p->col_num++;

    if (p->tok_end && (isspace(c) || c == ')')) {
      p->buf_idx--;
      p->col_num--;
      _read_return_character(data, p);
    } else {
      _read_add_to_tok_buf(p, c);
    }
  }
}

/**
 * @brief Helper function, return read number.
 * @param data Thread data object
 * @param p Input port
 * @param base Number base
 * @param exact Return an exact number if true
 */
static void _read_return_number(void *data, port_type *p, int base, int exact)
{
  // TODO: validation?
  p->tok_buf[p->tok_end] = '\0'; // TODO: what if buffer is full?
  p->tok_end = 0; // Reset for next atom
  make_empty_vector(vec);
  make_string(str, p->tok_buf);
  vec.num_elements = 3;
  vec.elements = (object *) alloca(sizeof(object) * vec.num_elements);
  vec.elements[0] = &str;
  vec.elements[1] = obj_int2obj(base);
  vec.elements[2] = exact ? boolean_t : boolean_f;
  return_thread_runnable_with_obj(data, &vec, p);
}

/**
 * @brief Helper function, parse&return read complex number.
 * @param data Thread data object
 * @param p Input port
 * @param base Number base
 * @param exact Return an exact number if true
 */
static void _read_return_complex_number(void *data, port_type *p, int len)
{
//      TODO: return complex num, see _read_return_number for possible template
//      probably want to have that function extract/identify the real/imaginary components.
//      can just scan the buffer and read out start/end index of each number.
  int i;
  make_empty_vector(vec);
  make_string(str, p->tok_buf);
  vec.num_elements = 2;
  vec.elements = (object *) alloca(sizeof(object) * vec.num_elements);
  vec.elements[0] = &str;
  i = 0;
  if (p->tok_buf[0] == '-' || p->tok_buf[0] == '+') {
    i++;
  }
  for (; i < len; i++) {
    if (!isdigit(p->tok_buf[i]) && p->tok_buf[i] != '.' && p->tok_buf[i] != 'e' && p->tok_buf[i] != 'E') {
      break;
    }
  }
  vec.elements[1] = obj_int2obj(i);
  return_thread_runnable_with_obj(data, &vec, p);
}

/**
 * @brief Helper function, read number.
 * @param data Thread data object
 * @param p Input port
 * @param base Number base
 * @param exact Return an exact number if true
 */
static void _read_number(void *data, port_type *p, int base, int exact) 
{
  char c;
  while(1) {
    // Read more data into buffer, if needed
    if (p->buf_idx == p->mem_buf_len) {
      if (!read_from_port(p)){
        _read_return_number(data, p, base, exact);
      }
    }
    c = p->mem_buf[p->buf_idx++];
    p->col_num++;

    if (isdigit(c)) {
      if ((base == 2 && c > '1') ||
          (base == 8 && c > '7')) {
        _read_error(data, p, "Illegal digit");
      }
      _read_add_to_tok_buf(p, c);
    } else if (c == '+' || c == '-' || c == '.') {
      _read_add_to_tok_buf(p, c);
    } else if (base == 16 && _read_is_hex_digit(c)) {
      _read_add_to_tok_buf(p, c);
    } else {
      p->buf_idx--;
      p->col_num--;
      _read_return_number(data, p, base, exact);
    }
  }
}

/**
 * @brief Helper function, return read atom.
 * @param data Thread data object
 * @param cont Current continuation
 * @param p Input port
 */
static void _read_return_atom(void *data, object cont, port_type *p) 
{
  object sym;
  int len = p->tok_end;

  // Back up a char, since we always get here after reaching a terminal char
  // indicating we have the full atom
  p->buf_idx--;
  p->col_num--;
  p->tok_buf[p->tok_end] = '\0'; // TODO: what if buffer is full?
  p->tok_end = 0; // Reset for next atom

  if (_read_is_numeric(p->tok_buf, len)) {
    make_string(str, p->tok_buf);
    str.num_cp = Cyc_utf8_count_code_points((uint8_t *)(p->tok_buf));
    make_c_opaque(opq, &str);
    if (_read_is_complex_number(p->tok_buf, len)) {
      _read_return_complex_number(data, p, len);
    } else {
      return_thread_runnable_with_obj(data, &opq, p);
    }
  } else if (strncmp("+inf.0", p->tok_buf, 6) == 0 ||
             strncmp("-inf.0", p->tok_buf, 6) == 0) {
    make_double(d, pow(2.0, 1000000));
    return_thread_runnable_with_obj(data, &d, p);
  } else if (strncmp("+nan.0", p->tok_buf, 6) == 0 ||
             strncmp("-nan.0", p->tok_buf, 6) == 0) {
    make_double(d, 0.0 / 0.0);
    return_thread_runnable_with_obj(data, &d, p);
  } else {
    sym = find_or_add_symbol(p->tok_buf);
    return_thread_runnable_with_obj(data, sym, p);
  }
}

/**
 * @brief Helper macro for Cyc_io_read_token
 */
#define _read_next_char(data, cont, p) \
 if (p->mem_buf_len == 0 || p->mem_buf_len == p->buf_idx) { \
   int rv = read_from_port(p); \
   if (!rv) { \
     if (p->tok_end) _read_return_atom(data, cont, p); \
     return_thread_runnable_with_obj(data, Cyc_EOF, p); \
   } \
 } 

object Cyc_io_peek_char(void *data, object cont, object port)
{
  FILE *stream;
  port_type *p;
  uint32_t state = CYC_UTF8_ACCEPT;
  char_type codepoint;
  int c, i = 0, at_mem_buf_end = 0;
  char buf[5];

  Cyc_check_port(data, port);
  {
    p = (port_type *)port;
    stream = ((port_type *) port)->fp;
    if (stream == NULL) {
      Cyc_rt_raise2(data, "Unable to read from closed port: ", port);
    }
    set_thread_blocked(data, cont);
    if (p->mem_buf_len == 0 || p->mem_buf_len == p->buf_idx) {
      _read_next_char(data, cont, p);
    }
    c = p->mem_buf[p->buf_idx];
    if (Cyc_utf8_decode(&state, &codepoint, (uint8_t)c)) {
      // Only have a partial UTF8 code point, read more chars.
      // Problem is that there may not be enough space to store them
      // and do need to set them aside since we are just peeking here
      // and not actually supposed to be reading past chars.

      buf[0] = c;
      i = 1;
      while (i < 5) { // TODO: limit to 4 chars??
        if (p->mem_buf_len == p->buf_idx + i) {
          // No more buffered chars
          at_mem_buf_end = 1;
          c = fgetc(stream);
          if (c == EOF) break; // TODO: correct to do this here????
        } else {
          c = p->mem_buf[p->buf_idx + i];
        }
        buf[i++] = c;
        if (!Cyc_utf8_decode(&state, &codepoint, (uint8_t)c)) {
          break;
        }
      }
    }
    if (at_mem_buf_end && c != EOF) {
      p->buf_idx = 0;
      p->mem_buf_len = i;
      memmove(p->mem_buf, buf, i);
    }

    return_thread_runnable_with_obj(data, (c != EOF) ? obj_char2obj(codepoint) : Cyc_EOF, p);
  }
  return Cyc_EOF;
}

object Cyc_io_peek_u8(void *data, object cont, object port)
{
  FILE *stream;
  port_type *p;
  uint8_t c;

  Cyc_check_port(data, port);
  {
    p = (port_type *)port;
    stream = ((port_type *) port)->fp;
    if (stream == NULL) {
      Cyc_rt_raise2(data, "Unable to read from closed port: ", port);
    }
    set_thread_blocked(data, cont);
    if (p->mem_buf_len == 0 || p->mem_buf_len == p->buf_idx) {
      _read_next_char(data, cont, p);
    }
    c = p->mem_buf[p->buf_idx];
    return_thread_runnable_with_obj(data, (c != EOF) ? obj_int2obj(c) : Cyc_EOF, p);
  }
  return Cyc_EOF;
}

// TODO: full requirements are:
//
// Returns #t if a character is ready on the textual input
// port and returns #f otherwise. If char-ready returns #t
// then the next read-char operation on the given port is
// guaranteed not to hang. If the port is at end of file then
// char-ready? returns #t.
//
// This is a bit of a challenge because the internal buffers 
// cannot differentiate between being empty and being at EOF.
//
//object Cyc_io_char_ready(void *data, object cont, object port)
//{
//  port_type *p = (port_type *)port;
//  Cyc_check_port(data, port);
//}

object Cyc_io_read_char(void *data, object cont, object port)
{
  port_type *p = (port_type *)port;
  Cyc_check_port(data, port);
  if (p->fp == NULL) {
    Cyc_rt_raise2(data, "Unable to read from closed port: ", port);
  }
  {
    uint32_t state = CYC_UTF8_ACCEPT;
    char_type codepoint;
    int c;
    set_thread_blocked(data, cont);
    do {
      _read_next_char(data, cont, p);
      c = p->mem_buf[p->buf_idx++];
      if (c == EOF) break;
    } while(Cyc_utf8_decode(&state, &codepoint, (uint8_t)c));
// TODO: limit above to 4 chars and then thrown an error?
    p->col_num++;
    return_thread_runnable_with_obj(data, (c != EOF) ? obj_char2obj(codepoint) : Cyc_EOF, p);
  }
  return Cyc_EOF;
}

object Cyc_io_read_u8(void *data, object cont, object port)
{
  port_type *p = (port_type *)port;
  Cyc_check_port(data, port);
  if (p->fp == NULL) {
    Cyc_rt_raise2(data, "Unable to read from closed port: ", port);
  }
  {
    uint8_t c;
    set_thread_blocked(data, cont);
    _read_next_char(data, cont, p);
    c = p->mem_buf[p->buf_idx++];
    p->col_num++;
    return_thread_runnable_with_obj(data, (c != EOF) ? obj_int2obj(c) : Cyc_EOF, p);
  }
  return Cyc_EOF;
}

/* TODO: this function needs some work, but approximates what is needed */
object Cyc_io_read_line(void *data, object cont, object port)
{
  FILE *stream = ((port_type *) port)->fp;
  char buf[1027];
  int len, num_cp, i = 0;
  char_type codepoint;
  uint32_t state;

  Cyc_check_port(data, port);
  if (stream == NULL) {
    Cyc_rt_raise2(data, "Unable to read from closed port: ", port);
  }
  set_thread_blocked(data, cont);
  errno = 0;
  if (fgets(buf, 1023, stream) != NULL) {
    state = Cyc_utf8_count_code_points_and_bytes((uint8_t *)buf, &codepoint, &num_cp, &len);
    // Check if we stopped reading in the middle of a code point and
    // if so, read one byte at a time until that code point is finished.
    while (state != CYC_UTF8_ACCEPT && i < 3) {
      int c = fgetc(stream);
      buf[len] = c;
      len++;
      Cyc_utf8_decode(&state, &codepoint, (uint8_t)c);
      if (state == CYC_UTF8_ACCEPT) {
        num_cp++;
        break;
      }
      i++;
    }

    {
      // Remove any trailing CR / newline chars
      while (len > 0 && (buf[len - 1] == '\n' ||
                         buf[len - 1] == '\r')) {
        len--;
        num_cp--;
      }
      buf[len] = '\0';
      make_string_noalloc(s, buf, len);
      s.num_cp = num_cp;
      return_thread_runnable_with_obj(data, &s, port);
    }
  } else {
    if (feof(stream)) {
      return_thread_runnable_with_obj(data, Cyc_EOF, port);
    } else {
      // TODO: can't do this because we said thread could be blocked
      //Cyc_rt_raise2(data, "Error reading from file: ", obj_int2obj(errno));
      return_thread_runnable_with_obj(data, Cyc_EOF, port);
    }
  }
  return NULL;
}

/**
 * @brief Read next token from the input port.
 * @param data Thread data object
 * @param cont Current continuation
 * @param port Input port
 */
void Cyc_io_read_token(void *data, object cont, object port)
{
  Cyc_check_port(data, port);
  port_type *p = (port_type *)port;
  char c;

  // Find and return (to cont, so want to minimize stack growth if possible) next token from buf
  set_thread_blocked(data, cont);
  while (1) {
    // Do an I/O read for more data if buffer is full/empty
    _read_next_char(data, cont, p);

    // Process input one char at a time
    c = p->mem_buf[p->buf_idx++];
    p->col_num++;

    // If comment found, eat up comment chars
    if (c == ';') {
      if (p->tok_end) _read_return_atom(data, cont, p);
      _read_line_comment(p);
    } else if (c == '\n') {
      if (p->tok_end) _read_return_atom(data, cont, p);
      p->line_num++;
      p->col_num = 1;
    } else if (isspace(c)) {
      if (p->tok_end) _read_return_atom(data, cont, p);
      _read_whitespace(p);
    } else if (c == '(' || c == ')' || c == '\'' || c == '`') {
      if (p->tok_end) _read_return_atom(data, cont, p);
      // Special encoding so we can distinguish from chars such as #\(
      make_c_opaque(opq, obj_char2obj(c));
      return_thread_runnable_with_obj(data, &opq, p);
    } else if (c == ',') {
      if (p->tok_end) _read_return_atom(data, cont, p);

      _read_next_char(data, cont, p); // Do another buffer read if needed
      if (p->mem_buf[p->buf_idx] == '@') {
        object unquote_splicing = find_or_add_symbol(",@");
        make_empty_vector(vec);
        vec.num_elements = 2;
        vec.elements = (object *) alloca(sizeof(object) * vec.num_elements);
        vec.elements[0] = unquote_splicing;
        vec.elements[1] = boolean_f;
        p->buf_idx++;
        p->col_num++;
        return_thread_runnable_with_obj(data, &vec, p);
      } else {
        // Again, special encoding for syntax
        make_c_opaque(opq, obj_char2obj(c));
        return_thread_runnable_with_obj(data, &opq, p);
      }
    } else if (c == '"') {
      if (p->tok_end) _read_return_atom(data, cont, p);
      _read_string(data, cont, p);
    } else if (c == '#' && !p->tok_end) {
      _read_next_char(data, cont, p); // Fill buffer
      c = p->mem_buf[p->buf_idx++];
      p->col_num++;
      if (c == 't') {
        if ((p->mem_buf_len - p->buf_idx) >= 3 &&
            p->mem_buf[p->buf_idx + 0] == 'r' &&
            p->mem_buf[p->buf_idx + 1] == 'u' &&
            p->mem_buf[p->buf_idx + 2] == 'e') {
          p->buf_idx += 3;
          p->col_num += 3;
        }
        return_thread_runnable_with_obj(data, boolean_t, p);
      } else if (c == 'f') {
        if ((p->mem_buf_len - p->buf_idx) >= 4 &&
            p->mem_buf[p->buf_idx + 0] == 'a' &&
            p->mem_buf[p->buf_idx + 1] == 'l' &&
            p->mem_buf[p->buf_idx + 2] == 's' &&
            p->mem_buf[p->buf_idx + 3] == 'e') {
          p->buf_idx += 4;
          p->col_num += 4;
        }
        return_thread_runnable_with_obj(data, boolean_f, p);
      } else if (c == '\\') {
        _read_character(data, p);
      } else if (c == 'e') {
        _read_number(data, p, 10, 1);
      } else if (c == 'i') {
        _read_number(data, p, 10, 0);
      } else if (c == 'b') {
        _read_number(data, p, 2, 1);
      } else if (c == 'o') {
        _read_number(data, p, 8, 1);
      } else if (c == 'x') {
        _read_number(data, p, 16, 1);
      } else if (c == '(') { // Vector
        make_empty_vector(vec);
        return_thread_runnable_with_obj(data, &vec, p);
      } else if (c == 'u') { // Bytevector
        _read_next_char(data, cont, p); // Fill buffer
        c = p->mem_buf[p->buf_idx++];
        p->col_num++;
        if (c == '8') {
          _read_next_char(data, cont, p); // Fill buffer
          c = p->mem_buf[p->buf_idx++];
          p->col_num++;
          if (c == '(') {
            make_empty_bytevector(vec);
            return_thread_runnable_with_obj(data, &vec, p);
          } else {
            _read_error(data, p, "Unhandled input sequence");
          }
        } else {
          _read_error(data, p, "Unhandled input sequence");
        }
      } else if (c == '|') { // Block comment
        _read_multiline_comment(p);
        continue;
      } else if (c == ';') { // Datum comment
        object sym = find_or_add_symbol("#;");
        make_empty_vector(vec);
        vec.num_elements = 2;
        vec.elements = (object *) alloca(sizeof(object) * vec.num_elements);
        vec.elements[0] = sym;
        vec.elements[1] = boolean_f;
        return_thread_runnable_with_obj(data, &vec, p);
      } else {
        char buf[31];
        snprintf(buf, 30, "Unhandled input sequence %c", c);
        _read_error(data, p, buf);
      }
    } else if (c == '|' && !p->tok_end) {
      _read_literal_identifier(data, p);
    } else if (c == '[' || c == '{') {
      if (p->tok_end) _read_return_atom(data, cont, p);
      // Special encoding so we can distinguish from chars such as #\(
      make_c_opaque(opq, obj_char2obj('(')); // Cheap support for brackets
      return_thread_runnable_with_obj(data, &opq, p);
    } else if (c == ']' || c == '}') {
      if (p->tok_end) _read_return_atom(data, cont, p);
      // Special encoding so we can distinguish from chars such as #\(
      make_c_opaque(opq, obj_char2obj(')')); // Cheap support for brackets
      return_thread_runnable_with_obj(data, &opq, p);
    } else {
      // No special meaning, add char to current token (an atom)
      _read_add_to_tok_buf(p, c);
    }

  }
}

////////////// UTF-8 Section //////////////

// Copyright (c) 2008-2009 Bjoern Hoehrmann <bjoern@hoehrmann.de>
// See http://bjoern.hoehrmann.de/utf-8/decoder/dfa/ for details.
static const uint8_t utf8d[] = {
  // The first part of the table maps bytes to character classes that
  // to reduce the size of the transition table and create bitmasks.
   0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
   0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
   0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
   0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
   1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,  9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,
   7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,  7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,
   8,8,2,2,2,2,2,2,2,2,2,2,2,2,2,2,  2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,
  10,3,3,3,3,3,3,3,3,3,3,3,3,4,3,3, 11,6,6,6,5,8,8,8,8,8,8,8,8,8,8,8,

  // The second part is a transition table that maps a combination
  // of a state of the automaton and a character class to a state.
   0,12,24,36,60,96,84,12,12,12,48,72, 12,12,12,12,12,12,12,12,12,12,12,12,
  12, 0,12,12,12,12,12, 0,12, 0,12,12, 12,24,12,12,12,12,12,24,12,24,12,12,
  12,12,12,12,12,12,12,24,12,12,12,12, 12,24,12,12,12,12,12,12,12,24,12,12,
  12,12,12,12,12,12,12,36,12,36,12,12, 12,36,12,12,12,12,12,36,12,36,12,12,
  12,36,12,12,12,12,12,12,12,12,12,12, 
};

/**
 * @brief Decode the next byte of a codepoint.
 *        Based on example code from Bjoern Hoehrmann.
 * @param state Out parameter, the state of the decoding
 * @param codep Out parameter, contains the codepoint
 * @param byte Byte to examine
 * @return The current state: `CYC_UTF8_ACCEPT` if successful otherwise `CYC_UTF8_REJECT`.
 */
static uint32_t Cyc_utf8_decode(uint32_t* state, uint32_t* codep, uint32_t byte) {
  uint32_t type = utf8d[byte];

  *codep = (*state != CYC_UTF8_ACCEPT) ?
    (byte & 0x3fu) | (*codep << 6) :
    (0xff >> type) & (byte);

  *state = utf8d[256 + *state + type];
  return *state;
}
// END Bjoern Hoehrmann

/**
 * @brief Count the number of code points in a string.
 *        Based on example code from Bjoern Hoehrmann.
 * @param s String to examine
 * @return The number of codepoints found, or -1 if there was an error.
 */
int Cyc_utf8_count_code_points(uint8_t* s) {
  uint32_t codepoint;
  uint32_t state = 0;
  int count;

  for (count = 0; *s; ++s)
    if (!Cyc_utf8_decode(&state, &codepoint, *s))
      count += 1;

  if (state != CYC_UTF8_ACCEPT)
    return -1;
  return count;
}

/**
 * @brief Count the number of code points and bytes in a string.
 * @param s String to examine
 * @param codepoint Out parameter, set to the codepoint.
 * @param cpts Out parameter, set to the number of code points
 * @param bytes Out parameter, set to the number of bytes
 * @return Returns `CYC_UTF8_ACCEPT`  on success, otherwise `CYC_UTF8_REJECT`.
 */
static int Cyc_utf8_count_code_points_and_bytes(uint8_t* s, char_type *codepoint, int *cpts, int *bytes) {
  uint32_t state = 0;
  *cpts = 0;
  *bytes = 0;
  for (; *s; ++s){
    *bytes += 1;
    if (!Cyc_utf8_decode(&state, codepoint, *s))
      *cpts += 1;
  }

  if (state != CYC_UTF8_ACCEPT)
    return state;
  return 0;
}

// TODO: index into X codepoint in a string 

/**
 * @brief
 * Use this when validating from a stream, as it may be that the stream stopped
 * in the middle of a codepoint, hence state passed in as an arg, so it can be
 * tested in a loop and also after the loop has finished.
 *
 * From https://stackoverflow.com/a/22135005/101258
 */
uint32_t Cyc_utf8_validate_stream(uint32_t *state, char *str, size_t len) {
   size_t i;
   uint32_t type;

    for (i = 0; i < len; i++) {
        // We don't care about the codepoint, so this is
        // a simplified version of the decode function.
        type = utf8d[(uint8_t)str[i]];
        *state = utf8d[256 + (*state) + type];

        if (*state == CYC_UTF8_REJECT)
            break;
    }

    return *state;
}

/**
 * @brief Simplified version of Cyc_utf8_validate_stream that must always be called with a complete string buffer.
 */
uint32_t Cyc_utf8_validate(char *str, size_t len) {
   size_t i;
   uint32_t state = CYC_UTF8_ACCEPT, type;

    for (i = 0; i < len; i++) {
        // We don't care about the codepoint, so this is
        // a simplified version of the decode function.
        type = utf8d[(uint8_t)str[i]];
        state = utf8d[256 + (state) + type];

        if (state == CYC_UTF8_REJECT)
            break;
    }

    return state;
}

//int uint32_num_bytes(uint32_t x) {
//  // TODO: could compute log(val) / log(256)
//  if (x < 0x100) return 1;
//  if (x < 0x10000) return 2;
//  if (x < 0x1000000) return 3;
//  return 4;
//}

/**
 * This function takes one or more 32-bit chars and encodes them 
 * as an array of UTF-8 bytes.
 * FROM: https://www.cprogramming.com/tutorial/utf8.c
 *
 * @param dest    Destination byte buffer
 * @param sz      size of dest buffer in bytes
 * @param src     Buffer of source data, in 32-bit characters
 * @param srcsz   number of source characters, or -1 if 0-terminated
 *
 * @return Number of characters converted
 *
 * dest will only be '\0'-terminated if there is enough space. this is
 * for consistency; imagine there are 2 bytes of space left, but the next
 * character requires 3 bytes. in this case we could NUL-terminate, but in
 * general we can't when there's insufficient space. therefore this function
 * only NUL-terminates if all the characters fit, and there's space for
 * the NUL as well.
 * the destination string will never be bigger than the source string.
 */
int Cyc_utf8_encode(char *dest, int sz, uint32_t *src, int srcsz)
{
    u_int32_t ch;
    int i = 0;
    char *dest_end = dest + sz;

    while (srcsz<0 ? src[i]!=0 : i < srcsz) {
        ch = src[i];
        if (ch < 0x80) {
            if (dest >= dest_end)
                return i;
            *dest++ = (char)ch;
        }
        else if (ch < 0x800) {
            if (dest >= dest_end-1)
                return i;
            *dest++ = (ch>>6) | 0xC0;
            *dest++ = (ch & 0x3F) | 0x80;
        }
        else if (ch < 0x10000) {
            if (dest >= dest_end-2)
                return i;
            *dest++ = (ch>>12) | 0xE0;
            *dest++ = ((ch>>6) & 0x3F) | 0x80;
            *dest++ = (ch & 0x3F) | 0x80;
        }
        else if (ch < 0x110000) {
            if (dest >= dest_end-3)
                return i;
            *dest++ = (ch>>18) | 0xF0;
            *dest++ = ((ch>>12) & 0x3F) | 0x80;
            *dest++ = ((ch>>6) & 0x3F) | 0x80;
            *dest++ = (ch & 0x3F) | 0x80;
        }
        i++;
    }
    if (dest < dest_end)
        *dest = '\0';
    return i;
}


////////////// END UTF-8 Section //////////////

/** 
 * Cyclone Scheme
 * Copyright (c) 2014, Justin Ethier
 * All rights reserved.
 *
 * This file contains the C runtime used by compiled programs.
 */

#include <ck_hs.h>
#include "cyclone/types.h"
#include "cyclone/runtime.h"
#include "cyclone/ck_ht_hash.h"
//#include <signal.h> // TODO: only used for debugging!

//int JAE_DEBUG = 0;
//int gcMoveCountsDEBUG[20] = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};

object Cyc_global_set(void *thd, object *glo, object value)
{
  gc_mut_update((gc_thread_data *)thd, *glo, value);
  *(glo) = value;
  return value;
}

/* Error checking section - type mismatch, num args, etc */
/* Type names to use for error messages */
const char *tag_names[21] = { \
   "pair" \
 , "symbol" \
 , "" \
 , "procedure" \
 , "procedure" \
 , "procedure" \
 , "procedure" \
 , "procedure" \
 , "procedure" \
 , "number" \
 , "number" \
 , "string" \
 , "primitive" \
 , "eof" \
 , "port" \
 , "boolean" \
 , "C primitive" \
 , "vector" \
 , "macro" \
 , "Reserved for future use" \
 , "Reserved for future use" };

void Cyc_invalid_type_error(void *data, int tag, object found) {
  char buf[256];
  snprintf(buf, 255, "Invalid type: expected %s, found ", tag_names[tag]);
  //snprintf(buf, 255, "Invalid type: expected %s, found (%p) ", tag_names[tag], found);
  Cyc_rt_raise2(data, buf, found);
}

void Cyc_check_obj(void *data, int tag, object obj) {
  if (!is_object_type(obj)) {
    Cyc_invalid_type_error(data, tag, obj);
  }
}

void Cyc_check_bounds(void *data, const char *label, int len, int index) {
  if (index < 0 || index >= len) {
    char buf[128];
    snprintf(buf, 127, "%s - invalid index %d", label, index);
    Cyc_rt_raise_msg(data, buf);
  }
}

/* END error checking */

/* These macros are hardcoded here to support functions in this module. */
#define closcall1(td,cfn,a1) if (type_of(cfn) == cons_tag || prim(cfn)) { Cyc_apply(td,0, (closure)a1, cfn); } else { ((cfn)->fn)(td,1,cfn,a1);}
/* Return to continuation after checking for stack overflow. */
#define return_closcall1(td,cfn,a1) \
{char stack; \
 if (check_overflow(&stack,(((gc_thread_data *)data)->stack_limit))) { \
     object buf[1]; buf[0] = a1;\
     GC(td,cfn,buf,1); return; \
 } else {closcall1(td,(closure) (cfn),a1); return;}}
#define closcall2(td,cfn,a1,a2) if (type_of(cfn) == cons_tag || prim(cfn)) { Cyc_apply(td,1, (closure)a1, cfn,a2); } else { ((cfn)->fn)(td,2,cfn,a1,a2);}
/* Return to continuation after checking for stack overflow. */
#define return_closcall2(td,cfn,a1,a2) \
{char stack; \
 if (check_overflow(&stack,(((gc_thread_data *)data)->stack_limit))) { \
     object buf[2]; buf[0] = a1;buf[1] = a2;\
     GC(td,cfn,buf,2); return; \
 } else {closcall2(td,(closure) (cfn),a1,a2); return;}}
/*END closcall section */

/* Global variables. */
static gc_heap *Cyc_heap;
long no_gcs = 0; /* Count the number of GC's. */
long no_major_gcs = 0; /* Count the number of GC's. */

object Cyc_global_variables = nil;
int _cyc_argc = 0;
char **_cyc_argv = NULL;

static symbol_type __EOF = {{0}, eof_tag, "", nil}; // symbol_type in lieu of custom type
const object Cyc_EOF = &__EOF;
static ck_hs_t symbol_table;
static int symbol_table_size = 65536;
static pthread_mutex_t symbol_table_lock;

// Functions to support concurrency kit hashset
// These are specifically for a table of symbols
static void *hs_malloc(size_t r)
{
    return malloc(r);
}

static void hs_free(void *p, size_t b, bool r)
{
//    (void)b;
//    (void)r;
    free(p);
//    return;
}

static struct ck_malloc my_allocator = {
    .malloc = hs_malloc,
    .free = hs_free
};

static unsigned long hs_hash(const void *object, unsigned long seed)
{
  const symbol_type *c = object;
  unsigned long h;

  h = (unsigned long)MurmurHash64A(c->pname, strlen(c->pname), seed);
  return h;
}

static bool hs_compare(const void *previous, const void *compare)
{
  return strcmp(symbol_pname(previous), symbol_pname(compare)) == 0;
}

static void *set_get(ck_hs_t *hs, const void *value)
{
  unsigned long h;
  void *v;

  h = CK_HS_HASH(hs, hs_hash, value);
  v = ck_hs_get(hs, h, value);
  return v;
}

static bool set_insert(ck_hs_t *hs, const void *value)
{
  unsigned long h;

  h = CK_HS_HASH(hs, hs_hash, value);
  return ck_hs_put(hs, h, value);
}
// End supporting functions

void gc_init_heap(long heap_size)
{
  Cyc_heap = gc_heap_create(heap_size, 0, 0);
  if (!ck_hs_init(&symbol_table, 
                  CK_HS_MODE_OBJECT | CK_HS_MODE_SPMC,
                  hs_hash, hs_compare,
                  &my_allocator,
                  symbol_table_size, 
                  43423)){
    fprintf(stderr, "Unable to initialize symbol table\n");
    exit(1);
  }
  if (pthread_mutex_init(&(symbol_table_lock), NULL) != 0) {
    fprintf(stderr, "Unable to initialize symbol_table_lock mutex\n");
    exit(1);
  }
}

gc_heap *gc_get_heap()
{
  return Cyc_heap;
}

object cell_get(object cell){
    return car(cell);
}

static boolean_type t_boolean = {{0}, boolean_tag, "t"};
static boolean_type f_boolean = {{0}, boolean_tag, "f"};
const object boolean_t = &t_boolean;
const object boolean_f = &f_boolean;

static symbol_type Cyc_void_symbol = {{0}, symbol_tag, "", nil};
const object quote_void = &Cyc_void_symbol;

/* Stack Traces */
void Cyc_st_add(void *data, char *frame) { 
  gc_thread_data *thd = (gc_thread_data *)data;
  // Do not allow recursion to remove older frames
  if (frame != thd->stack_prev_frame) { 
    thd->stack_prev_frame = frame;
    thd->stack_traces[thd->stack_trace_idx] = frame;
    thd->stack_trace_idx = (thd->stack_trace_idx + 1) % MAX_STACK_TRACES;
  }
}

void Cyc_st_print(void *data, FILE *out) {
  /* print to stream, note it is possible that
     some traces could be on the stack after a GC.
     not sure what to do about it, may need to
     detect that case and stop printing.
     or, with the tbl being so small, maybe it will
     not be an issue in practice? a bit risky to ignore though
  */
  gc_thread_data *thd = (gc_thread_data *)data;
  int i = (thd->stack_trace_idx + 1) % MAX_STACK_TRACES;
  while (i != thd->stack_trace_idx) {
    if (thd->stack_traces[i]) {
      fprintf(out, "%s\n", thd->stack_traces[i]);
    }
    i = (i + 1) % MAX_STACK_TRACES;
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
char *_strdup (const char *s) {
    char *d = malloc (strlen (s) + 1);
    if (d) { strcpy (d,s); }
    return d;
}

object find_symbol_by_name(const char *name) {
  symbol_type tmp = {{0}, symbol_tag, name, nil};
  object result = set_get(&symbol_table, &tmp);
  //if (result) {
  //  printf("found symbol %s\n", symbol_pname(result));
  //}
  return result;
}

object add_symbol(symbol_type *psym) {
  //printf("Adding symbol %s, table size = %ld\n", symbol_pname(psym), ck_hs_count(&symbol_table));
  pthread_mutex_lock(&symbol_table_lock); // Only 1 "writer" allowed
  if (ck_hs_count(&symbol_table) == symbol_table_size) {
    // TODO: grow table if it is not big enough
    fprintf(stderr, "Ran out of symbol table entries\n");
    exit(1);
  }
  set_insert(&symbol_table, psym);
  pthread_mutex_unlock(&symbol_table_lock);
  return psym;
}

object add_symbol_by_name(const char *name) {
  symbol_type sym = {{0}, symbol_tag, _strdup(name), nil};
  symbol_type *psym = malloc(sizeof(symbol_type));
  memcpy(psym, &sym, sizeof(symbol_type));
  return add_symbol(psym);
}

object find_or_add_symbol(const char *name){
  object sym = find_symbol_by_name(name);
  if (sym){
    return sym;
  } else {
    return add_symbol_by_name(name);
  }
}

/* END symbol table */

/* Global table */
list global_table = nil;

void add_global(object *glo) {
  // It would probably be more efficient to allocate
  // a contiguous block of memory for this... for now
  // this is more expedient
  global_table = mcons(mcvar(glo), global_table);
}

void debug_dump_globals()
{
  list l = global_table;
  for(; !nullp(l); l = cdr(l)){
   cvar_type *c = (cvar_type *)car(l);
   //gc_mark(h, *(c->pvar)); // Mark actual object the global points to
   printf("DEBUG %p ", c->pvar);
   if (*c->pvar){
     printf("mark = %d ", mark(*c->pvar));
     if (mark(*c->pvar) == gc_color_red) {
       printf("obj = ");
       Cyc_display(*c->pvar, stdout);
     }
     printf("\n");
   } else { 
     printf(" is NULL\n");
   }
  }
}

/* END Global table */

/* Mutation table
 *
 * Keep track of mutations (EG: set-car!) so that new
 * values are transported to the heap during GC.
 */
list mutation_table = nil;

void add_mutation(object var, object value){
  if (is_object_type(value)) {
    mutation_table = mcons(var, mutation_table);
  }
}

/* TODO: consider a more efficient implementation, such as reusing old nodes
         instead of reclaiming them each time
 */
void clear_mutations() {
  list l = mutation_table, next;
  while (!nullp(l)) {
    next = cdr(l);
    free(l);
    l = next;
  }
  mutation_table = nil;
}
/* END mutation table */

/* Runtime globals */
object Cyc_glo_call_cc = nil;
object Cyc_glo_eval = nil;

/* Exception handler */
object Cyc_exception_handler_stack = nil;

object Cyc_default_exception_handler(void *data, int argc, closure _, object err) {
    fprintf(stderr, "Error: ");

    if (nullp(err) || is_value_type(err) || type_of(err) != cons_tag) {
      Cyc_display(err, stderr);
    } else {
      // Error is list of form (type arg1 ... argn)
      err = cdr(err); // skip type field
      for (; !nullp(err); err = cdr(err)){ // output with no enclosing parens
        Cyc_display(car(err), stderr);
        fprintf(stderr, " ");
      }
    }

    fprintf(stderr, "\nCall history:\n");
    Cyc_st_print(data, stderr);
    fprintf(stderr, "\n");
    //raise(SIGINT); // break into debugger, unix only
    exit(1);
    return nil;
}

object Cyc_current_exception_handler() {
  if (nullp(Cyc_exception_handler_stack)) {
    return primitive_Cyc_91default_91exception_91handler;
  } else {
    return car(Cyc_exception_handler_stack);
  }
}

/* Raise an exception from the runtime code */
void Cyc_rt_raise(void *data, object err) {
    make_cons(c2, err, nil);
    make_cons(c1, boolean_f, &c2);
    make_cons(c0, &c1, nil);
    apply(data, nil, Cyc_current_exception_handler(), &c0);
    // Should never get here
    fprintf(stderr, "Internal error in Cyc_rt_raise\n");
    exit(1);
}
void Cyc_rt_raise2(void *data, const char *msg, object err) {
    make_string(s, msg);
    make_cons(c3, err, nil);
    make_cons(c2, &s, &c3);
    make_cons(c1, boolean_f, &c2);
    make_cons(c0, &c1, nil);
    apply(data, nil, Cyc_current_exception_handler(), &c0);
    // Should never get here
    fprintf(stderr, "Internal error in Cyc_rt_raise2\n");
    exit(1);
}
void Cyc_rt_raise_msg(void *data, const char *err) {
    make_string(s, err);
    Cyc_rt_raise(data, &s);
}
/* END exception handler */

int equal(x, y) object x, y;
{
    if (nullp(x)) return nullp(y);
    if (nullp(y)) return nullp(x);
    if (obj_is_char(x)) return obj_is_char(y) && x == y;
    switch(type_of(x)) {
    case integer_tag:
      return (type_of(y) == integer_tag &&
              ((integer_type *) x)->value == ((integer_type *) y)->value);
    case double_tag:
      return (type_of(y) == double_tag &&
              ((double_type *) x)->value == ((double_type *) y)->value);
    case string_tag:
      return (type_of(y) == string_tag &&
              strcmp(((string_type *) x)->str,
                     ((string_type *) y)->str) == 0);
    case vector_tag:
      if (type_of(y) == vector_tag && 
          ((vector)x)->num_elt == ((vector)y)->num_elt) {
        int i;
        for (i = 0; i < ((vector)x)->num_elt; i++) { 
          if (equalp(((vector)x)->elts[i], ((vector)y)->elts[i]) == boolean_f)
            return 0;
        }
        return 1;
      }
      return  0;
    default:
      return x == y;
    }
}

object Cyc_get_global_variables(){
    return Cyc_global_variables;
}

object Cyc_get_cvar(object var) {
    if (is_object_type(var) && type_of(var) == cvar_tag) {
        return *(((cvar_type *)var)->pvar);
    }
    return var;
}

object Cyc_set_cvar(object var, object value) {
    if (is_object_type(var) && type_of(var) == cvar_tag) {
        *(((cvar_type *)var)->pvar) = value;
    }
    return var;}

object Cyc_has_cycle(object lst) {
    object slow_lst, fast_lst;
    if (nullp(lst) || is_value_type(lst) ||
        (is_object_type(lst) && type_of(lst) != cons_tag)) {
        return (boolean_f);
    }
    slow_lst = lst;
    fast_lst = cdr(lst);
    while(1) {
        if (nullp(fast_lst)) return boolean_f;
        if (Cyc_is_cons(fast_lst) == boolean_f) return boolean_f;
        if (nullp(cdr(fast_lst))) return boolean_f;
        if (Cyc_is_cons(cdr(fast_lst)) == boolean_f) return boolean_f;
        if (is_object_type(car(slow_lst)) && 
            boolean_f == Cyc_is_boolean(car(slow_lst)) && // Avoid expected dupes
            //boolean_f == Cyc_is_symbol(car(slow_lst)) &&  // 
            eq(car(slow_lst), car(fast_lst))) return boolean_t;

        slow_lst = cdr(slow_lst);
        fast_lst = cddr(fast_lst);
    }
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
object dispatch_display_va(void *data, int argc, object clo, object cont, object x, ...) {
  object result;
  va_list ap;
  va_start(ap, x);
  result = Cyc_display_va_list(argc - 1, x, ap);
  va_end(ap);
  return_closcall1(data, cont, result);
}

object Cyc_display_va(int argc, object x, ...) {
  object result;
  va_list ap;
  va_start(ap, x);
  result = Cyc_display_va_list(argc, x, ap);
  va_end(ap);
  return result;
}

object Cyc_display_va_list(int argc, object x, va_list ap) {
  FILE *fp = stdout; // TODO: just a placeholder, should use current-output-port
  if (argc > 1) {
    object tmp;
    tmp = va_arg(ap, object);
    fp = ((port_type *)tmp)->fp;
  }
  return Cyc_display(x, fp);}

object Cyc_display(object x, FILE *port)
{object tmp = nil;
 object has_cycle = boolean_f;
 int i = 0;
 if (nullp(x)) {fprintf(port, "()"); return quote_void;}
 if (obj_is_char(x)) {fprintf(port, "%c", obj_obj2char(x)); return quote_void;}
 switch (type_of(x))
   {case macro_tag:
      fprintf(port, "<macro %p>",(void *)((closure) x)->fn);
      break;
    case closure0_tag:
    case closure1_tag:
    case closure2_tag:
    case closure3_tag:
    case closure4_tag:
    case closureN_tag:
      fprintf(port, "<procedure %p>",(void *)((closure) x)->fn);
      break;
    case eof_tag:
      fprintf(port, "<EOF>");
      break;
    case port_tag:
      fprintf(port, "<port %p>", ((port_type *)x)->fp);
      break;
    case primitive_tag:
      fprintf(port, "<primitive %s>", prim_name(x));
      break;
    case cvar_tag:
      Cyc_display(Cyc_get_cvar(x), port);
      break;
    case boolean_tag:
      fprintf(port, "#%s",((boolean_type *) x)->pname);
      break;
    case symbol_tag:
      fprintf(port, "%s",((symbol_type *) x)->pname);
      break;
    case integer_tag:
      fprintf(port, "%d", ((integer_type *) x)->value);
      break;
    case double_tag:
      fprintf(port, "%lf", ((double_type *) x)->value);
      break;
    case string_tag:
      fprintf(port, "%s", ((string_type *) x)->str);
      break;
    case vector_tag:
      fprintf(port, "#(");
      for (i = 0; i < ((vector) x)->num_elt; i++) {
        if (i > 0) { 
          fprintf(port, " "); 
        }
        Cyc_display(((vector)x)->elts[i], port);
      }
      fprintf(port, ")");
      break;
    case cons_tag:
      has_cycle = Cyc_has_cycle(x);
      fprintf(port, "("); 
      Cyc_display(car(x), port);

      // Experimenting with displaying lambda defs in REPL
      // not good enough but this is a start. would probably need
      // the same code in write()
      if (Cyc_is_symbol(car(x)) == boolean_t &&
          strncmp(((symbol)car(x))->pname, "procedure", 10) == 0) {
          fprintf(port, " ");
          Cyc_display(cadr(x), port);
          fprintf(port, " ...)"); /* skip body and env for now */
          break;
      }

      for (tmp = cdr(x); tmp && ((closure) tmp)->tag == cons_tag; tmp = cdr(tmp)) {
          if (has_cycle == boolean_t) {
              if (i++ > 20) break; /* arbitrary number, for now */
          }
          fprintf(port, " ");
          Cyc_display(car(tmp), port);
      }
      if (has_cycle == boolean_t) {
          fprintf(port, " ...");
      } else if (tmp) {
          fprintf(port, " . ");
          Cyc_display(tmp, port);
      }
      fprintf(port, ")");
      break;
    default:
      fprintf(port, "Cyc_display: bad tag x=%ld\n", ((closure)x)->tag); 
      exit(1);
 }
 return quote_void;}

object dispatch_write_va(void *data, int argc, object clo, object cont, object x, ...) {
  object result;
  va_list ap;
  va_start(ap, x);
  result = Cyc_write_va_list(argc - 1, x, ap);
  va_end(ap);
  return_closcall1(data, cont, result);
}

object Cyc_write_va(int argc, object x, ...) {
  object result;
  va_list ap;
  va_start(ap, x);
  result = Cyc_write_va_list(argc, x, ap);
  va_end(ap);
  return result;
}

object Cyc_write_va_list(int argc, object x, va_list ap) {
  FILE *fp = stdout; // OK since this is the internal version of write
                     // Longer-term maybe we get rid of varargs for this one
  if (argc > 1) {
    object tmp;
    tmp = va_arg(ap, object);
    fp = ((port_type *)tmp)->fp;
  }
  return Cyc_write(x, fp);}

static object _Cyc_write(object x, FILE *port)
{object tmp = nil;
 object has_cycle = boolean_f;
 int i = 0;
 if (nullp(x)) {fprintf(port, "()"); return quote_void;}
 if (obj_is_char(x)) {fprintf(port, "#\\%c", obj_obj2char(x)); return quote_void;}
 switch (type_of(x))
   {case string_tag:
      fprintf(port, "\"%s\"", ((string_type *) x)->str);
      break;
    // TODO: what about a list? contents should be displayed per (write)
    case cons_tag:
      has_cycle = Cyc_has_cycle(x);
      fprintf(port, "("); 
      _Cyc_write(car(x), port);

      // Experimenting with displaying lambda defs in REPL
      // not good enough but this is a start. would probably need
      // the same code in write()
      if (Cyc_is_symbol(car(x)) == boolean_t &&
          strncmp(((symbol)car(x))->pname, "procedure", 10) == 0) {
          fprintf(port, " ");
          _Cyc_write(cadr(x), port);
          fprintf(port, " ...)"); /* skip body and env for now */
          break;
      }

      for (tmp = cdr(x); tmp && ((closure) tmp)->tag == cons_tag; tmp = cdr(tmp)) {
          if (has_cycle == boolean_t) {
              if (i++ > 20) break; /* arbitrary number, for now */
          }
          fprintf(port, " ");
          _Cyc_write(car(tmp), port);
      }
      if (has_cycle == boolean_t) {
          fprintf(port, " ...");
      } else if (tmp) {
          fprintf(port, " . ");
          _Cyc_write(tmp, port);
      }
      fprintf(port, ")");
      break;
    default:
      Cyc_display(x, port);}
 return quote_void;}

object Cyc_write(object x, FILE *port)
{object y = _Cyc_write(x, port);
 fprintf(port, "\n");
 return y;}

object Cyc_write_char(void *data, object c, object port) 
{
  if (obj_is_char(c)) {
    fprintf(((port_type *)port)->fp, "%c", obj_obj2char(c));
  } else {
    Cyc_rt_raise2(data, "Argument is not a character", c);
  }
  return quote_void;
}

// TODO: should not be a predicate, may end up moving these to Scheme code
object memberp(void *data, object x, list l)
{Cyc_check_cons_or_nil(data, l);
 for (; !nullp(l); l = cdr(l)) if (boolean_f != equalp(x,car(l))) return boolean_t;
 return boolean_f;}

object memqp(void *data, object x, list l)
{Cyc_check_cons_or_nil(data, l);
 for (; !nullp(l); l = cdr(l)) if (eq(x,car(l))) return boolean_t;
 return boolean_f;}

object get(x,i) object x,i;
{register object plist; register object plistd;
 if (nullp(x)) return x;
 if (type_of(x)!=symbol_tag) {printf("get: bad x=%ld\n",((closure)x)->tag); exit(0);}
 plist = symbol_plist(x);
 for (; !nullp(plist); plist = cdr(plistd))
   {plistd = cdr(plist);
    if (eq(car(plist),i)) return car(plistd);}
 return nil;}

object equalp(x,y) object x,y;
{for (; ; x = cdr(x), y = cdr(y))
   {if (equal(x,y)) return boolean_t;
    if (obj_is_char(x) || obj_is_char(y) || 
        nullp(x) || nullp(y) ||
        type_of(x)!=cons_tag || type_of(y)!=cons_tag) return boolean_f;
    if (boolean_f == equalp(car(x),car(y))) return boolean_f;}}

list assq(void *data, object x, list l)
{if (nullp(l) || is_value_type(l) || type_of(l) != cons_tag) return boolean_f;
 for (; !nullp(l); l = cdr(l))
   {register list la = car(l); 
    Cyc_check_cons(data, la);
    if (eq(x,car(la))) return la;}
 return boolean_f;}

list assoc(void *data, object x, list l)
{if (nullp(l) || is_value_type(l) || type_of(l) != cons_tag) return boolean_f;
 for (; !nullp(l); l = cdr(l))
   {register list la = car(l); 
    Cyc_check_cons(data, la);
    if (boolean_f != equalp(x,car(la))) return la;}
 return boolean_f;}


// TODO: generate these using macros???
object __num_eq(void *data, object x, object y)
{Cyc_check_num(data, x);
 Cyc_check_num(data, y);
 if (((integer_type *)x)->value == ((integer_type *)y)->value)
    return boolean_t;
 return boolean_f;}

object __num_gt(void *data, object x, object y)
{Cyc_check_num(data, x);
 Cyc_check_num(data, y);
 if (((integer_type *)x)->value > ((integer_type *)y)->value)
    return boolean_t;
 return boolean_f;}

object __num_lt(void *data, object x, object y)
{Cyc_check_num(data, x);
 Cyc_check_num(data, y);
 if (((integer_type *)x)->value < ((integer_type *)y)->value)
    return boolean_t;
 return boolean_f;}

object __num_gte(void *data, object x, object y)
{Cyc_check_num(data, x);
 Cyc_check_num(data, y);
 if (((integer_type *)x)->value >= ((integer_type *)y)->value)
    return boolean_t;
 return boolean_f;}

object __num_lte(void *data, object x, object y)
{Cyc_check_num(data, x);
 Cyc_check_num(data, y);
 if (((integer_type *)x)->value <= ((integer_type *)y)->value)
    return boolean_t;
 return boolean_f;}

// TODO: object Cyc_is_eq(x, y) object x, y)
object Cyc_is_boolean(object o){
    if (!nullp(o) && 
        !is_value_type(o) &&
        ((list)o)->tag == boolean_tag &&
        (eq(boolean_f, o) || eq(boolean_t, o)))
        return boolean_t;
    return boolean_f;}

object Cyc_is_cons(object o){
    if (!nullp(o) && !is_value_type(o) && ((list)o)->tag == cons_tag)
        return boolean_t;
    return boolean_f;}

object Cyc_is_null(object o){
    if (nullp(o)) 
        return boolean_t;
    return boolean_f;}

object Cyc_is_number(object o){
    if (!nullp(o) && !is_value_type(o) && 
        (type_of(o) == integer_tag || type_of(o) == double_tag))
        return boolean_t;
    return boolean_f;}

object Cyc_is_real(object o){
    return Cyc_is_number(o);}

object Cyc_is_integer(object o){
    if (!nullp(o) && !is_value_type(o) && type_of(o) == integer_tag)
        return boolean_t;
    return boolean_f;}

object Cyc_is_symbol(object o){
    if (!nullp(o) && !is_value_type(o) && ((list)o)->tag == symbol_tag)
        return boolean_t;
    return boolean_f;}

object Cyc_is_vector(object o){
    if (!nullp(o) && !is_value_type(o) && ((list)o)->tag == vector_tag)
        return boolean_t;
    return boolean_f;}

object Cyc_is_port(object o){
    if (!nullp(o) && !is_value_type(o) && ((list)o)->tag == port_tag)
        return boolean_t;
    return boolean_f;}

object Cyc_is_string(object o){
    if (!nullp(o) && !is_value_type(o) && ((list)o)->tag == string_tag)
        return boolean_t;
    return boolean_f;}

object Cyc_is_char(object o){
    if (obj_is_char(o))
        return boolean_t;
    return boolean_f;}

object Cyc_is_procedure(void *data, object o) {
    int tag;
    if (!nullp(o) && !is_value_type(o)) {
        tag = type_of(o);
        if (tag == closure0_tag ||
            tag == closure1_tag ||
            tag == closure2_tag ||
            tag == closure3_tag ||
            tag == closure4_tag ||
            tag == closureN_tag ||
            tag == primitive_tag) {
            return boolean_t;
        } else if (tag == cons_tag) {
          integer_type l = Cyc_length(data, o);
          if (l.value > 0 && Cyc_is_symbol(car(o)) == boolean_t) {
            if (strncmp(((symbol)car(o))->pname, "primitive", 10) == 0 ||
                strncmp(((symbol)car(o))->pname, "procedure", 10) == 0 ) {
              return boolean_t;
            }
          }
        }
    }
    return boolean_f;
}

object Cyc_is_macro(object o) {
    int tag;
    if (!nullp(o) && !is_value_type(o)) {
        tag = type_of(o);
        if (tag == macro_tag) {
          return boolean_t;
        }
    }
    return boolean_f;
}

object Cyc_is_eof_object(object o) {
    if (!nullp(o) && !is_value_type(o) && type_of(o) == eof_tag)
        return boolean_t;
    return boolean_f;}

object Cyc_is_cvar(object o) {
    if (!nullp(o) && !is_value_type(o) && type_of(o) == cvar_tag)
        return boolean_t;
    return boolean_f;}

object Cyc_eq(object x, object y) {
    if (eq(x, y))
        return boolean_t;
    return boolean_f;
}

object Cyc_set_car(void *data, object l, object val) {
    if (Cyc_is_cons(l) == boolean_f) Cyc_invalid_type_error(data, cons_tag, l);
    gc_mut_update((gc_thread_data *)data, car(l), val);
    car(l) = val;
    add_mutation(l, val);
    return l;
}

object Cyc_set_cdr(void *data, object l, object val) {
    if (Cyc_is_cons(l) == boolean_f) Cyc_invalid_type_error(data, cons_tag, l);
    gc_mut_update((gc_thread_data *)data, cdr(l), val);
    cdr(l) = val;
    add_mutation(l, val);
    return l;
}

object Cyc_vector_set(void *data, object v, object k, object obj) {
  int idx;
  Cyc_check_vec(data, v);
  Cyc_check_int(data, k);
  idx = ((integer_type *)k)->value;

  if (idx < 0 || idx >= ((vector)v)->num_elt) {
    Cyc_rt_raise2(data, "vector-set! - invalid index", k);
  }

  gc_mut_update((gc_thread_data *)data, 
                ((vector)v)->elts[idx],
                obj);

  ((vector)v)->elts[idx] = obj;
  // TODO: probably could be more efficient here and also pass
  //       index, so only that one entry needs GC.
  add_mutation(v, obj);
  return v;
}

object Cyc_vector_ref(void *data, object v, object k) {
  if (nullp(v) || is_value_type(v) || ((list)v)->tag != vector_tag) {
    Cyc_rt_raise_msg(data, "vector-ref - invalid parameter, expected vector\n"); 
  }
  if (nullp(k) || is_value_type(k) || ((list)k)->tag != integer_tag) {
    Cyc_rt_raise_msg(data, "vector-ref - invalid parameter, expected integer\n"); 
  }
  if (integer_value(k) < 0 || integer_value(k) >= ((vector)v)->num_elt) {
    Cyc_rt_raise2(data, "vector-ref - invalid index", k);
  }

  return ((vector)v)->elts[((integer_type *)k)->value];
}

integer_type Cyc_vector_length(void *data, object v) {
    if (!nullp(v) && !is_value_type(v) && ((list)v)->tag == vector_tag) {
      make_int(len, ((vector)v)->num_elt);
      return len;
    }
    Cyc_rt_raise_msg(data, "vector-length - invalid parameter, expected vector\n"); }

integer_type Cyc_length(void *data, object l){
    make_int(len, 0);
    while(!nullp(l)){
        if (is_value_type(l) || ((list)l)->tag != cons_tag){
            Cyc_rt_raise_msg(data, "length - invalid parameter, expected list\n");
        }
        l = cdr(l);
        len.value++;
    }
    return len;
}

object Cyc_number2string(void *data, object cont, object n) {
    char buffer[1024];
    Cyc_check_num(data, n);
    if (type_of(n) == integer_tag) {
        snprintf(buffer, 1024, "%d", ((integer_type *)n)->value);
    } else if (type_of(n) == double_tag) {
        snprintf(buffer, 1024, "%lf", ((double_type *)n)->value);
    } else {
        Cyc_rt_raise2(data, "number->string - Unexpected object", n);
    }
    //make_string_noalloc(str, buffer, strlen(buffer));
    make_string(str, buffer);
    return_closcall1(data, cont, &str);
}

object Cyc_symbol2string(void *data, object cont, object sym) {
  Cyc_check_sym(data, sym);
  { const char *pname = symbol_pname(sym);
    make_string(str, pname);
    return_closcall1(data, cont, &str); }}

object Cyc_string2symbol(void *data, object str) {
    object sym;
    Cyc_check_str(data, str);
    sym = find_symbol_by_name(string_str(str));
    if (!sym) {
        sym = add_symbol_by_name(string_str(str));
    }
    return sym;
}

object Cyc_list2string(void *data, object cont, object lst){
    char *buf;
    int i = 0;
    integer_type len;

    Cyc_check_cons_or_nil(data, lst);
    
    len = Cyc_length(data, lst); // Inefficient, walks whole list
    buf = alloca(sizeof(char) * (len.value + 1));
    while(!nullp(lst)){
        buf[i++] = obj_obj2char(car(lst));
        lst = cdr(lst);
    }
    buf[i] = '\0';

    //{ make_string_noalloc(str, buf, i);
    { make_string(str, buf);
      return_closcall1(data, cont, &str);}
}

common_type Cyc_string2number(void *data, object str){
    common_type result;
    double n;
    Cyc_check_obj(data, string_tag, str);
    Cyc_check_str(data, str);
    if (type_of(str) == string_tag &&
        ((string_type *) str)->str){
        n = atof(((string_type *) str)->str);

        if (ceilf(n) == n) {
            result.integer_t.hdr.mark = gc_color_red;
            result.integer_t.hdr.grayed = 0;
            result.integer_t.tag = integer_tag;
            result.integer_t.value = (int)n;
        }
        else {
            result.double_t.hdr.mark = gc_color_red;
            result.double_t.hdr.grayed = 0;
            result.double_t.tag = double_tag;
            result.double_t.value = n;
        }
    } else {
        // TODO: not good enough because we do pointer comparisons to #f
        //result.boolean_t = boolean_f;
    }

    return result;
}

integer_type Cyc_string_cmp(void *data, object str1, object str2) {
  Cyc_check_str(data, str1);
  Cyc_check_str(data, str2);
  {
    make_int(cmp, strcmp(((string_type *)str1)->str,
                        ((string_type *)str2)->str));
    return cmp;
  }
}

#define Cyc_string_append_va_list(data, argc) { \
    int i = 0, total_len = 1; \
    int *len = alloca(sizeof(int) * argc); \
    char *buffer, *bufferp, **str = alloca(sizeof(char *) * argc); \
    object tmp; \
    if (argc > 0) { \
      Cyc_check_str(data, str1); \
      str[i] = ((string_type *)str1)->str; \
      len[i] = strlen(str[i]); \
      total_len += len[i]; \
    } \
    for (i = 1; i < argc; i++) { \
      tmp = va_arg(ap, object); \
      Cyc_check_str(data, tmp); \
      str[i] = ((string_type *)tmp)->str; \
      len[i] = strlen(str[i]); \
      total_len += len[i]; \
    } \
    buffer = bufferp = alloca(sizeof(char) * total_len); \
    for (i = 0; i < argc; i++) { \
        memcpy(bufferp, str[i], len[i]); \
        bufferp += len[i]; \
    } \
    *bufferp = '\0'; \
    make_string(result, buffer); \
    va_end(ap); \
    return_closcall1(data, cont, &result); \
}

void dispatch_string_91append(void *data, int _argc, object clo, object cont, object str1, ...) {
    va_list ap;
    va_start(ap, str1);
    Cyc_string_append_va_list(data, _argc - 1);
}

object Cyc_string_append(void *data, object cont, int _argc, object str1, ...) {
    va_list ap;
    va_start(ap, str1);
    Cyc_string_append_va_list(data, _argc);
}

integer_type Cyc_string_length(void *data, object str) {
  Cyc_check_obj(data, string_tag, str);
  Cyc_check_str(data, str);
  { make_int(len, strlen(string_str(str)));
    return len; }}

object Cyc_string_set(void *data, object str, object k, object chr) {
  char *raw;
  int idx, len;

  Cyc_check_str(data, str);
  Cyc_check_int(data, k);

  if (!eq(boolean_t, Cyc_is_char(chr))) {
    Cyc_rt_raise2(data, "Expected char but received", chr);
  }

  raw = string_str(str);
  idx = integer_value(k),
  len = strlen(raw);

  Cyc_check_bounds(data, "string-set!", len, idx);
  raw[idx] = obj_obj2char(chr);
  return str;
}

object Cyc_string_ref(void *data, object str, object k) {
  const char *raw;
  int idx, len;

  Cyc_check_str(data, str);
  Cyc_check_int(data, k);

  raw = string_str(str);
  idx = integer_value(k),
  len = strlen(raw);

  if (idx < 0 || idx >= len) {
    Cyc_rt_raise2(data, "string-ref - invalid index", k);
  }

  return obj_char2obj(raw[idx]);
}

object Cyc_substring(void *data, object cont, object str, object start, object end) {
  const char *raw;
  int s, e, len;

  Cyc_check_str(data, str);
  Cyc_check_int(data, start);
  Cyc_check_int(data, end);

  raw = string_str(str);
  s = integer_value(start),
  e = integer_value(end),
  len = strlen(raw);

  if (s > e) {
    Cyc_rt_raise2(data, "substring - start cannot be greater than end", start);
  }
  if (s > len) {
    Cyc_rt_raise2(data, "substring - start cannot be greater than string length", start);
  }
  if (e > len) {
    e = len;
  }

  {
    make_string_with_len(sub, raw + s, e - s);
    return_closcall1(data, cont, &sub);
  }
}

/**
 * Return directory where cyclone is installed.
 * This is configured via the makefile during a build.
 */
object Cyc_installation_dir(void *data, object cont, object type) {
  if (Cyc_is_symbol(type) == boolean_t &&
      strncmp(((symbol)type)->pname, "sld", 5) == 0) {
    char buf[1024];
    snprintf(buf, sizeof(buf), "%s", CYC_INSTALL_SLD);
    make_string(str, buf);
    return_closcall1(data, cont, &str);
  } else if (Cyc_is_symbol(type) == boolean_t &&
      strncmp(((symbol)type)->pname, "lib", 5) == 0) {
    char buf[1024];
    snprintf(buf, sizeof(buf), "%s", CYC_INSTALL_LIB);
    make_string(str, buf);
    return_closcall1(data, cont, &str);
  } else if (Cyc_is_symbol(type) == boolean_t &&
      strncmp(((symbol)type)->pname, "inc", 5) == 0) {
    char buf[1024];
    snprintf(buf, sizeof(buf), "%s", CYC_INSTALL_INC);
    make_string(str, buf);
    return_closcall1(data, cont, &str);
  } else {
    make_string(str, CYC_INSTALL_DIR);
    return_closcall1(data, cont, &str);
  }
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
object Cyc_command_line_arguments(void *data, object cont) {
  int i;
  object lis = nil;
  for (i = _cyc_argc; i > 1; i--) { // skip program name
    object ps = alloca(sizeof(string_type));
    object pl = alloca(sizeof(cons_type));
    make_string(s, _cyc_argv[i - 1]);
    memcpy(ps, &s, sizeof(string_type));
    ((list)pl)->hdr.mark = gc_color_red;
    ((list)pl)->hdr.grayed = 0;
    ((list)pl)->tag = cons_tag;
    ((list)pl)->cons_car = ps;
    ((list)pl)->cons_cdr = lis;
    lis = pl;
  }
  return_closcall1(data, cont, lis);
}

object Cyc_make_vector(void *data, object cont, object len, object fill) {
  object v = nil;
  int i;
  Cyc_check_int(data, len);
  v = alloca(sizeof(vector_type));
  ((vector)v)->hdr.mark = gc_color_red;
  ((vector)v)->hdr.grayed = 0;
  ((vector)v)->tag = vector_tag;
  ((vector)v)->num_elt = ((integer_type *)len)->value;
  ((vector)v)->elts = 
    (((vector)v)->num_elt > 0) ? 
     (object *)alloca(sizeof(object) * ((vector)v)->num_elt) : 
     NULL;
  for (i = 0; i < ((vector)v)->num_elt; i++) {
    ((vector)v)->elts[i] = fill;
  }
  return_closcall1(data, cont, v);
}

object Cyc_list2vector(void *data, object cont, object l) {
  object v = nil; 
  integer_type len;
  object lst = l; 
  int i = 0; 

  Cyc_check_cons_or_nil(data, l); 
  len = Cyc_length(data, l); 
  v = alloca(sizeof(vector_type)); 
  ((vector)v)->hdr.mark = gc_color_red;
  ((vector)v)->hdr.grayed = 0;
  ((vector)v)->tag = vector_tag; 
  ((vector)v)->num_elt = len.value; 
  ((vector)v)->elts = 
    (((vector)v)->num_elt > 0) ? 
       (object *)alloca(sizeof(object) * ((vector)v)->num_elt) : 
       NULL; 
  while(!nullp(lst)) {
    ((vector)v)->elts[i++] = car(lst); 
    lst = cdr(lst);
  }
  return_closcall1(data, cont, v);
}

integer_type Cyc_system(object cmd) {
  if (nullp(cmd) || is_value_type(cmd) || type_of(cmd) != string_tag) {
    make_int(n, -1);
    return n;
  } else {
    make_int(n, system(((string_type *)cmd)->str));
    return n;
  }
}

integer_type Cyc_char2integer(object chr){
    make_int(n, obj_obj2char(chr));
    return n;
}

object Cyc_integer2char(void *data, object n){
    int val = 0;

    Cyc_check_int(data, n);
    if (!nullp(n)) {
        val = ((integer_type *) n)->value;
    }

    return obj_char2obj(val);
}

void Cyc_halt(closure);
void Cyc_halt(env) closure env; {
#if DEBUG_SHOW_DIAG
    printf("Cyc_halt: heap bytes allocated=%d  time=%ld ticks  no_gcs=%ld no_m_gcs=%ld\n",
        allocp-bottom,clock()-start,no_gcs,no_major_gcs);
 printf("Cyc_halt: ticks/second=%ld\n",(long) CLOCKS_PER_SEC);
#endif
 exit(0);}

object __halt(object obj) {
    Cyc_halt(obj);
    return nil;
}

#define declare_num_op(FUNC, FUNC_OP, FUNC_APPLY, OP, DIV) \
common_type FUNC_OP(void *data, object x, object y) { \
    common_type s; \
    int tx = type_of(x), ty = type_of(y); \
    s.double_t.hdr.mark = gc_color_red; \
    s.double_t.hdr.grayed = 0; \
    s.double_t.tag = double_tag; \
    if (DIV &&  \
        ((ty == integer_tag && integer_value(y) == 0) || \
        (ty == double_tag && double_value(y) == 0.0))) { \
      Cyc_rt_raise_msg(data, "Divide by zero"); \
    } \
    if (tx == integer_tag && ty == integer_tag) { \
        s.integer_t.hdr.mark = gc_color_red; \
        s.integer_t.hdr.grayed = 0; \
        s.integer_t.tag = integer_tag; \
        s.integer_t.value = ((integer_type *)x)->value OP ((integer_type *)y)->value; \
    } else if (tx == double_tag && ty == integer_tag) { \
        s.double_t.value = ((double_type *)x)->value OP ((integer_type *)y)->value; \
    } else if (tx == integer_tag && ty == double_tag) { \
        s.double_t.value = ((integer_type *)x)->value OP ((double_type *)y)->value; \
    } else if (tx == double_tag && ty == double_tag) { \
        s.double_t.value = ((double_type *)x)->value OP ((double_type *)y)->value; \
    } else { \
        make_string(s, "Bad argument type"); \
        make_cons(c1, y, nil); \
        make_cons(c0, &s, &c1); \
        Cyc_rt_raise(data, &c0); \
    } \
    return s; \
} \
common_type FUNC(void *data, int argc, object n, ...) { \
    va_list ap; \
    va_start(ap, n); \
    common_type result = Cyc_num_op_va_list(data, argc, FUNC_OP, n, ap); \
    va_end(ap); \
    return result; \
} \
void FUNC_APPLY(void *data, int argc, object clo, object cont, object n, ...) { \
    va_list ap; \
    va_start(ap, n); \
    common_type result = Cyc_num_op_va_list(data, argc - 1, FUNC_OP, n, ap); \
    va_end(ap); \
    return_closcall1(data, cont, &result); \
}

declare_num_op(Cyc_sum, Cyc_sum_op, dispatch_sum, +, 0);
declare_num_op(Cyc_sub, Cyc_sub_op, dispatch_sub, -, 0);
declare_num_op(Cyc_mul, Cyc_mul_op, dispatch_mul, *, 0);
// TODO: what about divide-by-zero, and casting to double when
//       result contains a decimal component?
declare_num_op(Cyc_div, Cyc_div_op, dispatch_div, /, 1);

common_type Cyc_num_op_va_list(void *data, int argc, common_type (fn_op(void *, object, object)), object n, va_list ns) {
  common_type sum;
  int i;
  if (argc == 0) {
    sum.integer_t.hdr.mark = gc_color_red;
    sum.integer_t.hdr.grayed = 0;
    sum.integer_t.tag = integer_tag;
    sum.integer_t.value = 0;
    return sum;
  }

  if (type_of(n) == integer_tag) {
    sum.integer_t.hdr.mark = gc_color_red;
    sum.integer_t.hdr.grayed = 0; 
    sum.integer_t.tag = integer_tag;
    sum.integer_t.value = ((integer_type *)n)->value;
  } else if (type_of(n) == double_tag) {
    sum.double_t.hdr.mark = gc_color_red;
    sum.double_t.hdr.grayed = 0;
    sum.double_t.tag = double_tag;
    sum.double_t.value = ((double_type *)n)->value;
  } else {
      make_string(s, "Bad argument type");
      make_cons(c1, n, nil);
      make_cons(c0, &s, &c1);
      Cyc_rt_raise(data, &c0);
  }

  for (i = 1; i < argc; i++) {
    common_type result = fn_op(data, &sum, va_arg(ns, object));
    if (type_of(&result) == integer_tag) {
        sum.integer_t.hdr.mark = gc_color_red;
        sum.integer_t.hdr.grayed = 0;
        sum.integer_t.tag = integer_tag;
        sum.integer_t.value = ((integer_type *) &result)->value;
    } else if (type_of(&result) == double_tag) {
        sum.double_t.hdr.mark = gc_color_red;
        sum.double_t.hdr.grayed = 0;
        sum.double_t.tag = double_tag;
        sum.double_t.value = ((double_type *) &result)->value;
    } else {
        Cyc_rt_raise_msg(data, "Internal error, invalid tag in Cyc_num_op_va_list");
    }
  }

  return sum;
}

/* I/O functions */

port_type Cyc_stdout() {
  make_port(_stdout, stdout, 0);
  return _stdout;
}

port_type Cyc_stdin() {
    make_port(p, stdin, 1);
    return p;
}

port_type Cyc_stderr() {
    make_port(p, stderr, 0);
    return p;
}

port_type Cyc_io_open_input_file(void *data, object str) {
    const char *fname;
    Cyc_check_str(data, str);
    fname = ((string_type *)str)->str;
    make_port(p, NULL, 1);
    p.fp = fopen(fname, "r");
    if (p.fp == NULL) { Cyc_rt_raise2(data, "Unable to open file", str); }
    return p;
}

port_type Cyc_io_open_output_file(void *data, object str) {
    const char *fname;
    Cyc_check_str(data, str);
    fname = ((string_type *)str)->str;
    make_port(p, NULL, 0);
    p.fp = fopen(fname, "w");
    if (p.fp == NULL) { Cyc_rt_raise2(data, "Unable to open file", str); }
    return p;
}

object Cyc_io_close_input_port(void *data, object port) {
  return Cyc_io_close_port(data, port); }

object Cyc_io_close_output_port(void *data, object port) {
  return Cyc_io_close_port(data, port); }

object Cyc_io_close_port(void *data, object port) {
  Cyc_check_port(data, port);
  {
    FILE *stream = ((port_type *)port)->fp;
    if (stream) fclose(stream);
    ((port_type *)port)->fp = NULL;
  }
  return port;
}

object Cyc_io_flush_output_port(void *data, object port) {
  Cyc_check_port(data, port);
  {
    FILE *stream = ((port_type *)port)->fp;
    if (stream) { 
      int rv = fflush(stream);
      // TODO: handle error if non-zero value returned
    }
  }
  return port;
}

object Cyc_io_delete_file(void *data, object filename) {
  const char *fname;
  Cyc_check_str(data, filename);
  fname = ((string_type *)filename)->str;
  if (remove(fname) == 0)
    return boolean_t; // Success
  return boolean_f;
}

object Cyc_io_file_exists(void *data, object filename) {
  const char *fname;
  Cyc_check_str(data, filename);
  fname = ((string_type *)filename)->str;
  FILE *file;
  // Possibly overkill, but portable
  if (file = fopen(fname, "r")) {
    fclose(file);
    return boolean_t;
  }
  return boolean_f;
}

//  TODO: port arg is optional! (maybe handle that in expansion section??)
object Cyc_io_read_char(void *data, object cont, object port) {
    int c;
    Cyc_check_port(data, port);
    {
        gc_mutator_thread_blocked((gc_thread_data *)data, cont);
        c = fgetc(((port_type *) port)->fp);
        gc_mutator_thread_runnable(
          (gc_thread_data *)data, 
          (c != EOF) ? obj_char2obj(c) : Cyc_EOF);
        if (c != EOF) {
            return obj_char2obj(c);
        }
    }
    return Cyc_EOF;
}

/* TODO: this function needs some work, but approximates what is needed */
object Cyc_io_read_line(void *data, object cont, object port) {
  FILE *stream = ((port_type *)port)->fp;
  char buf[1024];
  int i = 0, c;
  
  gc_mutator_thread_blocked((gc_thread_data *)data, cont);
  while (1) {
    c = fgetc(stream);
    if (c == EOF && i == 0) {
      gc_mutator_thread_runnable((gc_thread_data *)data, Cyc_EOF);
      return_closcall1(data, cont, Cyc_EOF);
    } else if (c == EOF || i == 1023 || c == '\n') {
      buf[i] = '\0';
      {
        make_string(s, buf);
        gc_mutator_thread_runnable((gc_thread_data *)data, &s);
        return_closcall1(data, cont, &s);
      }
    }

    buf[i++] = c;
  }
  return nil;
}

object Cyc_io_peek_char(void *data, object cont, object port) {
    FILE *stream;
    int c;

    Cyc_check_port(data, port);
    {
        stream = ((port_type *) port)->fp;
        gc_mutator_thread_blocked((gc_thread_data *)data, cont);
        c = fgetc(stream);
        ungetc(c, stream);
        gc_mutator_thread_runnable(
          (gc_thread_data *)data, 
          (c != EOF) ? obj_char2obj(c) : Cyc_EOF);
        if (c != EOF) {
            return obj_char2obj(c);
        }
    }
    return Cyc_EOF;
}

/* This heap cons is used only for initialization. */
list mcons(a,d) object a,d;
{register cons_type *c = malloc(sizeof(cons_type));
 c->hdr.mark = gc_color_red;
 c->hdr.grayed = 0;
 c->tag = cons_tag; c->cons_car = a; c->cons_cdr = d;
 return c;}

cvar_type *mcvar(object *var) {
  cvar_type *c = malloc(sizeof(cvar_type));
  c->hdr.mark = gc_color_red;
  c->hdr.grayed = 0;
  c->tag = cvar_tag; 
  c->pvar = var;
  return c;}

void _Cyc_91global_91vars(void *data, object cont, object args){ 
    return_closcall1(data, cont, Cyc_global_variables); } 
void _car(void *data, object cont, object args) { 
    Cyc_check_num_args(data, "car", 1, args);
    { object var = car(args);
      Cyc_check_cons(data, var);
      return_closcall1(data, cont, car(var)); }}
void _cdr(void *data, object cont, object args) { 
    Cyc_check_num_args(data, "cdr", 1, args);
    Cyc_check_cons(data, car(args));
    return_closcall1(data, cont, cdr(car(args))); }
void _caar(void *data, object cont, object args) { 
    Cyc_check_num_args(data, "caar", 1, args);
    Cyc_check_cons(data, car(args));
    return_closcall1(data, cont, caar(car(args))); }
void _cadr(void *data, object cont, object args) { 
    Cyc_check_num_args(data, "cadr", 1, args);
    Cyc_check_cons(data, car(args));
    return_closcall1(data, cont, cadr(car(args))); }
void _cdar(void *data, object cont, object args) { 
    Cyc_check_num_args(data, "cdar", 1, args);
    Cyc_check_cons(data, car(args));
    return_closcall1(data, cont, cdar(car(args))); }
void _cddr(void *data, object cont, object args) { 
    Cyc_check_num_args(data, "cddr", 1, args);
    Cyc_check_cons(data, car(args));
    return_closcall1(data, cont, cddr(car(args))); }
void _caaar(void *data, object cont, object args) { 
    Cyc_check_num_args(data, "caaar", 1, args);
    Cyc_check_cons(data, car(args));
    return_closcall1(data, cont, caaar(car(args))); }
void _caadr(void *data, object cont, object args) { 
    Cyc_check_num_args(data, "caadr", 1, args);
    Cyc_check_cons(data, car(args));
    return_closcall1(data, cont, caadr(car(args))); }
void _cadar(void *data, object cont, object args) { 
    Cyc_check_num_args(data, "cadar", 1, args);
    Cyc_check_cons(data, car(args));
    return_closcall1(data, cont, cadar(car(args))); }
void _caddr(void *data, object cont, object args) { 
    Cyc_check_num_args(data, "caddr", 1, args);
    Cyc_check_cons(data, car(args));
    return_closcall1(data, cont, caddr(car(args))); }
void _cdaar(void *data, object cont, object args) { 
    Cyc_check_num_args(data, "cdaar", 1, args);
    Cyc_check_cons(data, car(args));
    return_closcall1(data, cont, cdaar(car(args))); }
void _cdadr(void *data, object cont, object args) { 
    Cyc_check_num_args(data, "cdadr", 1, args);
    Cyc_check_cons(data, car(args));
    return_closcall1(data, cont, cdadr(car(args))); }
void _cddar(void *data, object cont, object args) { 
    Cyc_check_num_args(data, "cddar", 1, args);
    Cyc_check_cons(data, car(args));
    return_closcall1(data, cont, cddar(car(args))); }
void _cdddr(void *data, object cont, object args) { 
    Cyc_check_num_args(data, "cdddr", 1, args);
    Cyc_check_cons(data, car(args));
    return_closcall1(data, cont, cdddr(car(args))); }
void _caaaar(void *data, object cont, object args) { 
    Cyc_check_num_args(data, "caaaar", 1, args);
    Cyc_check_cons(data, car(args));
    return_closcall1(data, cont, caaaar(car(args))); }
void _caaadr(void *data, object cont, object args) { 
    Cyc_check_num_args(data, "caaadr", 1, args);
    Cyc_check_cons(data, car(args));
    return_closcall1(data, cont, caaadr(car(args))); }
void _caadar(void *data, object cont, object args) { 
    Cyc_check_num_args(data, "caadar", 1, args);
    Cyc_check_cons(data, car(args));
    return_closcall1(data, cont, caadar(car(args))); }
void _caaddr(void *data, object cont, object args) { 
    Cyc_check_num_args(data, "caaddr", 1, args);
    Cyc_check_cons(data, car(args));
    return_closcall1(data, cont, caaddr(car(args))); }
void _cadaar(void *data, object cont, object args) { 
    Cyc_check_num_args(data, "cadaar", 1, args);
    Cyc_check_cons(data, car(args));
    return_closcall1(data, cont, cadaar(car(args))); }
void _cadadr(void *data, object cont, object args) { 
    Cyc_check_num_args(data, "cadadr", 1, args);
    Cyc_check_cons(data, car(args));
    return_closcall1(data, cont, cadadr(car(args))); }
void _caddar(void *data, object cont, object args) { 
    Cyc_check_num_args(data, "caddar", 1, args);
    Cyc_check_cons(data, car(args));
    return_closcall1(data, cont, caddar(car(args))); }
void _cadddr(void *data, object cont, object args) { 
    Cyc_check_num_args(data, "cadddr", 1, args);
    Cyc_check_cons(data, car(args));
    return_closcall1(data, cont, cadddr(car(args))); }
void _cdaaar(void *data, object cont, object args) { 
    Cyc_check_num_args(data, "cdaaar", 1, args);
    Cyc_check_cons(data, car(args));
    return_closcall1(data, cont, cdaaar(car(args))); }
void _cdaadr(void *data, object cont, object args) { 
    Cyc_check_num_args(data, "cdaadr", 1, args);
    Cyc_check_cons(data, car(args));
    return_closcall1(data, cont, cdaadr(car(args))); }
void _cdadar(void *data, object cont, object args) { 
    Cyc_check_num_args(data, "cdadar", 1, args);
    Cyc_check_cons(data, car(args));
    return_closcall1(data, cont, cdadar(car(args))); }
void _cdaddr(void *data, object cont, object args) { 
    Cyc_check_num_args(data, "cdaddr", 1, args);
    Cyc_check_cons(data, car(args));
    return_closcall1(data, cont, cdaddr(car(args))); }
void _cddaar(void *data, object cont, object args) { 
    Cyc_check_num_args(data, "cddaar", 1, args);
    Cyc_check_cons(data, car(args));
    return_closcall1(data, cont, cddaar(car(args))); }
void _cddadr(void *data, object cont, object args) { 
    Cyc_check_num_args(data, "cddadr", 1, args);
    Cyc_check_cons(data, car(args));
    return_closcall1(data, cont, cddadr(car(args))); }
void _cdddar(void *data, object cont, object args) { 
    Cyc_check_num_args(data, "cdddar", 1, args);
    Cyc_check_cons(data, car(args));
    return_closcall1(data, cont, cdddar(car(args))); }
void _cddddr(void *data, object cont, object args) { 
    Cyc_check_num_args(data, "cddddr", 1, args);
    Cyc_check_cons(data, car(args));
    return_closcall1(data, cont, cddddr(car(args))); }
void _cons(void *data, object cont, object args) { 
    Cyc_check_num_args(data, "cons", 2, args);
    { make_cons(c, car(args), cadr(args));
      return_closcall1(data, cont, &c); }}
void _eq_127(void *data, object cont, object args){ 
    Cyc_check_num_args(data, "eq?", 2, args);
    return_closcall1(data, cont, Cyc_eq(car(args), cadr(args))); }
void _eqv_127(void *data, object cont, object args){ 
    Cyc_check_num_args(data, "eqv?", 2, args);
    _eq_127(data, cont, args); }
void _equal_127(void *data, object cont, object args){ 
    Cyc_check_num_args(data, "equal?", 2, args);
    return_closcall1(data, cont, equalp(car(args), cadr(args))); }
void _length(void *data, object cont, object args){ 
    Cyc_check_num_args(data, "length", 1, args);
    { integer_type i = Cyc_length(data, car(args));
      return_closcall1(data, cont, &i); }}
void _vector_91length(void *data, object cont, object args){ 
    Cyc_check_num_args(data, "vector_91length", 1, args);
    { integer_type i = Cyc_vector_length(data, car(args));
      return_closcall1(data, cont, &i); }}
void _null_127(void *data, object cont, object args) { 
    Cyc_check_num_args(data, "null?", 1, args);
    return_closcall1(data, cont, Cyc_is_null(car(args))); }
void _set_91car_67(void *data, object cont, object args) { 
    Cyc_check_num_args(data, "set-car!", 2, args);
    return_closcall1(data, cont, Cyc_set_car(data, car(args), cadr(args))); }
void _set_91cdr_67(void *data, object cont, object args) { 
    Cyc_check_num_args(data, "set-cdr!", 2, args);
    return_closcall1(data, cont, Cyc_set_cdr(data, car(args), cadr(args))); }
void _Cyc_91has_91cycle_127(void *data, object cont, object args) { 
    Cyc_check_num_args(data, "Cyc-has-cycle?", 1, args);
    return_closcall1(data, cont, Cyc_has_cycle(car(args))); }
void _Cyc_91spawn_91thread_67(void *data, object cont, object args) {
    Cyc_check_num_args(data, "Cyc-spawn-thread!", 1, args);
    // TODO: validate argument type?
    return_closcall1(data, cont, Cyc_spawn_thread(car(args))); }
void _Cyc_91end_91thread_67(void *data, object cont, object args) {
    Cyc_end_thread((gc_thread_data *)data);
    return_closcall1(data, cont, boolean_f); }
void _thread_91sleep_67(void *data, object cont, object args) {
    Cyc_check_num_args(data, "thread-sleep!", 1, args);
    return_closcall1(data, cont, Cyc_thread_sleep(data, car(args))); }
void __87(void *data, object cont, object args) {
    integer_type argc = Cyc_length(data, args);
    dispatch(data, argc.value, (function_type)dispatch_sum, cont, cont, args); }
void __91(void *data, object cont, object args) {
    Cyc_check_num_args(data, "-", 1, args);
    { integer_type argc = Cyc_length(data, args);
      dispatch(data, argc.value, (function_type)dispatch_sub, cont, cont, args); }}
void __85(void *data, object cont, object args) {
    integer_type argc = Cyc_length(data, args);
    dispatch(data, argc.value, (function_type)dispatch_mul, cont, cont, args); }
void __95(void *data, object cont, object args) {
    Cyc_check_num_args(data, "/", 1, args);
    { integer_type argc = Cyc_length(data, args);
      dispatch(data, argc.value, (function_type)dispatch_div, cont, cont, args); }}
void _Cyc_91cvar_127(void *data, object cont, object args) {
    Cyc_check_num_args(data, "Cyc-cvar?", 1, args);
    return_closcall1(data, cont, Cyc_is_cvar(car(args))); }
void _boolean_127(void *data, object cont, object args) {
    Cyc_check_num_args(data, "boolean?", 1, args);
    return_closcall1(data, cont, Cyc_is_boolean(car(args))); }
void _char_127(void *data, object cont, object args) {
    Cyc_check_num_args(data, "char?", 1, args);
    return_closcall1(data, cont, Cyc_is_char(car(args))); }
void _eof_91object_127(void *data, object cont, object args) {
    Cyc_check_num_args(data, "eof_91object?", 1, args);
    return_closcall1(data, cont, Cyc_is_eof_object(car(args))); }
void _number_127(void *data, object cont, object args) {
    Cyc_check_num_args(data, "number?", 1, args);
    return_closcall1(data, cont, Cyc_is_number(car(args))); }
void _real_127(void *data, object cont, object args) {
    Cyc_check_num_args(data, "real?", 1, args);
    return_closcall1(data, cont, Cyc_is_real(car(args))); }
void _integer_127(void *data, object cont, object args) {
    Cyc_check_num_args(data, "integer?", 1, args);
    return_closcall1(data, cont, Cyc_is_integer(car(args))); }
void _pair_127(void *data, object cont, object args) {
    Cyc_check_num_args(data, "pair?", 1, args);
    return_closcall1(data, cont, Cyc_is_cons(car(args))); }
void _procedure_127(void *data, object cont, object args) {
    Cyc_check_num_args(data, "procedure?", 1, args);
    return_closcall1(data, cont, Cyc_is_procedure(data, car(args))); }
void _macro_127(void *data, object cont, object args) {
    Cyc_check_num_args(data, "macro?", 1, args);
    return_closcall1(data, cont, Cyc_is_macro(car(args))); }
void _port_127(void *data, object cont, object args) {
    Cyc_check_num_args(data, "port?", 1, args);
    return_closcall1(data, cont, Cyc_is_port(car(args))); }
void _vector_127(void *data, object cont, object args) {
    Cyc_check_num_args(data, "vector?", 1, args);
    return_closcall1(data, cont, Cyc_is_vector(car(args))); }
void _string_127(void *data, object cont, object args) {
    Cyc_check_num_args(data, "string?", 1, args);
    return_closcall1(data, cont, Cyc_is_string(car(args))); }
void _symbol_127(void *data, object cont, object args) {
    Cyc_check_num_args(data, "symbol?", 1, args);
    return_closcall1(data, cont, Cyc_is_symbol(car(args))); }

void _Cyc_91get_91cvar(void *data, object cont, object args) {  
    printf("not implemented\n"); exit(1); }
void _Cyc_91set_91cvar_67(void *data, object cont, object args) {  
    printf("not implemented\n"); exit(1); }
/* Note we cannot use _exit (per convention) because it is reserved by C */
void _cyc_exit(void *data, object cont, object args) {  
    if(nullp(args))
        __halt(nil);
    __halt(car(args));
}
void __75halt(void *data, object cont, object args) {  
    exit(0); }
void _cell_91get(void *data, object cont, object args) {  
    printf("not implemented\n"); exit(1); }
void _set_91global_67(void *data, object cont, object args) {  
    printf("not implemented\n"); exit(1); }
void _set_91cell_67(void *data, object cont, object args) {  
    printf("not implemented\n"); exit(1); }
void _cell(void *data, object cont, object args) {  
    printf("not implemented\n"); exit(1); }

void __123(void *data, object cont, object args) {  
    Cyc_check_num_args(data, "=", 2, args);
    return_closcall1(data, cont, __num_eq(data, car(args), cadr(args)));}
void __125(void *data, object cont, object args) {  
    Cyc_check_num_args(data, ">", 2, args);
    return_closcall1(data, cont, __num_gt(data, car(args), cadr(args)));}
void __121(void *data, object cont, object args) {
    Cyc_check_num_args(data, "<", 2, args);
    return_closcall1(data, cont, __num_lt(data, car(args), cadr(args)));}
void __125_123(void *data, object cont, object args) {
    Cyc_check_num_args(data, ">=", 2, args);
    return_closcall1(data, cont, __num_gte(data, car(args), cadr(args)));}
void __121_123(void *data, object cont, object args) {
    Cyc_check_num_args(data, "<=", 2, args);
    return_closcall1(data, cont, __num_lte(data, car(args), cadr(args)));}

void _apply(void *data, object cont, object args) {  
    Cyc_check_num_args(data, "apply", 2, args);
    apply(data, cont, car(args), cadr(args)); }
void _assoc (void *data, object cont, object args) {  
    Cyc_check_num_args(data, "assoc ", 2, args);
    return_closcall1(data, cont, assoc(data, car(args), cadr(args)));}
void _assq  (void *data, object cont, object args) {  
    Cyc_check_num_args(data, "assq  ", 2, args);
    return_closcall1(data, cont, assq(data, car(args), cadr(args)));}
void _assv  (void *data, object cont, object args) {  
    Cyc_check_num_args(data, "assv  ", 2, args);
    return_closcall1(data, cont, assq(data, car(args), cadr(args)));}
void _member(void *data, object cont, object args) {  
    Cyc_check_num_args(data, "member", 2, args);
    return_closcall1(data, cont, memberp(data, car(args), cadr(args)));}
void _memq(void *data, object cont, object args) {  
    Cyc_check_num_args(data, "memq", 2, args);
    return_closcall1(data, cont, memqp(data, car(args), cadr(args)));}
void _memv(void *data, object cont, object args) {  
    Cyc_check_num_args(data, "memv", 2, args);
    return_closcall1(data, cont, memqp(data, car(args), cadr(args)));}
void _char_91_125integer(void *data, object cont, object args) {  
    Cyc_check_num_args(data, "char->integer", 1, args);
    { integer_type i = Cyc_char2integer(car(args));
      return_closcall1(data, cont, &i);}}
void _integer_91_125char(void *data, object cont, object args) {  
    Cyc_check_num_args(data, "integer->char", 1, args);
    return_closcall1(data, cont, Cyc_integer2char(data, car(args)));}
void _string_91_125number(void *data, object cont, object args) {  
    Cyc_check_num_args(data, "string->number", 1, args);
    { common_type i = Cyc_string2number(data, car(args));
      return_closcall1(data, cont, &i);}}
void _string_91length(void *data, object cont, object args) {
    Cyc_check_num_args(data, "string-length", 1, args);
    { integer_type i = Cyc_string_length(data, car(args));
      return_closcall1(data, cont, &i);}}
void _cyc_substring(void *data, object cont, object args) {
    Cyc_check_num_args(data, "substring", 3, args);
    Cyc_substring(data, cont, car(args), cadr(args), caddr(args));}
void _cyc_string_91set_67(void *data, object cont, object args) {
    Cyc_check_num_args(data, "string-set!", 3, args);
    { object s = Cyc_string_set(data, car(args), cadr(args), caddr(args));
      return_closcall1(data, cont, s); }}
void _cyc_string_91ref(void *data, object cont, object args) {
    Cyc_check_num_args(data, "string-ref", 2, args);
    { object c = Cyc_string_ref(data, car(args), cadr(args));
      return_closcall1(data, cont, c); }}
void _Cyc_91installation_91dir(void *data, object cont, object args) {
    Cyc_check_num_args(data, "Cyc-installation-dir", 1, args);
    Cyc_installation_dir(data, cont, car(args));}
void _command_91line_91arguments(void *data, object cont, object args) {
    object cmdline = Cyc_command_line_arguments(data, cont);
    return_closcall1(data, cont, cmdline); }
void _cyc_system(void *data, object cont, object args) {
    Cyc_check_num_args(data, "system", 1, args);
    { integer_type i = Cyc_system(car(args));
      return_closcall1(data, cont, &i);}}
//void _error(void *data, object cont, object args) {
//    integer_type argc = Cyc_length(args);
//    dispatch_va(data, argc.value, dispatch_error, cont, cont, args); }
void _Cyc_91current_91exception_91handler(void *data, object cont, object args) {
    object handler = Cyc_current_exception_handler();
    return_closcall1(data, cont, handler); }
void _Cyc_91default_91exception_91handler(void *data, object cont, object args) {
    // TODO: this is a quick-and-dirty implementation, may be a better way to write this
    Cyc_default_exception_handler(data, 1, args, car(args));
}
void _string_91cmp(void *data, object cont, object args) {  
    Cyc_check_num_args(data, "string-cmp", 2, args);
    { integer_type cmp = Cyc_string_cmp(data, car(args), cadr(args));
      return_closcall1(data, cont, &cmp);}}
void _string_91append(void *data, object cont, object args) {  
    integer_type argc = Cyc_length(data, args);
    dispatch(data, argc.value, (function_type)dispatch_string_91append, cont, cont, args); }
void _make_91vector(void *data, object cont, object args) {
    Cyc_check_num_args(data, "make-vector", 1, args);
    { integer_type argc = Cyc_length(data, args);
      if (argc.value >= 2) {
        Cyc_make_vector(data, cont, car(args), cadr(args));}
      else {
        Cyc_make_vector(data, cont, car(args), boolean_f);}}}
void _vector_91ref(void *data, object cont, object args) {
    Cyc_check_num_args(data, "vector-ref", 2, args);
    { object ref = Cyc_vector_ref(data, car(args), cadr(args));
      return_closcall1(data, cont, ref);}}
void _vector_91set_67(void *data, object cont, object args) {
    Cyc_check_num_args(data, "vector-set!", 3, args);
    { object ref = Cyc_vector_set(data, car(args), cadr(args), caddr(args));
      return_closcall1(data, cont, ref);}}
void _list_91_125vector(void *data, object cont, object args) {
    Cyc_check_num_args(data, "list->vector", 1, args);
    Cyc_list2vector(data, cont, car(args));}
void _list_91_125string(void *data, object cont, object args) {  
    Cyc_check_num_args(data, "list->string", 1, args);
    Cyc_list2string(data, cont, car(args));}
void _string_91_125symbol(void *data, object cont, object args) {  
    Cyc_check_num_args(data, "string->symbol", 1, args);
    return_closcall1(data, cont, Cyc_string2symbol(data, car(args)));}
void _symbol_91_125string(void *data, object cont, object args) {  
    Cyc_check_num_args(data, "symbol->string", 1, args);
    Cyc_symbol2string(data, cont, car(args));}
void _number_91_125string(void *data, object cont, object args) {  
    Cyc_check_num_args(data, "number->string", 1, args);
    Cyc_number2string(data, cont, car(args));}
void _open_91input_91file(void *data, object cont, object args) {  
    Cyc_check_num_args(data, "open-input-file", 1, args);
    { port_type p = Cyc_io_open_input_file(data, car(args));
      return_closcall1(data, cont, &p);}}
void _open_91output_91file(void *data, object cont, object args) {  
    Cyc_check_num_args(data, "open-output-file", 1, args);
    { port_type p = Cyc_io_open_output_file(data, car(args));
      return_closcall1(data, cont, &p);}}
void _close_91port(void *data, object cont, object args) {  
    Cyc_check_num_args(data, "close-port", 1, args);
    return_closcall1(data, cont, Cyc_io_close_port(data, car(args)));}
void _close_91input_91port(void *data, object cont, object args) {  
    Cyc_check_num_args(data, "close-input-port", 1, args);
    return_closcall1(data, cont, Cyc_io_close_input_port(data, car(args)));}
void _close_91output_91port(void *data, object cont, object args) {  
    Cyc_check_num_args(data, "close-output-port", 1, args);
    return_closcall1(data, cont, Cyc_io_close_output_port(data, car(args)));}
void _Cyc_91flush_91output_91port(void *data, object cont, object args) {
    Cyc_check_num_args(data, "Cyc-flush-output-port", 1, args);
    return_closcall1(data, cont, Cyc_io_flush_output_port(data, car(args)));}
void _file_91exists_127(void *data, object cont, object args) {
    Cyc_check_num_args(data, "file-exists?", 1, args);
    return_closcall1(data, cont, Cyc_io_file_exists(data, car(args)));}
void _delete_91file(void *data, object cont, object args) {
    Cyc_check_num_args(data, "delete-file", 1, args);
    return_closcall1(data, cont, Cyc_io_delete_file(data, car(args)));}
void _read_91char(void *data, object cont, object args) {  
    Cyc_check_num_args(data, "read-char", 1, args);
    return_closcall1(data, cont, Cyc_io_read_char(data, cont, car(args)));}
void _peek_91char(void *data, object cont, object args) {  
    Cyc_check_num_args(data, "peek-char", 1, args);
    return_closcall1(data, cont, Cyc_io_peek_char(data, cont, car(args)));}
void _Cyc_91read_91line(void *data, object cont, object args) {  
    Cyc_check_num_args(data, "Cyc-read-line", 1, args);
    Cyc_io_read_line(data, cont, car(args));}
void _Cyc_91write_91char(void *data, object cont, object args) {  
    Cyc_check_num_args(data, "write-char", 2, args);
    return_closcall1(data, cont, Cyc_write_char(data, car(args), cadr(args)));}
void _Cyc_91write(void *data, object cont, object args) {  
    Cyc_check_num_args(data, "write", 1, args);
    { integer_type argc = Cyc_length(data, args);
     dispatch(data, argc.value, (function_type)dispatch_write_va, cont, cont, args); }}
void _display(void *data, object cont, object args) {  
    Cyc_check_num_args(data, "display", 1, args);
    { integer_type argc = Cyc_length(data, args);
      dispatch(data, argc.value, (function_type)dispatch_display_va, cont, cont, args); }}
void _call_95cc(void *data, object cont, object args){
    Cyc_check_num_args(data, "call/cc", 1, args);
    if (eq(boolean_f, Cyc_is_procedure(data, car(args)))) {
      Cyc_invalid_type_error(data, closure2_tag, car(args)); 
    }
    return_closcall2(data, __glo_call_95cc, cont, car(args));
}

/*
 * @param cont - Continuation for the function to call into
 * @param func - Function to execute
 * @param args - A list of arguments to the function
 */
object apply(void *data, object cont, object func, object args){
  common_type buf;

//printf("DEBUG apply: ");
//Cyc_display(args);
//printf("\n");
  if (!is_object_type(func)) {
     Cyc_rt_raise2(data, "Call of non-procedure: ", func);
  }

  // Causes problems...
  //Cyc_check_cons_or_nil(args);

  switch(type_of(func)) {
    case primitive_tag:
      // TODO: should probably check arg counts and error out if needed
      ((primitive_type *)func)->fn(data, cont, args);
      break;
    case macro_tag:
    case closure0_tag:
    case closure1_tag:
    case closure2_tag:
    case closure3_tag:
    case closure4_tag:
    case closureN_tag:
      buf.integer_t = Cyc_length(data, args);
      // TODO: validate number of args provided:
      Cyc_check_num_args(data, "<procedure>", ((closure)func)->num_args, args); // TODO: could be more efficient, eg: cyc_length(args) is called twice.
      dispatch(data, buf.integer_t.value, ((closure)func)->fn, func, cont, args);
      break;

    case cons_tag:
    {
      // TODO: should add more error checking here, make sure car(func) is a symbol
      object fobj = car(func);

      if (!is_object_type(fobj) || type_of(fobj) != symbol_tag) {
         Cyc_rt_raise2(data, "Call of non-procedure: ", func);
      } else if (strncmp(((symbol)fobj)->pname, "lambda", 7) == 0) {
          make_cons(c, func, args);
          //printf("JAE DEBUG, sending to eval: ");
          //Cyc_display(&c, stderr);
          ((closure)__glo_eval)->fn(data, 2, __glo_eval, cont, &c, nil);

      // TODO: would be better to compare directly against symbols here,
      //       but need a way of looking them up ahead of time.
      //       maybe a libinit() or such is required.
      } else if (strncmp(((symbol)fobj)->pname, "primitive", 10) == 0) {
          make_cons(c, cadr(func), args);
          ((closure)__glo_eval)->fn(data, 3, __glo_eval, cont, &c, nil);
      } else if (strncmp(((symbol)fobj)->pname, "procedure", 10) == 0) {
          make_cons(c, func, args);
          ((closure)__glo_eval)->fn(data, 3, __glo_eval, cont, &c, nil);
      } else {
          make_cons(c, func, args);
          Cyc_rt_raise2(data, "Unable to evaluate: ", &c);
      }
    }
      
    default:
      printf("Invalid object type %ld\n", type_of(func));
      exit(1);
  }
  return nil; // Never reached
}

// Version of apply meant to be called from within compiled code
void Cyc_apply(void *data, int argc, closure cont, object prim, ...){
    va_list ap;
    object tmp;
    int i;
    list args = alloca(sizeof(cons_type) * argc);
    
    va_start(ap, prim);

    for (i = 0; i < argc; i++) {
        tmp = va_arg(ap, object);
        args[i].hdr.mark = gc_color_red;
        args[i].hdr.grayed = 0;
        args[i].tag = cons_tag;
        args[i].cons_car = tmp;
        args[i].cons_cdr = (i == (argc-1)) ? nil : &args[i + 1];
    }
    //printf("DEBUG applying primitive to ");
    //Cyc_display((object)&args[0]);
    //printf("\n");

    va_end(ap);
    apply(data, cont, prim, (object)&args[0]);
}
// END apply

/* Extract args from given array, assuming cont is the first arg in buf */
void Cyc_apply_from_buf(void *data, int argc, object prim, object *buf) {
    list args;
    object cont;
    int i;

    if (argc == 0) {
      printf("Internal error in Cyc_apply_from_buf, argc is 0\n");
      exit(1);
    }

    args = alloca(sizeof(cons_type) * (argc - 1));
    cont = buf[0];
    
    for (i = 1; i < argc; i++) {
        args[i - 1].hdr.mark = gc_color_red;
        args[i - 1].hdr.grayed = 0;
        args[i - 1].tag = cons_tag;
        args[i - 1].cons_car = buf[i];
        args[i - 1].cons_cdr = (i == (argc-1)) ? nil : &args[i];
    }

    apply(data, cont, prim, (object)&args[0]);
}

/**
 * Start a thread's trampoline

TODO: should rename this function to make it more clear what is really going on

 */
void Cyc_start_thread(gc_thread_data *thd)
{
  /* Tank, load the jump program... */
  setjmp(*(thd->jmp_start));

#if DEBUG_GC
  printf("Done with GC\n");
#endif

  if (type_of(thd->gc_cont) == cons_tag || prim(thd->gc_cont)) {
    Cyc_apply_from_buf(thd, thd->gc_num_args, thd->gc_cont, thd->gc_args);
  } else {
    do_dispatch(thd, thd->gc_num_args, ((closure)(thd->gc_cont))->fn, thd->gc_cont, thd->gc_args);
  }

  printf("Internal error: should never have reached this line\n"); 
  exit(0);
}

// Mark globals as part of the tracing collector
// This is called by the collector thread
void gc_mark_globals()
{
#if GC_DEBUG_TRACE
  //fprintf(stderr, "(gc_mark_globals heap: %p size: %d)\n", h, (unsigned int)gc_heap_total_size(h));
  fprintf(stderr, "Cyc_global_variables %p\n", Cyc_global_variables);
#endif
  // Mark global variables
  gc_mark_black(Cyc_global_variables); // Internal global used by the runtime
                                       // Marking it ensures all glos are marked
  {
    list l = global_table;
    for(; !nullp(l); l = cdr(l)){
     cvar_type *c = (cvar_type *)car(l);
     object glo =  *(c->pvar);
     if (!nullp(glo)) {
#if GC_DEBUG_TRACE
       fprintf(stderr, "global pvar %p\n", glo);
#endif
       gc_mark_black(glo); // Mark actual object the global points to
     }
    }
  }
}

char *gc_fixup_moved_obj(gc_thread_data *thd, int *alloci, char *obj, object hp)
{
  if (grayed(obj)) {
    pthread_mutex_lock(&(thd->lock));
    gc_mark_gray2(thd, hp);
    pthread_mutex_unlock(&(thd->lock));
  }

  // hp ==> new heap object, point to it from old stack object
  forward(obj) = hp;
  type_of(obj) = forward_tag;
  // keep track of each allocation so we can scan/move 
  // the whole live object 'tree'
  gc_thr_add_to_move_buffer(thd, alloci, hp);
  return (char *)hp;
}

char *gc_move(char *obj, gc_thread_data *thd, int *alloci, int *heap_grown) {
  if (!is_object_type(obj)) return obj;
  switch(type_of(obj)){
    case cons_tag: {
      list hp = gc_alloc(Cyc_heap, sizeof(cons_type), obj, thd, heap_grown);
      return gc_fixup_moved_obj(thd, alloci, obj, hp);
    }
    case macro_tag: {
      macro_type *hp = gc_alloc(Cyc_heap, sizeof(macro_type), obj, thd, heap_grown);
      return gc_fixup_moved_obj(thd, alloci, obj, hp);
    }
    case closure0_tag: {
      closure0_type *hp = gc_alloc(Cyc_heap, sizeof(closure0_type), obj, thd, heap_grown);
      return gc_fixup_moved_obj(thd, alloci, obj, hp);
    }
    case closure1_tag: {
      closure1_type *hp = gc_alloc(Cyc_heap, sizeof(closure1_type), obj, thd, heap_grown);
      return gc_fixup_moved_obj(thd, alloci, obj, hp);
    }
    case closure2_tag: {
      closure2_type *hp = gc_alloc(Cyc_heap, sizeof(closure2_type), obj, thd, heap_grown);
      return gc_fixup_moved_obj(thd, alloci, obj, hp);
    }
    case closure3_tag: {
      closure3_type *hp = gc_alloc(Cyc_heap, sizeof(closure3_type), obj, thd, heap_grown);
      return gc_fixup_moved_obj(thd, alloci, obj, hp);
    }
    case closure4_tag: {
      closure4_type *hp = gc_alloc(Cyc_heap, sizeof(closure4_type), obj, thd, heap_grown);
      return gc_fixup_moved_obj(thd, alloci, obj, hp);
    }
    case closureN_tag: {
      closureN_type *hp = gc_alloc(Cyc_heap, 
                            sizeof(closureN_type) + sizeof(object) * (((closureN) obj)->num_elt), 
                            obj, thd, heap_grown);
      return gc_fixup_moved_obj(thd, alloci, obj, hp);
    }
    case vector_tag: {
      vector_type *hp = gc_alloc(Cyc_heap, 
                            sizeof(vector_type) + sizeof(object) * (((vector) obj)->num_elt), 
                            obj, thd, heap_grown);
      return gc_fixup_moved_obj(thd, alloci, obj, hp);
    }
    case string_tag: {
      string_type *hp = gc_alloc(Cyc_heap, 
        sizeof(string_type) + ((string_len(obj) + 1)), 
        obj, thd, heap_grown);
      return gc_fixup_moved_obj(thd, alloci, obj, hp);
    }
    case integer_tag: {
      integer_type *hp = gc_alloc(Cyc_heap, sizeof(integer_type), obj, thd, heap_grown);
      return gc_fixup_moved_obj(thd, alloci, obj, hp);
    }
    case double_tag: {
      double_type *hp = gc_alloc(Cyc_heap, sizeof(double_type), obj, thd, heap_grown);
      return gc_fixup_moved_obj(thd, alloci, obj, hp);
    }
    case port_tag: {
      port_type *hp = gc_alloc(Cyc_heap, sizeof(port_type), obj, thd, heap_grown);
      return gc_fixup_moved_obj(thd, alloci, obj, hp);
    }
    case cvar_tag: {
      cvar_type *hp = gc_alloc(Cyc_heap, sizeof(cvar_type), obj, thd, heap_grown);
      return gc_fixup_moved_obj(thd, alloci, obj, hp);
    }
    case forward_tag:
      return (char *)forward(obj);
    case eof_tag: break;
    case primitive_tag: break;
    case boolean_tag: break;
    case symbol_tag: break; // JAE TODO: raise an error here? Should not be possible in real code, though (IE, without GC DEBUG flag)
    default:
      fprintf(stderr, "gc_move: bad tag obj=%p obj.tag=%ld\n",(object) obj, type_of(obj));
      exit(1);
  }
  return (char *)obj;
}

#define gc_move2heap(obj) { \
  temp = obj; \
  if (check_overflow(low_limit, temp) && \
      check_overflow(temp, high_limit)){ \
    (obj) = (object) gc_move(temp, (gc_thread_data *)data, &alloci, &heap_grown); \
  } \
}

// Do a minor GC
int gc_minor(void *data, object low_limit, object high_limit, closure cont, object *args, int num_args)
{ 
  object temp;
  int i;
  int scani = 0, alloci = 0;
  int heap_grown = 0;

//fprintf(stdout, "DEBUG, started minor GC\n"); // JAE DEBUG
  // Prevent overrunning buffer
  if (num_args > NUM_GC_ANS) {
    printf("Fatal error - too many arguments (%d) to GC\n", num_args);
    exit(1);
  }

  gc_move2heap(cont);
  ((gc_thread_data *)data)->gc_cont = cont;
  ((gc_thread_data *)data)->gc_num_args = num_args;

  for (i = 0; i < num_args; i++){ 
    gc_move2heap(args[i]);
    ((gc_thread_data *)data)->gc_args[i] = args[i];
  }

  // Transport mutations
  {
    list l;
    for (l = mutation_table; !nullp(l); l = cdr(l)) {
      object o = car(l);
      if (type_of(o) == cons_tag) {
          gc_move2heap(car(o));
          gc_move2heap(cdr(o));
      } else if (type_of(o) == vector_tag) {
        int i;
        // TODO: probably too inefficient, try collecting single index
        for (i = 0; i < ((vector)o)->num_elt; i++) {
          gc_move2heap(((vector)o)->elts[i]);
        }
      } else if (type_of(o) == forward_tag) {
          // Already transported, skip
      } else {
          printf("Unexpected type %ld transporting mutation\n", type_of(o));
          exit(1);
      }
    }
  }
  clear_mutations(); // Reset for next time

  // Transport globals
  gc_move2heap(Cyc_global_variables); // Internal global used by the runtime
  {
    list l = global_table;
    for(; !nullp(l); l = cdr(l)){
      cvar_type *c = (cvar_type *)car(l);
      gc_move2heap(*(c->pvar)); // Transport underlying global, not the pvar
    }
  }

  // Check allocated objects, moving additional objects as needed
  while (scani < alloci) {
    object obj = ((gc_thread_data *)data)->moveBuf[scani];
    switch(type_of(obj)) {
      case cons_tag: {
        gc_move2heap(car(obj));
        gc_move2heap(cdr(obj));
        break;
      }
      case closure1_tag:
        gc_move2heap(((closure1) obj)->elt1);
        break;
      case closure2_tag:
        gc_move2heap(((closure2) obj)->elt1);
        gc_move2heap(((closure2) obj)->elt2);
      case closure3_tag:
        gc_move2heap(((closure3) obj)->elt1);
        gc_move2heap(((closure3) obj)->elt2);
        gc_move2heap(((closure3) obj)->elt3);
      case closure4_tag:
        gc_move2heap(((closure4) obj)->elt1);
        gc_move2heap(((closure4) obj)->elt2);
        gc_move2heap(((closure4) obj)->elt3);
        gc_move2heap(((closure4) obj)->elt4);
        break;
      case closureN_tag: {
        int i, n = ((closureN) obj)->num_elt;
        for (i = 0; i < n; i++) {
          gc_move2heap(((closureN) obj)->elts[i]);
        }
        break;
      }
      case vector_tag: {
        int i, n = ((vector) obj)->num_elt;
        for (i = 0; i < n; i++) {
          gc_move2heap(((vector) obj)->elts[i]);
        }
        break;
      }
      // No child objects to move
      case closure0_tag:
      case macro_tag:
      case string_tag:
      case integer_tag:
      case double_tag:
      case port_tag:
      case cvar_tag:
        break;
      // These types are not heap-allocated
      case eof_tag:
      case primitive_tag:
      case symbol_tag: 
      case boolean_tag:
      default:
        fprintf(stderr, 
          "GC: unexpected object type %ld for object %p\n", type_of(obj), obj);
        exit(1);
    }
    scani++;
  }
  return alloci;
}

/**
 * Run a minor GC from a mutator thread.
 * This function runs the core GC algorithm, cooperates with
 * the collector, and then calls its continuation.
 */
void GC(void *data, closure cont, object *args, int num_args)
{ 
  char tmp;
  object low_limit = &tmp; // This is one end of the stack...
  object high_limit = ((gc_thread_data *)data)->stack_start;
  int alloci = gc_minor(data, low_limit, high_limit, cont, args, num_args);
  // Cooperate with the collector thread
  gc_mut_cooperate((gc_thread_data *)data, alloci);
#if GC_DEBUG_TRACE
  fprintf(stderr, "done with minor GC\n");
#endif
  // Let it all go, Neo...
  longjmp(*(((gc_thread_data *)data)->jmp_start), 1);
}


 /* Overall GC notes:
 note fwd pointers are only ever placed on the stack, never the heap
 
 we now have 2 GC's:
 - Stack GC, a minor collection where we move live stack objs to heap
 - Heap GC, a major collection where we do mark&sweep

 when replacing an object,
 - only need to do this for objects on 'this' stack
 - if object is a fwd pointer, return it's forwarding address
 - otherwise, 
   * allocate them on the heap
   * return the new address
   * leave a forwarding pointer on the stack with the new address
 - may be able to modify transp macro to do this part

 can still use write buffer to ensure any heap->stack references are handled
 - also want to use this barrier to handle any globals that are re-assigned to 
   locations on the stack, to ensure they are moved to the heap during GC.
 - write barrier really should be per-stack, since OK to leave those items until
   stack is collected
 - TBD how this works with multiple threads, each with its own stack

 need to transport:
 - stack closure/args
 - mutation write barrier
 - globals

 after transport is complete, we will not be scanning newspace but
 do need to transport any stack objects referenced by the above
 a couple of ideas:
 - create a list of allocated objects, and pass over them in much
   the same way the cheney algorithm does (2 "fingers"??). I think
   this could actually just be a list of pointers since we want to
   copy to the heap not the scan space. the goal is just to ensure
   all live stack references are moved to the heap. trick here is to 
   ensure scan space is large enough, although if it runs out
   we can just allocate a new space (of say double the size), 
   memcpy the old one, and update scanp/allocp accordingly.
   * can use a bump pointer to build the list, so it should be
     fairly efficient, especially if we don't have to resize too much
   * will be writing all of this code from scratch, but can use
     existing scan code as a guide
 - or, during transport recursively transport objects that could
   contain references (closures, lists, etc). This may be more
   convenient to code, although it requires stack space to traverse
   the structures. I think it might also get stuck processing circular
   structures (!!!), so this approach is not an option
 TBD how (or even if) this can scale to multiple threads...
 is is possible to use write barrier(s) to detect if one thread is
 working with another's data during GC? This will be an important
 point to keep in mind as the code is being written

!!!
IMPORTANT - does the timing of GC matter? for example, if we GC before
scanning all the stack space, there might be an object referenced by
a live stack object that would get freed because we haven't gotten to
it yet!

so I think we have to scan all the stack space before doing a GC.
alternatively, can we use a write barrier to keep track of when a
stack object references one on the heap? that would effectively make
the heap object into a root until stack GC

Originally thought this, but now not so sure because it seems the above
has to be taken into account:

 Do not have to explicitly GC until heap is full enough for one to 
 be initiated. do need to code gc_collect though, and ensure it is
 called at the appropriate time.

I think everything else will work as written, but not quite sure how
to handle this detail yet. and it is very important to get right
!!!!

 thoughts:
 - worth having a write barrier for globals? that is, only GC those that
   were modified. just an idea...
 - KEEP IN MIND AN OVERALL GOAL, that this should try to be as close as
   possible to the cheney algorithm in terms of performance. so obviously we 
   want to try and do as little work as necessary during each minor GC.
   since we will use a write barrier to keep track of the heap's stack refs,
   it seems reasonable that we could skip globals that live on the heap.
 - To some extent, it should be possible to test changes that improve performance 
   by coding something inefficient (but guaranteed to work) and then modifying it to
   be more efficient (but not quite sure if idea will work).
 */

/**
 * Receive a list of arguments and apply them to the given function
 */
void dispatch(void *data, int argc, function_type func, object clo, object cont, object args) {
  object b[argc + 1]; // OK to do this? Is this portable?
  int i; 

  argc++;
  b[0] = cont;
  for (i = 1; i < argc; i++){ 
    b[i] = car(args); 
    args = cdr(args); 
  } 

  do_dispatch(data, argc, func, clo, b);
}

/**
 * Same as above but for a varargs C function
 */
void dispatch_va(void *data, int argc, function_type_va func, object clo, object cont, object args) {
  object b[argc + 1]; // OK to do this? Is this portable?
  int i; 
 
  argc++;
  b[0] = cont;
  for (i = 1; i < argc; i++){ 
    b[i] = car(args); 
    args = cdr(args); 
  } 

  do_dispatch(data, argc, (function_type)func, clo, b);
}

static primitive_type Cyc_91global_91vars_primitive = {{0}, primitive_tag, "Cyc-global-vars", &_Cyc_91global_91vars};
static primitive_type Cyc_91get_91cvar_primitive = {{0}, primitive_tag, "Cyc-get-cvar", &_Cyc_91get_91cvar};
static primitive_type Cyc_91set_91cvar_67_primitive = {{0}, primitive_tag, "Cyc-set-cvar!", &_Cyc_91set_91cvar_67};
static primitive_type Cyc_91cvar_127_primitive = {{0}, primitive_tag, "Cyc-cvar?", &_Cyc_91cvar_127};
static primitive_type Cyc_91has_91cycle_127_primitive = {{0}, primitive_tag, "Cyc-has-cycle?", &_Cyc_91has_91cycle_127};
static primitive_type Cyc_91spawn_91thread_67_primitive = {{0}, primitive_tag, "Cyc-spawn-thread!", &_Cyc_91spawn_91thread_67};
static primitive_type Cyc_91end_91thread_67_primitive = {{0}, primitive_tag, "Cyc-end-thread!", &_Cyc_91end_91thread_67};
static primitive_type thread_91sleep_67_primitive = {{0}, primitive_tag, "thread-sleep!", &_thread_91sleep_67};
static primitive_type _87_primitive = {{0}, primitive_tag, "+", &__87};
static primitive_type _91_primitive = {{0}, primitive_tag, "-", &__91};
static primitive_type _85_primitive = {{0}, primitive_tag, "*", &__85};
static primitive_type _95_primitive = {{0}, primitive_tag, "/", &__95};
static primitive_type _123_primitive = {{0}, primitive_tag, "=", &__123};
static primitive_type _125_primitive = {{0}, primitive_tag, ">", &__125};
static primitive_type _121_primitive = {{0}, primitive_tag, "<", &__121};
static primitive_type _125_123_primitive = {{0}, primitive_tag, ">=", &__125_123};
static primitive_type _121_123_primitive = {{0}, primitive_tag, "<=", &__121_123};
static primitive_type apply_primitive = {{0}, primitive_tag, "apply", &_apply};
static primitive_type _75halt_primitive = {{0}, primitive_tag, "%halt", &__75halt};
static primitive_type exit_primitive = {{0}, primitive_tag, "exit", &_cyc_exit};
static primitive_type Cyc_91current_91exception_91handler_primitive = {{0}, primitive_tag, "Cyc_current_exception_handler", &_Cyc_91current_91exception_91handler};
static primitive_type Cyc_91default_91exception_91handler_primitive = {{0}, primitive_tag, "Cyc_default_exception_handler", &_Cyc_91default_91exception_91handler};
static primitive_type cons_primitive = {{0}, primitive_tag, "cons", &_cons};
static primitive_type cell_91get_primitive = {{0}, primitive_tag, "cell-get", &_cell_91get};
static primitive_type set_91global_67_primitive = {{0}, primitive_tag, "set-global!", &_set_91global_67};
static primitive_type set_91cell_67_primitive = {{0}, primitive_tag, "set-cell!", &_set_91cell_67};
static primitive_type cell_primitive = {{0}, primitive_tag, "cell", &_cell};
static primitive_type eq_127_primitive = {{0}, primitive_tag, "eq?", &_eq_127};
static primitive_type eqv_127_primitive = {{0}, primitive_tag, "eqv?", &_eqv_127};
static primitive_type equal_127_primitive = {{0}, primitive_tag, "equal?", &_equal_127};
static primitive_type assoc_primitive = {{0}, primitive_tag, "assoc", &_assoc};
static primitive_type assq_primitive = {{0}, primitive_tag, "assq", &_assq};
static primitive_type assv_primitive = {{0}, primitive_tag, "assv", &_assv};
static primitive_type member_primitive = {{0}, primitive_tag, "member", &_member};
static primitive_type memq_primitive = {{0}, primitive_tag, "memq", &_memq};
static primitive_type memv_primitive = {{0}, primitive_tag, "memv", &_memv};
static primitive_type length_primitive = {{0}, primitive_tag, "length", &_length};
static primitive_type vector_91length_primitive = {{0}, primitive_tag, "vector-length", &_vector_91length};
static primitive_type set_91car_67_primitive = {{0}, primitive_tag, "set-car!", &_set_91car_67};
static primitive_type set_91cdr_67_primitive = {{0}, primitive_tag, "set-cdr!", &_set_91cdr_67};
static primitive_type car_primitive = {{0}, primitive_tag, "car", &_car};
static primitive_type cdr_primitive = {{0}, primitive_tag, "cdr", &_cdr};
static primitive_type caar_primitive = {{0}, primitive_tag, "caar", &_caar};
static primitive_type cadr_primitive = {{0}, primitive_tag, "cadr", &_cadr};
static primitive_type cdar_primitive = {{0}, primitive_tag, "cdar", &_cdar};
static primitive_type cddr_primitive = {{0}, primitive_tag, "cddr", &_cddr};
static primitive_type caaar_primitive = {{0}, primitive_tag, "caaar", &_caaar};
static primitive_type caadr_primitive = {{0}, primitive_tag, "caadr", &_caadr};
static primitive_type cadar_primitive = {{0}, primitive_tag, "cadar", &_cadar};
static primitive_type caddr_primitive = {{0}, primitive_tag, "caddr", &_caddr};
static primitive_type cdaar_primitive = {{0}, primitive_tag, "cdaar", &_cdaar};
static primitive_type cdadr_primitive = {{0}, primitive_tag, "cdadr", &_cdadr};
static primitive_type cddar_primitive = {{0}, primitive_tag, "cddar", &_cddar};
static primitive_type cdddr_primitive = {{0}, primitive_tag, "cdddr", &_cdddr};
static primitive_type caaaar_primitive = {{0}, primitive_tag, "caaaar", &_caaaar};
static primitive_type caaadr_primitive = {{0}, primitive_tag, "caaadr", &_caaadr};
static primitive_type caadar_primitive = {{0}, primitive_tag, "caadar", &_caadar};
static primitive_type caaddr_primitive = {{0}, primitive_tag, "caaddr", &_caaddr};
static primitive_type cadaar_primitive = {{0}, primitive_tag, "cadaar", &_cadaar};
static primitive_type cadadr_primitive = {{0}, primitive_tag, "cadadr", &_cadadr};
static primitive_type caddar_primitive = {{0}, primitive_tag, "caddar", &_caddar};
static primitive_type cadddr_primitive = {{0}, primitive_tag, "cadddr", &_cadddr};
static primitive_type cdaaar_primitive = {{0}, primitive_tag, "cdaaar", &_cdaaar};
static primitive_type cdaadr_primitive = {{0}, primitive_tag, "cdaadr", &_cdaadr};
static primitive_type cdadar_primitive = {{0}, primitive_tag, "cdadar", &_cdadar};
static primitive_type cdaddr_primitive = {{0}, primitive_tag, "cdaddr", &_cdaddr};
static primitive_type cddaar_primitive = {{0}, primitive_tag, "cddaar", &_cddaar};
static primitive_type cddadr_primitive = {{0}, primitive_tag, "cddadr", &_cddadr};
static primitive_type cdddar_primitive = {{0}, primitive_tag, "cdddar", &_cdddar};
static primitive_type cddddr_primitive = {{0}, primitive_tag, "cddddr", &_cddddr};
static primitive_type char_91_125integer_primitive = {{0}, primitive_tag, "char->integer", &_char_91_125integer};
static primitive_type integer_91_125char_primitive = {{0}, primitive_tag, "integer->char", &_integer_91_125char};
static primitive_type string_91_125number_primitive = {{0}, primitive_tag, "string->number", &_string_91_125number};
static primitive_type string_91length_primitive = {{0}, primitive_tag, "string-length", &_string_91length};
static primitive_type substring_primitive = {{0}, primitive_tag, "substring", &_cyc_substring};
static primitive_type string_91ref_primitive = {{0}, primitive_tag, "string-ref", &_cyc_string_91ref};
static primitive_type string_91set_67_primitive = {{0}, primitive_tag, "string-set!", &_cyc_string_91set_67};
static primitive_type Cyc_91installation_91dir_primitive = {{0}, primitive_tag, "Cyc-installation-dir", &_Cyc_91installation_91dir};
static primitive_type command_91line_91arguments_primitive = {{0}, primitive_tag, "command-line-arguments", &_command_91line_91arguments};
static primitive_type system_primitive = {{0}, primitive_tag, "system", &_cyc_system};
static primitive_type string_91cmp_primitive = {{0}, primitive_tag, "string-cmp", &_string_91cmp};
static primitive_type string_91append_primitive = {{0}, primitive_tag, "string-append", &_string_91append};
static primitive_type list_91_125string_primitive = {{0}, primitive_tag, "list->string", &_list_91_125string};
static primitive_type string_91_125symbol_primitive = {{0}, primitive_tag, "string->symbol", &_string_91_125symbol};
static primitive_type symbol_91_125string_primitive = {{0}, primitive_tag, "symbol->string", &_symbol_91_125string};
static primitive_type number_91_125string_primitive = {{0}, primitive_tag, "number->string", &_number_91_125string};
static primitive_type list_91_125vector_primitive = {{0}, primitive_tag, "list-vector", &_list_91_125vector};
static primitive_type make_91vector_primitive = {{0}, primitive_tag, "make-vector", &_make_91vector};
static primitive_type vector_91ref_primitive = {{0}, primitive_tag, "vector-ref", &_vector_91ref};
static primitive_type vector_91set_67_primitive = {{0}, primitive_tag, "vector-set!", &_vector_91set_67};
static primitive_type boolean_127_primitive = {{0}, primitive_tag, "boolean?", &_boolean_127};
static primitive_type char_127_primitive = {{0}, primitive_tag, "char?", &_char_127};
static primitive_type eof_91object_127_primitive = {{0}, primitive_tag, "eof-object?", &_eof_91object_127};
static primitive_type null_127_primitive = {{0}, primitive_tag, "null?", &_null_127};
static primitive_type number_127_primitive = {{0}, primitive_tag, "number?", &_number_127};
static primitive_type real_127_primitive = {{0}, primitive_tag, "real?", &_real_127};
static primitive_type integer_127_primitive = {{0}, primitive_tag, "integer?", &_integer_127};
static primitive_type pair_127_primitive = {{0}, primitive_tag, "pair?", &_pair_127};
static primitive_type procedure_127_primitive = {{0}, primitive_tag, "procedure?", &_procedure_127};
static primitive_type macro_127_primitive = {{0}, primitive_tag, "macro?", &_macro_127};
static primitive_type port_127_primitive = {{0}, primitive_tag, "port?", &_port_127};
static primitive_type vector_127_primitive = {{0}, primitive_tag, "vector?", &_vector_127};
static primitive_type string_127_primitive = {{0}, primitive_tag, "string?", &_string_127};
static primitive_type symbol_127_primitive = {{0}, primitive_tag, "symbol?", &_symbol_127};
static primitive_type open_91input_91file_primitive = {{0}, primitive_tag, "open-input-file", &_open_91input_91file};
static primitive_type open_91output_91file_primitive = {{0}, primitive_tag, "open-output-file", &_open_91output_91file};
static primitive_type close_91port_primitive = {{0}, primitive_tag, "close-port", &_close_91port};
static primitive_type close_91input_91port_primitive = {{0}, primitive_tag, "close-input-port", &_close_91input_91port};
static primitive_type close_91output_91port_primitive = {{0}, primitive_tag, "close-output-port", &_close_91output_91port};
static primitive_type Cyc_91flush_91output_91port_primitive = {{0}, primitive_tag, "Cyc-flush-output-port", &_Cyc_91flush_91output_91port};
static primitive_type file_91exists_127_primitive = {{0}, primitive_tag, "file-exists?", &_file_91exists_127};
static primitive_type delete_91file_primitive = {{0}, primitive_tag, "delete-file", &_delete_91file};
static primitive_type read_91char_primitive = {{0}, primitive_tag, "read-char", &_read_91char};
static primitive_type peek_91char_primitive = {{0}, primitive_tag, "peek-char", &_peek_91char};
static primitive_type Cyc_91read_91line_primitive = {{0}, primitive_tag, "Cyc-read-line", &_Cyc_91read_91line};
static primitive_type Cyc_91write_primitive = {{0}, primitive_tag, "Cyc-write", &_Cyc_91write};
static primitive_type Cyc_91write_91char_primitive = {{0}, primitive_tag, "Cyc-write-char", &_Cyc_91write_91char};
static primitive_type Cyc_91display_primitive = {{0}, primitive_tag, "Cyc-display", &_display};
static primitive_type call_95cc_primitive = {{0}, primitive_tag, "call/cc", &_call_95cc};

const object primitive_Cyc_91global_91vars = &Cyc_91global_91vars_primitive;
const object primitive_Cyc_91get_91cvar = &Cyc_91get_91cvar_primitive;
const object primitive_Cyc_91set_91cvar_67 = &Cyc_91set_91cvar_67_primitive;
const object primitive_Cyc_91cvar_127 = &Cyc_91cvar_127_primitive;
const object primitive_Cyc_91has_91cycle_127 = &Cyc_91has_91cycle_127_primitive;
const object primitive_Cyc_91spawn_91thread_67 = &Cyc_91spawn_91thread_67_primitive;
const object primitive_Cyc_91end_91thread_67 = &Cyc_91end_91thread_67_primitive;
const object primitive_thread_91sleep_67 = &thread_91sleep_67_primitive;
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
const object primitive_Cyc_91current_91exception_91handler = &Cyc_91current_91exception_91handler_primitive;
const object primitive_Cyc_91default_91exception_91handler = &Cyc_91default_91exception_91handler_primitive;
const object primitive_cons = &cons_primitive;
const object primitive_cell_91get = &cell_91get_primitive;
const object primitive_set_91global_67 = &set_91global_67_primitive;
const object primitive_set_91cell_67 = &set_91cell_67_primitive;
const object primitive_cell = &cell_primitive;
const object primitive_eq_127 = &eq_127_primitive;
const object primitive_eqv_127 = &eqv_127_primitive;
const object primitive_equal_127 = &equal_127_primitive;
const object primitive_assoc = &assoc_primitive;
const object primitive_assq = &assq_primitive;
const object primitive_assv = &assv_primitive;
const object primitive_member = &member_primitive;
const object primitive_memq = &memq_primitive;
const object primitive_memv = &memv_primitive;
const object primitive_length = &length_primitive;
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
const object primitive_Cyc_91installation_91dir = &Cyc_91installation_91dir_primitive;
const object primitive_command_91line_91arguments = &command_91line_91arguments_primitive;
const object primitive_system = &system_primitive;
const object primitive_string_91cmp = &string_91cmp_primitive;
const object primitive_string_91append = &string_91append_primitive;
const object primitive_list_91_125string = &list_91_125string_primitive;
const object primitive_string_91_125symbol = &string_91_125symbol_primitive;
const object primitive_symbol_91_125string = &symbol_91_125string_primitive;
const object primitive_number_91_125string = &number_91_125string_primitive;
const object primitive_make_91vector = &make_91vector_primitive;
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
const object primitive_string_127 = &string_127_primitive;
const object primitive_port_127 = &port_127_primitive;
const object primitive_vector_127 = &vector_127_primitive;
const object primitive_symbol_127 = &symbol_127_primitive;
const object primitive_open_91input_91file = &open_91input_91file_primitive;
const object primitive_open_91output_91file = &open_91output_91file_primitive;
const object primitive_close_91port = &close_91port_primitive;
const object primitive_close_91input_91port = &close_91input_91port_primitive;
const object primitive_close_91output_91port = &close_91output_91port_primitive;
const object primitive_Cyc_91flush_91output_91port = &Cyc_91flush_91output_91port_primitive;
const object primitive_file_91exists_127 = &file_91exists_127_primitive;
const object primitive_delete_91file = &delete_91file_primitive;
const object primitive_read_91char = &read_91char_primitive;
const object primitive_peek_91char = &peek_91char_primitive;
const object primitive_Cyc_91read_91line = &Cyc_91read_91line_primitive;
const object primitive_Cyc_91write_91char = &Cyc_91write_91char_primitive;
const object primitive_Cyc_91write = &Cyc_91write_primitive;
const object primitive_Cyc_91display = &Cyc_91display_primitive;
const object primitive_call_95cc = &call_95cc_primitive;

/**
 * Thread initialization function only called from within the runtime
 */
void *Cyc_init_thread(object thunk)
{
  long stack_start;
  gc_thread_data *thd;
  thd = malloc(sizeof(gc_thread_data));
  gc_thread_data_init(thd, 0, (char *) &stack_start, global_stack_size);
  thd->gc_cont = thunk;
  thd->gc_num_args = 1;
  thd->gc_args[0] = &Cyc_91end_91thread_67_primitive;
//  thd->thread = pthread_self(); // TODO: ptr vs instance
//  returns instance so would need to malloc here
//  would also need to update termination code to free that memory
  gc_add_mutator(thd);
  ATOMIC_SET_IF_EQ(&(thd->thread_state), CYC_THREAD_STATE_NEW, CYC_THREAD_STATE_RUNNABLE);
  Cyc_start_thread(thd);
  return NULL;
}

/**
 * Spawn a new thread to execute the given thunk
 */
object Cyc_spawn_thread(object thunk)
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
  if (pthread_create(&thread, NULL, Cyc_init_thread, thunk)) {
    fprintf(stderr, "Error creating a new thread\n");
    exit(1);
  }
  return boolean_t;
}

/**
 * Terminate a thread
 */
void Cyc_end_thread(gc_thread_data *thd) 
{
  // TODO: should we consider passing the current continuation (and args)
  // as an argument? if we don't, will objects be collected that are still
  // being used by active threads??
  mclosure0(clo, Cyc_exit_thread);
  GC(thd, &clo, thd->gc_args, 0);
}

void Cyc_exit_thread(gc_thread_data *thd)
{
  // alternatively could call longjmp with a null continuation, but that seems
  // more complicated than necessary. or does it... see next comment:
  
  // TODO: what if there are any locals from the thread's stack still being
  // referenced? might want to do one more minor GC to clear the stack before
  // terminating the thread

//printf("DEBUG - exiting thread\n");
  // Remove thread from the list of mutators, and mark its data to be freed
  gc_remove_mutator(thd);
  ATOMIC_SET_IF_EQ(&(thd->thread_state), CYC_THREAD_STATE_RUNNABLE, CYC_THREAD_STATE_TERMINATED);
  pthread_exit(NULL); // For now, just a proof of concept
}

// For now, accept a number of milliseconds to sleep
object Cyc_thread_sleep(void *data, object timeout)
{
  struct timespec tim;
  long value;
  Cyc_check_num(data, timeout);
  value = ((integer_type *)timeout)->value;
  tim.tv_sec = value / 1000;
  tim.tv_nsec = (value % 1000) * NANOSECONDS_PER_MILLISECOND;
  nanosleep(&tim, NULL);
  return boolean_t;
}


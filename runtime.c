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
#include <ck_pr.h>
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
const char *tag_names[23] = { \
   "pair" \
 , "symbol" \
 , "" \
 , "procedure" \
 , "procedure" \
 , "" \
 , "" \
 , "" \
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
 , "mutex" \
 , "condition variable" \
 , "bytevector" \
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
#define closcall1(td,clo,a1) if (type_of(clo) == cons_tag || prim(clo)) { Cyc_apply(td,0, (closure)(a1), clo); } else { ((clo)->fn)(td,1,clo,a1);}
/* Return to continuation after checking for stack overflow. */
#define return_closcall1(td,clo,a1) { \
 char top; \
 if (stack_overflow(&top,(((gc_thread_data *)data)->stack_limit))) { \
     object buf[1]; buf[0] = a1;\
     GC(td,clo,buf,1); return; \
 } else {closcall1(td,(closure) (clo),a1); return;}}
#define closcall2(td,clo,a1,a2) if (type_of(clo) == cons_tag || prim(clo)) { Cyc_apply(td,1, (closure)(a1), clo,a2); } else { ((clo)->fn)(td,2,clo,a1,a2);}
/* Return to continuation after checking for stack overflow. */
#define return_closcall2(td,clo,a1,a2) { \
 char top; \
 if (stack_overflow(&top,(((gc_thread_data *)data)->stack_limit))) { \
     object buf[2]; buf[0] = a1;buf[1] = a2;\
     GC(td,clo,buf,2); return; \
 } else {closcall2(td,(closure) (clo),a1,a2); return;}}
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

/* Mutation table functions
 *
 * Keep track of mutations (EG: set-car!) so that new
 * values are transported to the heap during GC.
 * Note these functions and underlying data structure are only used by
 * the calling thread, so locking is not required.
 */

void add_mutation(void *data, object var, object value){
  gc_thread_data *thd = (gc_thread_data *)data;
  if (is_object_type(value)) {
    thd->mutations = mcons(var, thd->mutations);
  }
}

/* TODO: consider a more efficient implementation, such as reusing old nodes
         instead of reclaiming them each time
 */
void clear_mutations(void *data) {
  gc_thread_data *thd = (gc_thread_data *)data;
  list l = thd->mutations, next;
  while (!nullp(l)) {
    next = cdr(l);
    free(l);
    l = next;
  }
  thd->mutations = nil;
}
/* END mutation table */

/* Runtime globals */
object Cyc_glo_call_cc = nil;
object Cyc_glo_eval_from_c = nil;

/* Exception handler */
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

object Cyc_current_exception_handler(void *data) {
  gc_thread_data *thd = (gc_thread_data *)data;
  if (nullp(thd->exception_handler_stack)) {
    return primitive_Cyc_91default_91exception_91handler;
  } else {
    return car(thd->exception_handler_stack);
  }
}

/* Raise an exception from the runtime code */
void Cyc_rt_raise(void *data, object err) {
    make_cons(c2, err, nil);
    make_cons(c1, boolean_f, &c2);
    make_cons(c0, &c1, nil);
    apply(data, nil, Cyc_current_exception_handler(data), &c0);
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
    apply(data, nil, Cyc_current_exception_handler(data), &c0);
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
    if (obj_is_int(x)) return (obj_is_int(y) && x == y) ||
                              (is_object_type(y) && 
                               type_of(y) == integer_tag &&
                               integer_value(y) == obj_obj2int(x));
    switch(type_of(x)) {
    case integer_tag:
      return (obj_is_int(y) && obj_obj2int(y) == integer_value(x)) ||
             (is_object_type(y) && 
              type_of(y) == integer_tag &&
              ((integer_type *) x)->value == ((integer_type *) y)->value);
    case double_tag:
      return (is_object_type(y) &&
              type_of(y) == double_tag &&
              ((double_type *) x)->value == ((double_type *) y)->value);
    case string_tag:
      return (is_object_type(y) &&
              type_of(y) == string_tag &&
              strcmp(((string_type *) x)->str,
                     ((string_type *) y)->str) == 0);
    case vector_tag:
      if (is_object_type(y) &&
          type_of(y) == vector_tag && 
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
 if (obj_is_int(x)) { fprintf(port, "%ld", obj_obj2int(x)); return quote_void; }
 switch (type_of(x))
   {case macro_tag:
      fprintf(port, "<macro %p>",(void *)((closure) x)->fn);
      break;
    case closure0_tag:
    case closure1_tag:
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
    case mutex_tag:
      fprintf(port, "<mutex %p>", x);
      break;
    case cond_var_tag:
      fprintf(port, "<condition variable %p>", x);
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
      fprintf(port, "%.16f", ((double_type *) x)->value);
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
    case bytevector_tag:
      fprintf(port, "#u8(");
      for (i = 0; i < ((bytevector) x)->len; i++) {
        if (i > 0) { 
          fprintf(port, " "); 
        }
        fprintf(port, "%u", (unsigned char)(((bytevector)x)->data[i]));
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
 if (obj_is_int(x)) {Cyc_display(x, port); return quote_void;}
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
 //fprintf(port, "\n");
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
    if (is_value_type(x) || is_value_type(y) ||
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

object Cyc_num_cmp_va_list(void *data, int argc, int (fn_op(void *, object, object)), object n, va_list ns) {
  int i;
  object next; 

  if (argc < 2) {
    Cyc_rt_raise_msg(data, "Not enough arguments for boolean operator\n");
  }

  for (i = 1; i < argc; i++) {
    next = va_arg(ns, object);
    if (!fn_op(data, n, next)) {
      return boolean_f;
    }
    n = next;
  }

  return boolean_t;
}

#define declare_num_cmp(FUNC, FUNC_OP, FUNC_APPLY, OP) \
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
    } else { \
        make_string(s, "Bad argument type"); \
        make_cons(c1, y, nil); \
        make_cons(c0, &s, &c1); \
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
    return_closcall1(data, cont, result); \
} \
void FUNC_APPLY(void *data, int argc, object clo, object cont, object n, ...) { \
    object result; \
    va_list ap; \
    va_start(ap, n); \
    result = Cyc_num_cmp_va_list(data, argc - 1, FUNC_OP, n, ap); \
    va_end(ap); \
    return_closcall1(data, cont, result); \
}

declare_num_cmp(Cyc_num_eq,  Cyc_num_eq_op,  dispatch_num_eq,  ==);
declare_num_cmp(Cyc_num_gt,  Cyc_num_gt_op,  dispatch_num_gt,  >);
declare_num_cmp(Cyc_num_lt,  Cyc_num_lt_op,  dispatch_num_lt,  <);
declare_num_cmp(Cyc_num_gte, Cyc_num_gte_op, dispatch_num_gte, >=);
declare_num_cmp(Cyc_num_lte, Cyc_num_lte_op, dispatch_num_lte, <=);

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
    if (!nullp(o) && (obj_is_int(o) || 
        (!is_value_type(o) && (type_of(o) == integer_tag || type_of(o) == double_tag))))
        return boolean_t;
    return boolean_f;}

object Cyc_is_real(object o){
    return Cyc_is_number(o);}

object Cyc_is_integer(object o){
    if (!nullp(o) && (obj_is_int(o) ||
        (!is_value_type(o) && type_of(o) == integer_tag)))
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

object Cyc_is_bytevector(object o){
    if (!nullp(o) && !is_value_type(o) && ((list)o)->tag == bytevector_tag)
        return boolean_t;
    return boolean_f;}

object Cyc_is_port(object o){
    if (!nullp(o) && !is_value_type(o) && ((list)o)->tag == port_tag)
        return boolean_t;
    return boolean_f;}

object Cyc_is_mutex(object o){
    if (!nullp(o) && !is_value_type(o) && ((list)o)->tag == mutex_tag)
        return boolean_t;
    return boolean_f;}

object Cyc_is_cond_var(object o){
    if (!nullp(o) && !is_value_type(o) && ((list)o)->tag == cond_var_tag)
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
            tag == closureN_tag ||
            tag == primitive_tag) {
            return boolean_t;
        } else if (tag == cons_tag) {
          integer_type l = Cyc_length_as_object(data, o);
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
    add_mutation(data, l, val);
    return l;
}

object Cyc_set_cdr(void *data, object l, object val) {
    if (Cyc_is_cons(l) == boolean_f) Cyc_invalid_type_error(data, cons_tag, l);
    gc_mut_update((gc_thread_data *)data, cdr(l), val);
    cdr(l) = val;
    add_mutation(data, l, val);
    return l;
}

object Cyc_vector_set(void *data, object v, object k, object obj) {
  int idx;
  Cyc_check_vec(data, v);
  Cyc_check_int(data, k);
  idx = obj_is_int(k) ? obj_obj2int(k) : ((integer_type *)k)->value;

  if (idx < 0 || idx >= ((vector)v)->num_elt) {
    Cyc_rt_raise2(data, "vector-set! - invalid index", k);
  }

  gc_mut_update((gc_thread_data *)data, 
                ((vector)v)->elts[idx],
                obj);

  ((vector)v)->elts[idx] = obj;
  // TODO: probably could be more efficient here and also pass
  //       index, so only that one entry needs GC.
  add_mutation(data, v, obj);
  return v;
}

object Cyc_vector_ref(void *data, object v, object k) {
  int idx;
  if (nullp(v) || is_value_type(v) || ((list)v)->tag != vector_tag) {
    Cyc_rt_raise_msg(data, "vector-ref - invalid parameter, expected vector\n"); 
  }
  if ((!obj_is_int(k)) && (nullp(k) || is_value_type(k) || ((list)k)->tag != integer_tag)) {
    Cyc_rt_raise_msg(data, "vector-ref - invalid parameter, expected integer\n"); 
  }
  if (obj_is_int(k)) {
    idx = obj_obj2int(k);
  } else {
    idx = integer_value(k);
  }
  if (idx < 0 || idx >= ((vector)v)->num_elt) {
    Cyc_rt_raise2(data, "vector-ref - invalid index", obj_int2obj(idx));
  }

  return ((vector)v)->elts[idx];
}

integer_type Cyc_length_as_object(void *data, object l){
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

object Cyc_vector_length(void *data, object v) {
    if (!nullp(v) && !is_value_type(v) && ((list)v)->tag == vector_tag) {
      return obj_int2obj(((vector)v)->num_elt);
    }
    Cyc_rt_raise_msg(data, "vector-length - invalid parameter, expected vector\n"); }


object Cyc_length(void *data, object l){
    int len = 0;
    while(!nullp(l)){
        if (is_value_type(l) || ((list)l)->tag != cons_tag){
            Cyc_rt_raise_msg(data, "length - invalid parameter, expected list\n");
        }
        l = cdr(l);
        len++;
    }
    return obj_int2obj(len);
}

char *int_to_binary(char *b, int x)
{
    b[0] = '\0';

    int z;
    for (z = 65536; z > 0; z >>= 1)
    {
        strcat(b, ((x & z) == z) ? "1" : "0");
    }

    return b;
}

object Cyc_number2string2(void *data, object cont, int argc, object n, ...) {
    object base = nil;
    int base_num = 10, val;
    char buffer[1024];
    va_list ap;
    va_start(ap, n);
    if (argc > 1) {
      base = va_arg(ap, object);
      Cyc_check_int(data, base);
    }
    va_end(ap);
    Cyc_check_num(data, n);
    if (base) {
      base_num = obj_is_int(base) ? obj_obj2int(base) : integer_value(base);
    }

    if (base_num == 2) {
      val = obj_is_int(n) ?
              obj_obj2int(n) :
              type_of(n) == integer_tag ?
                integer_value(n) :
                ((int)double_value(n));
      int_to_binary(buffer, val);
    } else if (base_num == 8) {
      val = obj_is_int(n) ?
              obj_obj2int(n) :
              type_of(n) == integer_tag ?
                integer_value(n) :
                ((int)double_value(n));
      snprintf(buffer, 1024, "%o", val);
    } else if (base_num == 16) {
      val = obj_is_int(n) ?
              obj_obj2int(n) :
              type_of(n) == integer_tag ?
                integer_value(n) :
                ((int)double_value(n));
      snprintf(buffer, 1024, "%X", val);
    } else {
      if (obj_is_int(n)) {
          snprintf(buffer, 1024, "%ld", obj_obj2int(n));
      }else if (type_of(n) == integer_tag) {
          snprintf(buffer, 1024, "%d", ((integer_type *)n)->value);
      } else if (type_of(n) == double_tag) {
          snprintf(buffer, 1024, "%.16f", ((double_type *)n)->value);
      } else {
          Cyc_rt_raise2(data, "number->string - Unexpected object", n);
      }
    }
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
    object len;

    Cyc_check_cons_or_nil(data, lst);
    
    len = Cyc_length(data, lst); // Inefficient, walks whole list
    buf = alloca(sizeof(char) * (obj_obj2int(len) + 1));
    while(!nullp(lst)){
        buf[i++] = obj_obj2char(car(lst));
        lst = cdr(lst);
    }
    buf[i] = '\0';

    //{ make_string_noalloc(str, buf, i);
    { make_string(str, buf);
      return_closcall1(data, cont, &str);}
}

object Cyc_string2number2_(void *data, object cont, int argc, object str, ...) 
{
  object base = nil;
  int base_num, result;
  va_list ap;
  va_start(ap, str);
  if (argc > 1) {
    base = va_arg(ap, object);
    Cyc_check_int(data, base);
  }
  va_end(ap);
  if (base) {
    base_num = obj_is_int(base) ? obj_obj2int(base) : integer_value(base);
    Cyc_check_str(data, str);
    if (base_num == 2) {
      result = binstr2int(string_str(str));
      return_closcall1(data, cont, obj_int2obj(result));
    }else if (base_num == 8) {
      result = octstr2int(string_str(str));
      return_closcall1(data, cont, obj_int2obj(result));
    }else if (base_num == 16) {
      result = hexstr2int(string_str(str));
      return_closcall1(data, cont, obj_int2obj(result));
    }
  }
  Cyc_string2number_(data, cont, str);
}

object Cyc_string2number_(void *data, object cont, object str){
    int result;
    double n;
    Cyc_check_obj(data, string_tag, str);
    Cyc_check_str(data, str);
    if (type_of(str) == string_tag &&
        ((string_type *) str)->str){
        n = atof(((string_type *) str)->str);

        if (ceilf(n) == n) {
            result = (int)n;
            return_closcall1(data, cont, obj_int2obj(result));
        }
        else {
            make_double(result, n);
            return_closcall1(data, cont, &result);
        }
    } else {
        // TODO: not good enough because we do pointer comparisons to #f
        //result.boolean_t = boolean_f;
    }

    Cyc_rt_raise2(data, "Expected string but received", str);
}

int binstr2int(const char *str)
{
  int num = 0;
  while (*str) {
    num <<= 1;
    if (*str++ == '1') num++;
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

int hexstr2int(const char *str)
{
  int num = 0;
  while (*str) {
    num <<= 4;
    if (*str >= 'A' && *str <= 'F'){
      num += (((*str) - 'A') + 10);
    } else if (*str >= 'a' && *str <= 'f'){
      num += (((*str) - 'a') + 10);
    } else {
      num += ((*str) - '0');
    }
    *str++;
  }
  return num;
}

object Cyc_string_cmp(void *data, object str1, object str2) {
  Cyc_check_str(data, str1);
  Cyc_check_str(data, str2);
  return obj_int2obj( strcmp(((string_type *)str1)->str,
                             ((string_type *)str2)->str) );
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

object Cyc_string_length(void *data, object str) {
  Cyc_check_obj(data, string_tag, str);
  Cyc_check_str(data, str);
  return obj_int2obj(strlen(string_str(str))); }

object Cyc_string_set(void *data, object str, object k, object chr) {
  char *raw;
  int idx, len;

  Cyc_check_str(data, str);
  Cyc_check_int(data, k);

  if (!eq(boolean_t, Cyc_is_char(chr))) {
    Cyc_rt_raise2(data, "Expected char but received", chr);
  }

  raw = string_str(str);
  idx = obj_is_int(k) ? obj_obj2int(k) : integer_value(k),
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
  idx = obj_is_int(k) ? obj_obj2int(k) : integer_value(k),
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
  s = obj_is_int(start) ? obj_obj2int(start) : integer_value(start),
  e = obj_is_int(end) ? obj_obj2int(end) : integer_value(end),
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


object Cyc_make_vector(void *data, object cont, int argc, object len, ...) {
  object v = nil;
  object fill = boolean_f;
  int i;
  va_list ap;
  va_start(ap, len);
  if (argc > 1) {
    fill = va_arg(ap, object);
  }
  va_end(ap);
  Cyc_check_int(data, len);
  v = alloca(sizeof(vector_type));
  ((vector)v)->hdr.mark = gc_color_red;
  ((vector)v)->hdr.grayed = 0;
  ((vector)v)->tag = vector_tag;
  ((vector)v)->num_elt = obj_is_int(len) ? obj_obj2int(len) : ((integer_type *)len)->value;
  ((vector)v)->elts = 
    (((vector)v)->num_elt > 0) ? 
     (object *)alloca(sizeof(object) * ((vector)v)->num_elt) : 
     NULL;
  for (i = 0; i < ((vector)v)->num_elt; i++) {
    ((vector)v)->elts[i] = fill;
  }
  return_closcall1(data, cont, v);
}

object Cyc_make_bytevector(void *data, object cont, int argc, object len, ...) {
  object bv = nil;
  object fill;
  int i, length, fill_val;
  va_list ap;
  va_start(ap, len);
  if (argc > 1) {
    fill = va_arg(ap, object);
  }
  va_end(ap);
  Cyc_check_int(data, len);
  length = obj_is_int(len) ? obj_obj2int(len) : integer_value(len);

  bv = alloca(sizeof(bytevector_type));
  ((bytevector)bv)->hdr.mark = gc_color_red;
  ((bytevector)bv)->hdr.grayed = 0;
  ((bytevector)bv)->tag = bytevector_tag;
  ((bytevector)bv)->len = length;
  ((bytevector)bv)->data = alloca(sizeof(char) * length);
  if (argc > 1) {
    Cyc_check_int(data, fill);
    fill_val = obj_is_int(fill) ? obj_obj2int(fill) : integer_value(fill);
    memset(((bytevector)bv)->data, (unsigned char)fill_val, length);
  }
  return_closcall1(data, cont, bv);
}

#define Cyc_bytevector_va_list(argc) { \
  int i = 0, val; \
  va_list ap; \
  object tmp; \
  char *buffer; \
  make_empty_bytevector(bv); \
  if (argc > 0) { \
    Cyc_check_int(data, bval); \
    buffer = alloca(sizeof(char) * argc); \
    val = obj_is_int(bval) ? obj_obj2int(bval) : integer_value(bval); \
    buffer[i] = val; \
    va_start(ap, bval); \
    for(i = 1; i < argc; i++) { \
      tmp = va_arg(ap, object); \
      Cyc_check_int(data, tmp); \
      val = obj_is_int(tmp) ? obj_obj2int(tmp) : integer_value(tmp); \
      buffer[i] = (unsigned char)val; \
    } \
    va_end(ap); \
    bv.len = argc; \
    bv.data = buffer; \
  } \
  return_closcall1(data, cont, &bv); \
}

void dispatch_bytevector(void *data, int _argc, object clo, object cont, object bval, ...) {
  Cyc_bytevector_va_list((_argc - 1));
}

object Cyc_bytevector(void *data, object cont, int _argc, object bval, ...) {
  Cyc_bytevector_va_list(_argc);
}

#define Cyc_bytevector_append_va_list(argc) { \
  int i = 0, buf_idx = 0, val, total_length = 0; \
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
  return_closcall1(data, cont, &result); \
}

void dispatch_bytevector_91append(void *data, int _argc, object clo, object cont, object bv, ...) {
  Cyc_bytevector_append_va_list((_argc - 1));
}

object Cyc_bytevector_append(void *data, object cont, int _argc, object bv, ...) {
  Cyc_bytevector_append_va_list(_argc);
}

object Cyc_bytevector_copy(void *data, object cont, object bv, object start, object end) {
  const char *buf;
  int s, e;
  int len;
  make_empty_bytevector(result);

  Cyc_check_bvec(data, bv);
  Cyc_check_int(data, start);
  Cyc_check_int(data, end);

  buf = ((bytevector)bv)->data;
  s = obj_is_int(start) ? obj_obj2int(start) : integer_value(start);
  e = obj_is_int(end) ? obj_obj2int(end) : integer_value(end);
  len = e - s;

  if (s < 0 || s >= ((bytevector)bv)->len) {
    Cyc_rt_raise2(data, "bytevector-copy - invalid start", start);
  }

  if (e < 0 || e < s || e > ((bytevector)bv)->len) {
    Cyc_rt_raise2(data, "bytevector-copy - invalid end", end);
  }

  result.len = len;
  result.data = alloca(sizeof(char) * len);
  memcpy(&result.data[0], &(((bytevector)bv)->data)[s], len);
  return_closcall1(data, cont, &result);
}

object Cyc_utf82string(void *data, object cont, object bv, object start, object end) {
  const char *buf;
  int s, e;
  int len;

  Cyc_check_bvec(data, bv);
  Cyc_check_int(data, start);
  Cyc_check_int(data, end);

  buf = ((bytevector)bv)->data;
  s = obj_is_int(start) ? obj_obj2int(start) : integer_value(start);
  e = obj_is_int(end) ? obj_obj2int(end) : integer_value(end);
  len = e - s;

  if (s < 0 || (s >= ((bytevector)bv)->len && len > 0)) {
    Cyc_rt_raise2(data, "utf8->string - invalid start", start);
  }

  if (e < 0 || e < s || e > ((bytevector)bv)->len) {
    Cyc_rt_raise2(data, "utf8->string - invalid end", end);
  }

  {
    make_string_noalloc(st, NULL, len);
    st.str = alloca(sizeof(char) * (len + 1));
    memcpy(st.str, &buf[s], len);
    st.str[len] = '\0';
    return_closcall1(data, cont, &st);
  }
}

object Cyc_string2utf8(void *data, object cont, object str, object start, object end) {
  const char *buf;
  int s, e;
  int len;
  make_empty_bytevector(result);

  Cyc_check_str(data, str);
  Cyc_check_int(data, start);
  Cyc_check_int(data, end);

  buf = string_str(str);
  s = obj_is_int(start) ? obj_obj2int(start) : integer_value(start);
  e = obj_is_int(end) ? obj_obj2int(end) : integer_value(end);
  len = e - s;

  if (s < 0 || (s >= string_len(str) && len > 0)) {
    Cyc_rt_raise2(data, "string->utf8 - invalid start", start);
  }

  if (e < 0 || e < s || e > string_len(str)) {
    Cyc_rt_raise2(data, "string->utf8 - invalid end", end);
  }

  result.len = len;
  result.data = alloca(sizeof(char) * len);
  memcpy(&result.data[0], &(string_str(str))[s], len);
  return_closcall1(data, cont, &result);
}

object Cyc_bytevector_u8_ref(void *data, object bv, object k) {
  const char *buf;
  int idx;
  int val;

  Cyc_check_bvec(data, bv);
  Cyc_check_int(data, k);

  buf = ((bytevector)bv)->data;
  idx = obj_is_int(k) ? obj_obj2int(k) : integer_value(k);

  if (idx < 0 || idx >= ((bytevector)bv)->len) {
    Cyc_rt_raise2(data, "bytevector-u8-ref - invalid index", k);
  }

  val = (unsigned char)(buf[idx]);
  return obj_int2obj(val);
}

object Cyc_bytevector_u8_set(void *data, object bv, object k, object b) {
  char *buf;
  int idx, len, val;

  Cyc_check_bvec(data, bv);
  Cyc_check_int(data, k);
  Cyc_check_int(data, b);

  buf = ((bytevector)bv)->data;
  idx = obj_is_int(k) ? obj_obj2int(k) : integer_value(k);
  val = obj_is_int(b) ? obj_obj2int(b) : integer_value(b);
  len = ((bytevector)bv)->len;

  Cyc_check_bounds(data, "bytevector-u8-set!", len, idx);
  buf[idx] = (unsigned char)val;
  return bv;
}

object Cyc_bytevector_length(void *data, object bv) {
    if (!nullp(bv) && !is_value_type(bv) && ((list)bv)->tag == bytevector_tag) {
      return obj_int2obj(((bytevector)bv)->len);
    }
    Cyc_rt_raise_msg(data, "bytevector-length - invalid parameter, expected bytevector\n"); }

object Cyc_list2vector(void *data, object cont, object l) {
  object v = nil; 
  object len;
  object lst = l; 
  int i = 0; 

  Cyc_check_cons_or_nil(data, l); 
  len = Cyc_length(data, l); 
  v = alloca(sizeof(vector_type)); 
  ((vector)v)->hdr.mark = gc_color_red;
  ((vector)v)->hdr.grayed = 0;
  ((vector)v)->tag = vector_tag; 
  ((vector)v)->num_elt = obj_obj2int(len);
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

object Cyc_system(object cmd) {
  if (nullp(cmd) || is_value_type(cmd) || type_of(cmd) != string_tag) {
    return obj_int2obj(-1);
  }
  return obj_int2obj(system(((string_type *)cmd)->str));
}

object Cyc_char2integer(object chr){
    return obj_int2obj(obj_obj2char(chr));
}

object Cyc_integer2char(void *data, object n){
    int val = 0;

    Cyc_check_int(data, n);
    val = (obj_is_int(n) ? obj_obj2int(n) : integer_value(n));
    return obj_char2obj(val);
}

void Cyc_halt(closure);
void Cyc_halt(env) closure env; {
#if DEBUG_SHOW_DIAG
 gc_print_stats(Cyc_heap);
#endif
 exit(0);}

object __halt(object obj) {
    Cyc_halt(obj);
    return nil;
}

#define declare_num_op(FUNC, FUNC_OP, FUNC_APPLY, OP, DIV) \
object FUNC_OP(void *data, common_type *x, object y) { \
    int tx = type_of(x), ty = (obj_is_int(y) ? -1 : type_of(y)); \
    if (DIV &&  \
        ((ty == -1 && (obj_obj2int(y) == 0)) || \
         (ty == integer_tag && integer_value(y) == 0) || \
         (ty == double_tag && double_value(y) == 0.0))) { \
      Cyc_rt_raise_msg(data, "Divide by zero"); \
    } \
    if (tx == integer_tag && ty == -1) { \
        x->integer_t.value = (x->integer_t.value) OP (obj_obj2int(y)); \
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
    } else { \
        make_string(s, "Bad argument type"); \
        make_cons(c1, y, nil); \
        make_cons(c0, &s, &c1); \
        Cyc_rt_raise(data, &c0); \
    } \
    return x; \
} \
object FUNC(void *data, object cont, int argc, object n, ...) { \
    common_type buffer; \
    object result; \
    va_list ap; \
    va_start(ap, n); \
    result = Cyc_num_op_va_list(data, argc, FUNC_OP, n, ap, &buffer); \
    va_end(ap); \
    return_closcall1(data, cont, result); \
} \
void FUNC_APPLY(void *data, int argc, object clo, object cont, object n, ...) { \
    common_type buffer; \
    object result; \
    va_list ap; \
    va_start(ap, n); \
    result = Cyc_num_op_va_list(data, argc - 1, FUNC_OP, n, ap, &buffer); \
    va_end(ap); \
    return_closcall1(data, cont, result); \
}

declare_num_op(Cyc_sum, Cyc_sum_op, dispatch_sum, +, 0);
declare_num_op(Cyc_sub, Cyc_sub_op, dispatch_sub, -, 0);
declare_num_op(Cyc_mul, Cyc_mul_op, dispatch_mul, *, 0);
declare_num_op(Cyc_div, Cyc_div_op, dispatch_div, /, 1);

object Cyc_num_op_va_list(void *data, int argc, object (fn_op(void *, common_type *, object)), object n, va_list ns, common_type *buf) {
  int i;
  if (argc == 0) {
    buf->integer_t.hdr.mark = gc_color_red;
    buf->integer_t.hdr.grayed = 0;
    buf->integer_t.tag = integer_tag;
    buf->integer_t.value = 0;
    return buf;
  }

  if (obj_is_int(n)) {
    buf->integer_t.hdr.mark = gc_color_red;
    buf->integer_t.hdr.grayed = 0; 
    buf->integer_t.tag = integer_tag;
    buf->integer_t.value = obj_obj2int(n);
  } else if (type_of(n) == integer_tag) {
    buf->integer_t.hdr.mark = gc_color_red;
    buf->integer_t.hdr.grayed = 0; 
    buf->integer_t.tag = integer_tag;
    buf->integer_t.value = ((integer_type *)n)->value;
  } else if (type_of(n) == double_tag) {
    buf->double_t.hdr.mark = gc_color_red;
    buf->double_t.hdr.grayed = 0;
    buf->double_t.tag = double_tag;
    buf->double_t.value = ((double_type *)n)->value;
  } else {
      make_string(s, "Bad argument type");
      make_cons(c1, n, nil);
      make_cons(c0, &s, &c1);
      Cyc_rt_raise(data, &c0);
  }

  for (i = 1; i < argc; i++) {
    fn_op(data, buf, va_arg(ns, object));
  }

  // TODO: if result is integer, could convert to an immediate here
  if (type_of(buf) == integer_tag) {
    return obj_int2obj(buf->integer_t.value);
  }

  return buf;
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
        set_thread_blocked(data, cont);
        c = fgetc(((port_type *) port)->fp);
        return_thread_runnable(data, (c != EOF) ? obj_char2obj(c) : Cyc_EOF);
    }
    return Cyc_EOF;
}

/* TODO: this function needs some work, but approximates what is needed */
object Cyc_io_read_line(void *data, object cont, object port) {
  FILE *stream = ((port_type *)port)->fp;
  char buf[1024];
  int i = 0, c;
  
  set_thread_blocked(data, cont);
  while (1) {
    c = fgetc(stream);
    if (c == EOF && i == 0) {
      return_thread_runnable(data, Cyc_EOF);
    } else if (c == EOF || i == 1023 || c == '\n') {
      buf[i] = '\0';
      {
        make_string(s, buf);
        return_thread_runnable(data, &s);
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
        set_thread_blocked(data, cont);
        c = fgetc(stream);
        ungetc(c, stream);
        return_thread_runnable(data, (c != EOF) ? obj_char2obj(c) : Cyc_EOF);
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
    { object obj = Cyc_length(data, car(args));
      return_closcall1(data, cont, obj); }}
void _bytevector_91length(void *data, object cont, object args){ 
    Cyc_check_num_args(data, "bytevector_91length", 1, args);
    { object obj = Cyc_bytevector_length(data, car(args));
      return_closcall1(data, cont, obj); }}
void _bytevector_91u8_91ref(void *data, object cont, object args) {
    Cyc_check_num_args(data, "bytevector-u8-ref", 2, args);
    { object c = Cyc_bytevector_u8_ref(data, car(args), cadr(args));
      return_closcall1(data, cont, c); }}
void _bytevector_91u8_91set_67(void *data, object cont, object args) {
    Cyc_check_num_args(data, "bytevector-u8-set!", 3, args);
    { object bv = Cyc_bytevector_u8_set(data, car(args), cadr(args), caddr(args));
      return_closcall1(data, cont, bv); }}
void _bytevector(void *data, object cont, object args) {
    object argc = Cyc_length(data, args);
    dispatch(data, obj_obj2int(argc), (function_type)dispatch_bytevector, cont, cont, args); }
void _bytevector_91append(void *data, object cont, object args) {
    object argc = Cyc_length(data, args);
    dispatch(data, obj_obj2int(argc), (function_type)dispatch_bytevector_91append, cont, cont, args); }
void _Cyc_91bytevector_91copy(void *data, object cont, object args) {
    object argc = Cyc_length(data, args);
    Cyc_check_num_args(data, "Cyc-bytevector-copy", 3, args);
    Cyc_bytevector_copy(data, cont, car(args), cadr(args), caddr(args)); }
void _Cyc_91string_91_125utf8(void *data, object cont, object args) {
    object argc = Cyc_length(data, args);
    Cyc_check_num_args(data, "Cyc-string->utf8", 3, args);
    Cyc_string2utf8(data, cont, car(args), cadr(args), caddr(args)); }
void _Cyc_91utf8_91_125string(void *data, object cont, object args) {
    object argc = Cyc_length(data, args);
    Cyc_check_num_args(data, "Cyc-utf8->string", 3, args);
    Cyc_utf82string(data, cont, car(args), cadr(args), caddr(args)); }
void _vector_91length(void *data, object cont, object args){ 
    Cyc_check_num_args(data, "vector_91length", 1, args);
    { object obj = Cyc_vector_length(data, car(args));
      return_closcall1(data, cont, obj); }}
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
void __87(void *data, object cont, object args) {
    integer_type argc = Cyc_length_as_object(data, args);
    dispatch(data, argc.value, (function_type)dispatch_sum, cont, cont, args); }
void __91(void *data, object cont, object args) {
    Cyc_check_num_args(data, "-", 1, args);
    { integer_type argc = Cyc_length_as_object(data, args);
      dispatch(data, argc.value, (function_type)dispatch_sub, cont, cont, args); }}
void __85(void *data, object cont, object args) {
    integer_type argc = Cyc_length_as_object(data, args);
    dispatch(data, argc.value, (function_type)dispatch_mul, cont, cont, args); }
void __95(void *data, object cont, object args) {
    Cyc_check_num_args(data, "/", 1, args);
    { integer_type argc = Cyc_length_as_object(data, args);
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
void _bytevector_127(void *data, object cont, object args) {
    Cyc_check_num_args(data, "bytevector?", 1, args);
    return_closcall1(data, cont, Cyc_is_bytevector(car(args))); }
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
#if DEBUG_SHOW_DIAG
    gc_print_stats(Cyc_heap);
#endif
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
    integer_type argc = Cyc_length_as_object(data, args);
    dispatch(data, argc.value, (function_type)dispatch_num_eq, cont, cont, args); }
void __125(void *data, object cont, object args) {  
    integer_type argc = Cyc_length_as_object(data, args);
    dispatch(data, argc.value, (function_type)dispatch_num_gt, cont, cont, args); }
void __121(void *data, object cont, object args) {
    integer_type argc = Cyc_length_as_object(data, args);
    dispatch(data, argc.value, (function_type)dispatch_num_lt, cont, cont, args); }
void __125_123(void *data, object cont, object args) {
    integer_type argc = Cyc_length_as_object(data, args);
    dispatch(data, argc.value, (function_type)dispatch_num_gte, cont, cont, args); }
void __121_123(void *data, object cont, object args) {
    integer_type argc = Cyc_length_as_object(data, args);
    dispatch(data, argc.value, (function_type)dispatch_num_lte, cont, cont, args); }

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
    { object obj = Cyc_char2integer(car(args));
      return_closcall1(data, cont, obj);}}
void _integer_91_125char(void *data, object cont, object args) {  
    Cyc_check_num_args(data, "integer->char", 1, args);
    return_closcall1(data, cont, Cyc_integer2char(data, car(args)));}
void _string_91_125number(void *data, object cont, object args) {  
    Cyc_check_num_args(data, "string->number", 1, args);
    { object tail = cdr(args);
      if (tail) {
        Cyc_string2number2_(data, cont, 2, car(args), cadr(args));
      } else {
        Cyc_string2number_(data, cont, car(args)); }}}
void _string_91length(void *data, object cont, object args) {
    Cyc_check_num_args(data, "string-length", 1, args);
    { object obj = Cyc_string_length(data, car(args));
      return_closcall1(data, cont, obj);}}
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
    { object obj = Cyc_system(car(args));
      return_closcall1(data, cont, obj);}}
//void _error(void *data, object cont, object args) {
//    integer_type argc = Cyc_length_as_object(args);
//    dispatch_va(data, argc.value, dispatch_error, cont, cont, args); }
void _Cyc_91current_91exception_91handler(void *data, object cont, object args) {
    object handler = Cyc_current_exception_handler(data);
    return_closcall1(data, cont, handler); }
void _Cyc_91default_91exception_91handler(void *data, object cont, object args) {
    // TODO: this is a quick-and-dirty implementation, may be a better way to write this
    Cyc_default_exception_handler(data, 1, args, car(args));
}
void _string_91cmp(void *data, object cont, object args) {  
    Cyc_check_num_args(data, "string-cmp", 2, args);
    { object obj = Cyc_string_cmp(data, car(args), cadr(args));
      return_closcall1(data, cont, obj);}}
void _string_91append(void *data, object cont, object args) {  
    object argc = Cyc_length(data, args);
    dispatch(data, obj_obj2int(argc), (function_type)dispatch_string_91append, cont, cont, args); }
void _make_91vector(void *data, object cont, object args) {
    Cyc_check_num_args(data, "make-vector", 1, args);
    { object argc = Cyc_length(data, args);
      if (obj_obj2int(argc) >= 2) {
        Cyc_make_vector(data, cont, 2, car(args), cadr(args));}
      else {
        Cyc_make_vector(data, cont, 2, car(args), boolean_f);}}}
void _make_91bytevector(void *data, object cont, object args) {
    Cyc_check_num_args(data, "make-bytevector", 1, args);
    { object argc = Cyc_length(data, args);
      if (obj_obj2int(argc) >= 2) {
        Cyc_make_bytevector(data, cont, 2, car(args), cadr(args));}
      else {
        Cyc_make_bytevector(data, cont, 1, car(args));}}}
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
    { object tail = cdr(args);
      if (tail) {
        Cyc_number2string2(data, cont, 2, car(args), cadr(args));
      } else {
        Cyc_number2string2(data, cont, 1, car(args)); }}}
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
    { object argc = Cyc_length(data, args);
     dispatch(data, obj_obj2int(argc), (function_type)dispatch_write_va, cont, cont, args); }}
void _display(void *data, object cont, object args) {  
    Cyc_check_num_args(data, "display", 1, args);
    { object argc = Cyc_length(data, args);
      dispatch(data, obj_obj2int(argc), (function_type)dispatch_display_va, cont, cont, args); }}
void _call_95cc(void *data, object cont, object args){
    Cyc_check_num_args(data, "call/cc", 1, args);
    if (eq(boolean_f, Cyc_is_procedure(data, car(args)))) {
      Cyc_invalid_type_error(data, closure1_tag, car(args)); 
    }
    return_closcall2(data, __glo_call_95cc_scheme_base, cont, car(args));
}

/*
 * @param cont - Continuation for the function to call into
 * @param func - Function to execute
 * @param args - A list of arguments to the function
 */
object apply(void *data, object cont, object func, object args){
  object count;

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
    case closureN_tag:
      if (func == Cyc_glo_call_cc) {
        make_cons(c, cont, args);
//Cyc_display(args, stderr);
//        args = &c;
//Cyc_display(&c, stderr);
        count = Cyc_length(data, args);
        Cyc_check_num_args(data, "<procedure>", 1, args);
        dispatch(data, obj_obj2int(count), ((closure)func)->fn, func, cont, args);
      }
      count = Cyc_length(data, args);
      // TODO: validate number of args provided:
      Cyc_check_num_args(data, "<procedure>", ((closure)func)->num_args, args); // TODO: could be more efficient, eg: cyc_length(args) is called twice.
      dispatch(data, obj_obj2int(count), ((closure)func)->fn, func, cont, args);
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
          ((closure)Cyc_glo_eval_from_c)->fn(data, 2, Cyc_glo_eval_from_c, cont, &c, nil);

      // TODO: would be better to compare directly against symbols here,
      //       but need a way of looking them up ahead of time.
      //       maybe a libinit() or such is required.
      } else if (strncmp(((symbol)fobj)->pname, "primitive", 10) == 0) {
          make_cons(c, cadr(func), args);
          ((closure)Cyc_glo_eval_from_c)->fn(data, 3, Cyc_glo_eval_from_c, cont, &c, nil);
      } else if (strncmp(((symbol)fobj)->pname, "procedure", 10) == 0) {
          make_cons(c, func, args);
          ((closure)Cyc_glo_eval_from_c)->fn(data, 3, Cyc_glo_eval_from_c, cont, &c, nil);
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
    apply(data, cont, prim, 
          (argc > 0) 
            ? (object)&args[0]
            : nil);
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
 */
void Cyc_start_trampoline(gc_thread_data *thd)
{
  // Tank, load the jump program
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
#if GC_DEBUG_VERBOSE
       fprintf(stderr, "global pvar %p\n", glo);
#endif
       gc_mark_black(glo); // Mark actual object the global points to
     }
    }
  }
}

char *gc_fixup_moved_obj(gc_thread_data *thd, int *alloci, char *obj, object hp)
{
  int acquired_lock = 0;
  if (grayed(obj)) {
    // Try to acquire the lock, because we are already locked if
    // the collector is cooperating on behalf of the mutator
    if (pthread_mutex_trylock(&(thd->lock)) == 0) {
      acquired_lock = 1;
    }
    gc_mark_gray2(thd, hp);
    if (acquired_lock){
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
    case bytevector_tag: {
      bytevector_type *hp = gc_alloc(Cyc_heap, 
                              sizeof(bytevector_type) + sizeof(char) * (((bytevector) obj)->len), 
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
  if (stack_overflow(low_limit, temp) && \
      stack_overflow(temp, high_limit)){ \
    (obj) = (object) gc_move(temp, (gc_thread_data *)data, &alloci, &heap_grown); \
  } \
}

object Cyc_trigger_minor_gc(void *data, object cont) {
  gc_thread_data* thd = (gc_thread_data *)data;
  thd->gc_args[0] = boolean_t;
  GC(data, cont, thd->gc_args, 1);
  return nil;
}

// Do a minor GC
int gc_minor(void *data, object low_limit, object high_limit, closure cont, object *args, int num_args)
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

  // Transport exception stack
  gc_move2heap(((gc_thread_data *)data)->exception_handler_stack);

  // Transport mutations
  {
    list l;
    for (l = ((gc_thread_data *)data)->mutations; !nullp(l); l = cdr(l)) {
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
  clear_mutations(data); // Reset for next time

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
      case bytevector_tag:
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
void GC(void *data, closure cont, object *args, int num_args)
{ 
  char tmp;
  object low_limit = &tmp; // This is one end of the stack...
  object high_limit = ((gc_thread_data *)data)->stack_start;
  int alloci = gc_minor(data, low_limit, high_limit, cont, args, num_args);
  // Cooperate with the collector thread
  gc_mut_cooperate((gc_thread_data *)data, alloci);
  // Let it all go, Neo...
  longjmp(*(((gc_thread_data *)data)->jmp_start), 1);
}

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
static primitive_type bytevector_91length_primitive = {{0}, primitive_tag, "bytevector-length", &_bytevector_91length};
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
static primitive_type make_91bytevector_primitive = {{0}, primitive_tag, "make-bytevector", &_make_91bytevector};

static primitive_type bytevector_primitive = {{0}, primitive_tag, "bytevector", &_bytevector};
static primitive_type bytevector_91append_primitive = {{0}, primitive_tag, "bytevector-append", &_bytevector_91append};
static primitive_type Cyc_91bytevector_91copy_primitive = {{0}, primitive_tag, "Cyc-bytevector-copy", &_Cyc_91bytevector_91copy};
static primitive_type bytevector_91u8_91ref_primitive = {{0}, primitive_tag, "bytevector-u8-ref", &_bytevector_91u8_91ref};
static primitive_type bytevector_91u8_91set_67_primitive = {{0}, primitive_tag, "bytevector-u8-set!", &_bytevector_91u8_91set_67};
static primitive_type Cyc_91string_91_125utf8_primitive = {{0}, primitive_tag, "Cyc-string->utf8", &_Cyc_91string_91_125utf8};
static primitive_type Cyc_91utf8_91_125string_primitive = {{0}, primitive_tag, "Cyc-utf8->string", &_Cyc_91utf8_91_125string};
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
static primitive_type bytevector_127_primitive = {{0}, primitive_tag, "bytevector?", &_vector_127};
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
const object primitive_Cyc_91installation_91dir = &Cyc_91installation_91dir_primitive;
const object primitive_command_91line_91arguments = &command_91line_91arguments_primitive;
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
const object primitive_Cyc_91bytevector_91copy = &Cyc_91bytevector_91copy_primitive;
const object primitive_bytevector_91u8_91ref = &bytevector_91u8_91ref_primitive;
const object primitive_bytevector_91u8_91set_67 = &bytevector_91u8_91set_67_primitive;
const object primitive_Cyc_91string_91_125utf8 = & Cyc_91string_91_125utf8_primitive;
const object primitive_Cyc_91utf8_91_125string = &Cyc_91utf8_91_125string_primitive;
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
const object primitive_bytevector_127 = &bytevector_127_primitive;
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
  ck_pr_cas_int((int *)&(thd->thread_state), CYC_THREAD_STATE_NEW, CYC_THREAD_STATE_RUNNABLE); 
  Cyc_start_trampoline(thd);
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
  ck_pr_cas_int((int *)&(thd->thread_state), CYC_THREAD_STATE_RUNNABLE, CYC_THREAD_STATE_TERMINATED);
  pthread_exit(NULL); // For now, just a proof of concept
}

// For now, accept a number of milliseconds to sleep
object Cyc_thread_sleep(void *data, object timeout)
{
  struct timespec tim;
  long value;
  Cyc_check_int(data, timeout);
  value = (obj_is_int(timeout) ? obj_obj2int(timeout) : integer_value(timeout));
  tim.tv_sec = value / 1000;
  tim.tv_nsec = (value % 1000) * NANOSECONDS_PER_MILLISECOND;
  nanosleep(&tim, NULL);
  return boolean_t;
}

// Copy given object to the heap, if it is from the stack.
// This function is intended to be called directly from application code
object copy2heap(void *data, object obj) 
{
  char stack_pos;
  gc_thread_data *thd = (gc_thread_data *)data;
  int on_stack = stack_overflow((object)(&stack_pos), obj) &&
                 stack_overflow(obj, (object)thd->stack_start);
  if (!is_object_type(obj) || !on_stack) {
    return obj;
  }

  return gc_alloc(Cyc_heap, gc_allocated_bytes(obj, NULL, NULL), obj, data, &on_stack);
}

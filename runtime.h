/** 
 * Cyclone Scheme
 * Copyright (c) 2014, Justin Ethier
 * All rights reserved.
 *
 * This file contains the C runtime used by compiled programs.
 */

#ifndef CYCLONE_RUNTIME_H
#define CYCLONE_RUNTIME_H

#include "cyclone.h"

long global_stack_size;
long global_heap_size;

static symbol_type __EOF = {eof_tag, "", nil}; // symbol_type in lieu of custom type
static const object Cyc_EOF = &__EOF;

static object cell_get(object cell){
    return car(cell);
}

#define global_set(glo,value) (glo=value)

/* Variable argument count support 

   This macro is intended to be executed at the top of a function that
   is passed 'var' as a variable-length argument. 'count' is the number
   of varargs that were passed. EG: 
   - C definition: f(object a, ...)
   - C call: f(1, 2, 3)
   - var: a
   - count: 3

   Argument count would need to be passed by the caller of f. Presumably
   our compiler will compute the difference between the number of required
   args and the number of provided ones, and pass the difference as 'count'
 */
#define load_varargs(var, count) { \
  int i; \
  object tmp; \
  list args = nil; \
  va_list va; \
  if (count > 0) { \
    args = alloca(sizeof(cons_type)*count); \
    va_start(va, var); \
    for (i = 0; i < count; i++) { \
      if (i) { \
          tmp = va_arg(va, object); \
      } else { \
          tmp = var; \
      } \
      args[i].tag = cons_tag; \
      args[i].cons_car = tmp; \
      args[i].cons_cdr = (i == (count-1)) ? nil : &args[i + 1]; \
    } \
    va_end(va); \
  } \
  var = args; \
}

/* Prototypes for Lisp built-in functions. */

static object Cyc_global_variables = nil;
static object Cyc_get_global_variables();
static object Cyc_get_cvar(object var);
static object Cyc_set_cvar(object var, object value);
static object apply(object cont, object func, object args);
static void Cyc_apply(int argc, closure cont, object prim, ...);
static void dispatch_string_91append(int argc, object clo, object cont, object str1, ...);
static string_type Cyc_string_append(int argc, object str1, ...);
static string_type Cyc_string_append_va_list(int, object, va_list);
//static void dispatch_error(int argc, object clo, object cont, object obj1, ...);
//static object Cyc_error(int count, object obj1, ...);
//static object Cyc_error_va(int count, object obj1, va_list ap);
//object Cyc_raise(object);
static object Cyc_default_exception_handler(int argc, closure _, object err);
object Cyc_current_exception_handler();
static list mcons(object,object);
static object terpri(void);
static object Cyc_display(object);
static object Cyc_write(object);
static object Cyc_is_boolean(object o);
static object Cyc_is_cons(object o);
static object Cyc_is_null(object o);
static object Cyc_is_number(object o);
static object Cyc_is_real(object o);
static object Cyc_is_integer(object o);
static object Cyc_is_symbol(object o);
static object Cyc_is_string(object o);
static object Cyc_is_char(object o);
static object Cyc_is_procedure(object o);
static object Cyc_is_eof_object(object o);
static object Cyc_is_cvar(object o);
static common_type Cyc_sum_op(object x, object y);
static common_type Cyc_sub_op(object x, object y);
static common_type Cyc_mul_op(object x, object y);
static common_type Cyc_div_op(object x, object y);
static common_type Cyc_sum(int argc, object n, ...);
static common_type Cyc_sub(int argc, object n, ...);
static common_type Cyc_mul(int argc, object n, ...);
static common_type Cyc_div(int argc, object n, ...);
static common_type Cyc_num_op_va_list(int argc, common_type (fn_op(object, object)), object n, va_list ns);
static int equal(object,object);
static list assq(object,list);
static object get(object,object);
static object equalp(object,object);
static object memberp(object,list);
static object memqp(object,list);
static char *transport(char *,int);
static void GC(closure,object*,int) never_returns;
static void main_main(long stack_size,long heap_size,char *stack_base) never_returns;
static long long_arg(int argc,char **argv,char *name,long dval);

/* Symbol Table */

/* Notes for the symbol table

 string->symbol can:
  - lookup symbol in the table
  - if found, return that pointer
  - otherwise, allocate symbol in table and return ptr to it

 For now, GC of symbols is missing. long-term it probably would be desirable
*/

char *_strdup (const char *s);
static object add_symbol(symbol_type *psym);
static object add_symbol_by_name(const char *name);
static object find_symbol_by_name(const char *name);
static object find_or_add_symbol(const char *name);
list symbol_table = nil;

char *_strdup (const char *s) {
    char *d = malloc (strlen (s) + 1);
    if (d) { strcpy (d,s); }
    return d;
}

static object find_symbol_by_name(const char *name) {
  list l = symbol_table;
  for (; !nullp(l); l = cdr(l)) {
    const char *str = symbol_pname(car(l));
    if (strcmp(str, name) == 0) return car(l);
  }
  return nil;
}

static object add_symbol(symbol_type *psym) {
  symbol_table = mcons(psym, symbol_table);
  return psym;
}

static object add_symbol_by_name(const char *name) {
  symbol_type sym = {symbol_tag, _strdup(name), nil};
  symbol_type *psym = malloc(sizeof(symbol_type));
  memcpy(psym, &sym, sizeof(symbol_type));
  return add_symbol(psym);
}

static object find_or_add_symbol(const char *name){
  object sym = find_symbol_by_name(name);
  if (sym){
    return sym;
  } else {
    return add_symbol_by_name(name);
  }
}
/* END symbol table */

/* Mutation table
 *
 * Keep track of mutations (EG: set-car!) so that new
 * values are transported to the heap during GC.
 */
list mutation_table = nil;

static void add_mutation(object var, object value);
static void clear_mutations();

static void add_mutation(object var, object value){
  if (is_object_type(value)) {
    mutation_table = mcons(var, mutation_table);
  }
}

/* TODO: consider a more efficient implementation, such as reusing old nodes
         instead of reclaiming them each time
 */
static void clear_mutations() {
  list l = mutation_table, next;
  while (!nullp(l)) {
    next = cdr(l);
    free(l);
    l = next;
  }
  mutation_table = nil;
}
/* END mutation table */

/* Global variables. */

static clock_t start;   /* Starting time. */

static char *stack_begin;   /* Initialized by main. */
static char *stack_limit1;  /* Initialized by main. */
static char *stack_limit2;

static char *bottom;    /* Bottom of tospace. */
static char *allocp;    /* Cheney allocate pointer. */
static char *alloc_end;

/* TODO: not sure this is the best strategy for strings, especially if there 
   are a lot of long, later gen strings because that will cause a lot of
   copying to occur during GC */
static char *dhbottom; /* Bottom of data heap */
static char *dhallocp; /* Current place in data heap */
static char *dhalloc_end;

static long no_gcs = 0; /* Count the number of GC's. */
static long no_major_gcs = 0; /* Count the number of GC's. */

static object gc_cont;   /* GC continuation closure. */
static object gc_ans[NUM_GC_ANS];    /* argument for GC continuation closure. */
static int gc_num_ans;
static jmp_buf jmp_main; /* Where to jump to. */

//static object test_exp1, test_exp2; /* Expressions used within test. */

/* Define the Lisp atoms that we need. */

defboolean(f,f);
defboolean(t,t);
defsymbol(Cyc_191procedure, procedure);

//static object quote_list_f;  /* Initialized by main to '(f) */
//static object quote_list_t;  /* Initialized by main to '(t) */

//static volatile object unify_subst = nil; /* This is a global Lisp variable. */
DECLARE_GLOBALS

/* These (crufty) printing functions are used for debugging. */
static object terpri() {printf("\n"); return nil;}

static int equal(x, y) object x, y;
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
    default:
      return x == y;
    }
}

static object Cyc_get_global_variables(){
    return Cyc_global_variables;
}

static object Cyc_get_cvar(object var) {
    if (is_object_type(var) && type_of(var) == cvar_tag) {
        return *(((cvar_type *)var)->pvar);
    }
    return var;
}

static object Cyc_set_cvar(object var, object value) {
    if (is_object_type(var) && type_of(var) == cvar_tag) {
        *(((cvar_type *)var)->pvar) = value;
    }
    return var;}

static object Cyc_has_cycle(object lst) {
    object slow_lst, fast_lst;
    int is_obj = is_object_type(lst);
    int type = type_of(lst);
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

static object Cyc_display(x) object x;
{object tmp = nil;
 object has_cycle = boolean_f;
 int i = 0;
 if (nullp(x)) {printf("()"); return x;}
 if (obj_is_char(x)) {printf("%c", obj_obj2char(x)); return x;}
 switch (type_of(x))
   {case closure0_tag:
    case closure1_tag:
    case closure2_tag:
    case closure3_tag:
    case closure4_tag:
    case closureN_tag:
      printf("<procedure %p>",(void *)((closure) x)->fn);
      break;
    case eof_tag:
      printf("<EOF>");
      break;
    case port_tag:
      printf("<port>");
      break;
    case primitive_tag:
      printf("<primitive %s>", prim_name(x));
      break;
    case cvar_tag:
      Cyc_display(Cyc_get_cvar(x));
      break;
    case boolean_tag:
      printf("#%s",((boolean_type *) x)->pname);
      break;
    case symbol_tag:
      printf("%s",((symbol_type *) x)->pname);
      break;
    case integer_tag:
      printf("%d", ((integer_type *) x)->value);
      break;
    case double_tag:
      printf("%lf", ((double_type *) x)->value);
      break;
    case string_tag:
      printf("%s", ((string_type *) x)->str);
      break;
    case cons_tag:
      has_cycle = Cyc_has_cycle(x);
      printf("("); 
      Cyc_display(car(x));

      // Experimenting with displaying lambda defs in REPL
      // not good enough but this is a start. would probably need
      // the same code in write()
      if (equal(quote_Cyc_191procedure, car(x))) {
          printf(" ");
          Cyc_display(cadr(x));
          printf(" ...)"); /* skip body and env for now */
          break;
      }

      for (tmp = cdr(x); tmp && ((closure) tmp)->tag == cons_tag; tmp = cdr(tmp)) {
          if (has_cycle == boolean_t) {
              if (i++ > 20) break; /* arbitrary number, for now */
          }
          printf(" ");
          Cyc_display(car(tmp));
      }
      if (has_cycle == boolean_t) {
          printf(" ...");
      } else if (tmp) {
          printf(" . ");
          Cyc_display(tmp);
      }
      printf(")");
      break;
    default:
      printf("Cyc_display: bad tag x=%ld\n", ((closure)x)->tag); getchar(); exit(0);}
 return x;}

static object _Cyc_write(x) object x;
{object tmp = nil;
 object has_cycle = boolean_f;
 int i = 0;
 if (nullp(x)) {printf("()"); return x;}
 if (obj_is_char(x)) {printf("#\\%c", obj_obj2char(x)); return x;}
 switch (type_of(x))
   {case string_tag:
      printf("\"%s\"", ((string_type *) x)->str);
      break;
    // TODO: what about a list? contents should be displayed per (write)
    case cons_tag:
      has_cycle = Cyc_has_cycle(x);
      printf("("); 
      _Cyc_write(car(x));

      // Experimenting with displaying lambda defs in REPL
      // not good enough but this is a start. would probably need
      // the same code in write()
      if (equal(quote_Cyc_191procedure, car(x))) {
          printf(" ");
          _Cyc_write(cadr(x));
          printf(" ...)"); /* skip body and env for now */
          break;
      }

      for (tmp = cdr(x); tmp && ((closure) tmp)->tag == cons_tag; tmp = cdr(tmp)) {
          if (has_cycle == boolean_t) {
              if (i++ > 20) break; /* arbitrary number, for now */
          }
          printf(" ");
          _Cyc_write(car(tmp));
      }
      if (has_cycle == boolean_t) {
          printf(" ...");
      } else if (tmp) {
          printf(" . ");
          _Cyc_write(tmp);
      }
      printf(")");
      break;
    default:
      Cyc_display(x);}
 return x;}

static object Cyc_write(x) object x;
{object y = _Cyc_write(x);
 printf("\n");
 return y;}

/* Some of these non-consing functions have been optimized from CPS. */

// TODO: should not be a predicate, may end up moving these to Scheme code
static object memberp(x,l) object x; list l;
{for (; !nullp(l); l = cdr(l)) if (boolean_f != equalp(x,car(l))) return boolean_t;
 return boolean_f;}

static object memqp(x,l) object x; list l;
{for (; !nullp(l); l = cdr(l)) if (eq(x,car(l))) return boolean_t;
 return boolean_f;}

static object get(x,i) object x,i;
{register object plist; register object plistd;
 if (nullp(x)) return x;
 if (type_of(x)!=symbol_tag) {printf("get: bad x=%ld\n",((closure)x)->tag); exit(0);}
 plist = symbol_plist(x);
 for (; !nullp(plist); plist = cdr(plistd))
   {plistd = cdr(plist);
    if (eq(car(plist),i)) return car(plistd);}
 return nil;}

static object equalp(x,y) object x,y;
{for (; ; x = cdr(x), y = cdr(y))
   {if (equal(x,y)) return boolean_t;
    if (obj_is_char(x) || obj_is_char(y) || 
        nullp(x) || nullp(y) ||
        type_of(x)!=cons_tag || type_of(y)!=cons_tag) return boolean_f;
    if (boolean_f == equalp(car(x),car(y))) return boolean_f;}}

static list assq(x,l) object x; list l;
{for (; !nullp(l); l = cdr(l))
   {register list la = car(l); if (eq(x,car(la))) return la;}
 return boolean_f;}

static list assoc(x,l) object x; list l;
{for (; !nullp(l); l = cdr(l))
   {register list la = car(l); if (boolean_f != equalp(x,car(la))) return la;}
 return boolean_f;}


// TODO: generate these using macros???
static object __num_eq(x, y) object x, y;
{if (x && y && ((integer_type *)x)->value == ((integer_type *)y)->value)
    return boolean_t;
 return boolean_f;}

static object __num_gt(x, y) object x, y;
{//printf("DEBUG cmp %d, x %d, y %d, x tag %d, y tag %d\n", 
 //   (((integer_type *)x)->value > ((integer_type *)y)->value),
 //   ((integer_type *)x)->value, ((integer_type *)y)->value,
 //   ((list)x)->tag, ((list)y)->tag);
 //exit(1);
 if (((integer_type *)x)->value > ((integer_type *)y)->value)
    return boolean_t;
 return boolean_f;}

static object __num_lt(x, y) object x, y;
{if (((integer_type *)x)->value < ((integer_type *)y)->value)
    return boolean_t;
 return boolean_f;}

static object __num_gte(x, y) object x, y;
{if (((integer_type *)x)->value >= ((integer_type *)y)->value)
    return boolean_t;
 return boolean_f;}

static object __num_lte(x, y) object x, y;
{if (((integer_type *)x)->value <= ((integer_type *)y)->value)
    return boolean_t;
 return boolean_f;}

// TODO: static object Cyc_is_eq(x, y) object x, y)
static object Cyc_is_boolean(object o){
    if (!nullp(o) && 
        !is_value_type(o) &&
        ((list)o)->tag == boolean_tag &&
        (eq(boolean_f, o) || eq(boolean_t, o)))
        return boolean_t;
    return boolean_f;}

static object Cyc_is_cons(object o){
    if (!nullp(o) && !is_value_type(o) && ((list)o)->tag == cons_tag)
        return boolean_t;
    return boolean_f;}

static object Cyc_is_null(object o){
    if (nullp(o)) 
        return boolean_t;
    return boolean_f;}

static object Cyc_is_number(object o){
    if (!nullp(o) && !is_value_type(o) && 
        (type_of(o) == integer_tag || type_of(o) == double_tag))
        return boolean_t;
    return boolean_f;}

static object Cyc_is_real(object o){
    return Cyc_is_number(o);}

static object Cyc_is_integer(object o){
    if (!nullp(o) && !is_value_type(o) && type_of(o) == integer_tag)
        return boolean_t;
    return boolean_f;}

static object Cyc_is_symbol(object o){
    if (!nullp(o) && !is_value_type(o) && ((list)o)->tag == symbol_tag)
        return boolean_t;
    return boolean_f;}

static object Cyc_is_string(object o){
    if (!nullp(o) && !is_value_type(o) && ((list)o)->tag == string_tag)
        return boolean_t;
    return boolean_f;}

static object Cyc_is_char(object o){
    if (obj_is_char(o))
        return boolean_t;
    return boolean_f;}

static object Cyc_is_procedure(object o) {
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
        }
    }
    return boolean_f;
}

static object Cyc_is_eof_object(object o) {
    if (!nullp(o) && !is_value_type(o) && type_of(o) == eof_tag)
        return boolean_t;
    return boolean_f;}

static object Cyc_is_cvar(object o) {
    if (!nullp(o) && !is_value_type(o) && type_of(o) == cvar_tag)
        return boolean_t;
    return boolean_f;}

static object Cyc_eq(object x, object y) {
    if (eq(x, y))
        return boolean_t;
    return boolean_f;
}

static object Cyc_set_car(object l, object val) {
    car(l) = val;
    add_mutation(l, val);
    return l;
}

static object Cyc_set_cdr(object l, object val) {
    cdr(l) = val;
    add_mutation(l, val);
    return l;
}

static integer_type Cyc_length(object l){
    make_int(len, 0);
    while(!nullp(l)){
        if (((list)l)->tag != cons_tag){
            printf("length - invalid parameter, expected list\n");
            exit(1);
        }
        l = cdr(l);
        len.value++;
    }
    return len;
}

static string_type Cyc_number2string(object n) {
    char buffer[1024];
    if (type_of(n) == integer_tag) {
        snprintf(buffer, 1024, "%d", ((integer_type *)n)->value);
    } else if (type_of(n) == double_tag) {
        snprintf(buffer, 1024, "%lf", ((double_type *)n)->value);
    } else {
        buffer[0] = '\0'; // TODO: throw error instead
    }
    make_string(str, buffer);
    return str;
}

static string_type Cyc_symbol2string(object sym) {
    make_string(str, symbol_pname(sym));
    return str;
}

static object Cyc_string2symbol(object str) {
    object sym = find_symbol_by_name(symbol_pname(str));
    if (!sym) {
        sym = add_symbol_by_name(symbol_pname(str));
    }
    return sym;
}

static string_type Cyc_list2string(object lst){
    char *buf;
    int i = 0;
    integer_type len = Cyc_length(lst); // Inefficient, walks whole list
    buf = alloca(sizeof(char) * (len.value + 1));

    while(!nullp(lst)){
        buf[i++] = obj_obj2char(car(lst));
        lst = cdr(lst);
    }
    buf[i] = '\0';

    make_string(str, buf);
    return str;
}

#define string2list(c,s) object c = nil; { \
  char *str = ((string_type *)s)->str; \
  int len = strlen(str); \
  cons_type *buf; \
  if (len > 0) { \
      buf = alloca(sizeof(cons_type) * len); \
      __string2list(str, buf, len); \
      c = (object)&(buf[0]); \
  } \
}

static void __string2list(const char *str, cons_type *buf, int buflen){
    int i = 0;
    while (str[i]){
        buf[i].tag = cons_tag;
        buf[i].cons_car = obj_char2obj(str[i]);
        buf[i].cons_cdr = (i == buflen - 1) ? nil : buf + (i + 1); 
        i++;
    }
}

static common_type Cyc_string2number(object str){
    common_type result;
    double n;
    if (type_of(str) == string_tag &&
        ((string_type *) str)->str){
        n = atof(((string_type *) str)->str);

        if (ceilf(n) == n) {
            result.integer_t.tag = integer_tag;
            result.integer_t.value = (int)n;
        }
        else {
            result.double_t.tag = double_tag;
            result.double_t.value = n;
        }
    } else {
        // TODO: not good enough because we do pointer comparisons to #f
        //result.boolean_t = boolean_f;
    }

    return result;
}

static void dispatch_string_91append(int argc, object clo, object cont, object str1, ...) {
    string_type result;
    va_list ap;
    va_start(ap, str1);
    result = Cyc_string_append_va_list(argc - 1, str1, ap);
    va_end(ap);
    return_funcall1(cont, &result);
}

static string_type Cyc_string_append(int argc, object str1, ...) {
    string_type result;
    va_list ap;
    va_start(ap, str1);
    result = Cyc_string_append_va_list(argc, str1, ap);
    va_end(ap);
    return result;
}

static string_type Cyc_string_append_va_list(int argc, object str1, va_list ap) {
    // TODO: one way to do this, perhaps not the most efficient:
    //   compute lengths of the strings,
    //   store lens and str ptrs
    //   allocate buffer, memcpy each str to buffer
    //   make_string using buffer

    int i = 0, total_len = 1; // for null char
    int *len = alloca(sizeof(int) * argc);
    char *buffer, *bufferp, **str = alloca(sizeof(char *) * argc);
    object tmp;
    
    if (argc > 0) {
      str[i] = ((string_type *)str1)->str;
      len[i] = strlen(str[i]);
      total_len += len[i];
    }

    for (i = 1; i < argc; i++) {
        tmp = va_arg(ap, object);
        str[i] = ((string_type *)tmp)->str;
        len[i] = strlen(str[i]);
        total_len += len[i];
    }

    buffer = bufferp = alloca(sizeof(char) * total_len);
    for (i = 0; i < argc; i++) {
        memcpy(bufferp, str[i], len[i]);
        bufferp += len[i];
    }
    *bufferp = '\0';
    make_string(result, buffer);
    return result;
}

static integer_type Cyc_char2integer(object chr){
    make_int(n, obj_obj2char(chr));
    return n;
}

static object Cyc_integer2char(object n){
    int val = 0;

    if (!nullp(n)) {
        val = ((integer_type *) n)->value;
    }

    return obj_char2obj(val);
}

static void my_exit(closure) never_returns;
static void my_exit(env) closure env; {
#if DEBUG_SHOW_DIAG
    printf("my_exit: heap bytes allocated=%d  time=%ld ticks  no_gcs=%ld no_m_gcs=%ld\n",
        allocp-bottom,clock()-start,no_gcs,no_major_gcs);
 printf("my_exit: ticks/second=%ld\n",(long) CLOCKS_PER_SEC);
#endif
 exit(0);}

static object __halt(object obj) {
#if DEBUG_SHOW_DIAG
    printf("\nhalt: ");
    Cyc_display(obj);
    printf("\n");
#endif
    my_exit(obj);
    return nil;
}

#define declare_num_op(FUNC, FUNC_OP, FUNC_APPLY, OP) \
static common_type FUNC_OP(object x, object y) { \
    common_type s; \
    int tx = type_of(x), ty = type_of(y); \
    s.double_t.tag = double_tag; \
    if (tx == integer_tag && ty == integer_tag) { \
        s.integer_t.tag = integer_tag; \
        s.integer_t.value = ((integer_type *)x)->value OP ((integer_type *)y)->value; \
    } else if (tx == double_tag && ty == integer_tag) { \
        s.double_t.value = ((double_type *)x)->value OP ((integer_type *)y)->value; \
    } else if (tx == integer_tag && ty == double_tag) { \
        s.double_t.value = ((integer_type *)x)->value OP ((double_type *)y)->value; \
    } else if (tx == double_tag && ty == double_tag) { \
        s.double_t.value = ((double_type *)x)->value OP ((double_type *)y)->value; \
    } else { \
        Cyc_rt_raise_msg("Bad argument type\n"); \
    } \
    return s; \
} \
static common_type FUNC(int argc, object n, ...) { \
    va_list ap; \
    va_start(ap, n); \
    common_type result = Cyc_num_op_va_list(argc, FUNC_OP, n, ap); \
    va_end(ap); \
    return result; \
} \
static void FUNC_APPLY(int argc, object clo, object cont, object n, ...) { \
    va_list ap; \
    va_start(ap, n); \
    common_type result = Cyc_num_op_va_list(argc - 1, FUNC_OP, n, ap); \
    va_end(ap); \
    return_funcall1(cont, &result); \
}

declare_num_op(Cyc_sum, Cyc_sum_op, dispatch_sum, +);
declare_num_op(Cyc_sub, Cyc_sub_op, dispatch_sub, -);
declare_num_op(Cyc_mul, Cyc_mul_op, dispatch_mul, *);
// TODO: what about divide-by-zero, and casting to double when
//       result contains a decimal component?
declare_num_op(Cyc_div, Cyc_div_op, dispatch_div, /);

static common_type Cyc_num_op_va_list(int argc, common_type (fn_op(object, object)), object n, va_list ns) {
  common_type sum;
  int i;
  if (argc == 0) {
    sum.integer_t.tag = integer_tag;
    sum.integer_t.value = 0;
    return sum;
  }

  if (type_of(n) == integer_tag) {
    sum.integer_t.tag = integer_tag;
    sum.integer_t.value = ((integer_type *)n)->value;
  } else if (type_of(n) == double_tag) {
    sum.double_t.tag = double_tag;
    sum.double_t.value = ((double_type *)n)->value;
  } else {
      Cyc_rt_raise_msg("Bad argument type");
  }

  for (i = 1; i < argc; i++) {
    common_type result = fn_op(&sum, va_arg(ns, object));
    if (type_of(&result) == integer_tag) {
        sum.integer_t.tag = integer_tag;
        sum.integer_t.value = ((integer_type *) &result)->value;
    } else if (type_of(&result) == double_tag) {
        sum.double_t.tag = double_tag;
        sum.double_t.value = ((double_type *) &result)->value;
    } else {
        Cyc_rt_raise_msg("Invalid tag in Cyc_num_op_va_list");
    }
  }

  return sum;
}

/* I/O functions */

static port_type Cyc_io_current_input_port() {
    make_port(p, stdin, 0);
    return p;
}

static port_type Cyc_io_open_input_file(object str) {
    const char *fname = ((string_type *)str)->str;
    make_port(p, NULL, 0);
    p.fp = fopen(fname, "r");
    return p;
}

static object Cyc_io_close_input_port(object port) {
    if (port && type_of(port) == port_tag) {
       FILE *stream = ((port_type *)port)->fp;
       if (stream) fclose(stream);
       ((port_type *)port)->fp = NULL;
    }
    return port;
}

//  TODO: port arg is optional! (maybe handle that in expansion section??)
static object Cyc_io_read_char(object port) {
    if (type_of(port) == port_tag) {
        int c = fgetc(((port_type *) port)->fp);
        if (c != EOF) {
            return obj_char2obj(c);
        }
    }
    return Cyc_EOF;
}

static object Cyc_io_peek_char(object port) {
    FILE *stream;
    int c;

    if (type_of(port) == port_tag) {
        stream = ((port_type *) port)->fp;
        c = fgetc(stream);
        ungetc(c, stream);
        if (c != EOF) {
            return obj_char2obj(c);
        }
    }
    return Cyc_EOF;
}

static void _Cyc_91global_91vars(object cont, object args){ 
    return_funcall1(cont, Cyc_global_variables); } 
static void _car(object cont, object args) { 
    return_funcall1(cont, car(car(args))); }
static void _cdr(object cont, object args) { 
    return_funcall1(cont, cdr(car(args))); }
static void _caar(object cont, object args) { 
    return_funcall1(cont, caar(car(args))); }
static void _cadr(object cont, object args) { 
    return_funcall1(cont, cadr(car(args))); }
static void _cdar(object cont, object args) { 
    return_funcall1(cont, cdar(car(args))); }
static void _cddr(object cont, object args) { 
    return_funcall1(cont, cddr(car(args))); }
static void _caaar(object cont, object args) { 
    return_funcall1(cont, caaar(car(args))); }
static void _caadr(object cont, object args) { 
    return_funcall1(cont, caadr(car(args))); }
static void _cadar(object cont, object args) { 
    return_funcall1(cont, cadar(car(args))); }
static void _caddr(object cont, object args) { 
    return_funcall1(cont, caddr(car(args))); }
static void _cdaar(object cont, object args) { 
    return_funcall1(cont, cdaar(car(args))); }
static void _cdadr(object cont, object args) { 
    return_funcall1(cont, cdadr(car(args))); }
static void _cddar(object cont, object args) { 
    return_funcall1(cont, cddar(car(args))); }
static void _cdddr(object cont, object args) { 
    return_funcall1(cont, cdddr(car(args))); }
static void _caaaar(object cont, object args) { 
    return_funcall1(cont, caaaar(car(args))); }
static void _caaadr(object cont, object args) { 
    return_funcall1(cont, caaadr(car(args))); }
static void _caadar(object cont, object args) { 
    return_funcall1(cont, caadar(car(args))); }
static void _caaddr(object cont, object args) { 
    return_funcall1(cont, caaddr(car(args))); }
static void _cadaar(object cont, object args) { 
    return_funcall1(cont, cadaar(car(args))); }
static void _cadadr(object cont, object args) { 
    return_funcall1(cont, cadadr(car(args))); }
static void _caddar(object cont, object args) { 
    return_funcall1(cont, caddar(car(args))); }
static void _cadddr(object cont, object args) { 
    return_funcall1(cont, cadddr(car(args))); }
static void _cdaaar(object cont, object args) { 
    return_funcall1(cont, cdaaar(car(args))); }
static void _cdaadr(object cont, object args) { 
    return_funcall1(cont, cdaadr(car(args))); }
static void _cdadar(object cont, object args) { 
    return_funcall1(cont, cdadar(car(args))); }
static void _cdaddr(object cont, object args) { 
    return_funcall1(cont, cdaddr(car(args))); }
static void _cddaar(object cont, object args) { 
    return_funcall1(cont, cddaar(car(args))); }
static void _cddadr(object cont, object args) { 
    return_funcall1(cont, cddadr(car(args))); }
static void _cdddar(object cont, object args) { 
    return_funcall1(cont, cdddar(car(args))); }
static void _cddddr(object cont, object args) { 
    return_funcall1(cont, cddddr(car(args))); }
static void _cons(object cont, object args) { 
    make_cons(c, car(args), cadr(args));
    return_funcall1(cont, &c); }
static void _eq_127(object cont, object args){ 
    return_funcall1(cont, Cyc_eq(car(args), cadr(args))); }
static void _eqv_127(object cont, object args){ 
    _eq_127(cont, args); }
static void _equal_127(object cont, object args){ 
    return_funcall1(cont, equalp(car(args), cadr(args))); }
static void _length(object cont, object args){ 
    integer_type i = Cyc_length(car(args));
    return_funcall1(cont, &i); }
static void _null_127(object cont, object args) { 
    return_funcall1(cont, Cyc_is_null(car(args))); }
static void _set_91car_67(object cont, object args) { 
    return_funcall1(cont, Cyc_set_car(car(args), cadr(args))); }
static void _set_91cdr_67(object cont, object args) { 
    return_funcall1(cont, Cyc_set_cdr(car(args), cadr(args))); }
static void _Cyc_91has_91cycle_127(object cont, object args) { 
    return_funcall1(cont, Cyc_has_cycle(car(args))); }
static void __87(object cont, object args) {
    integer_type argc = Cyc_length(args);
    dispatch(argc.value, (function_type)dispatch_sum, cont, cont, args); }
static void __91(object cont, object args) {
    integer_type argc = Cyc_length(args);
    dispatch(argc.value, (function_type)dispatch_sub, cont, cont, args); }
static void __85(object cont, object args) {
    integer_type argc = Cyc_length(args);
    dispatch(argc.value, (function_type)dispatch_mul, cont, cont, args); }
static void __95(object cont, object args) {
    integer_type argc = Cyc_length(args);
    dispatch(argc.value, (function_type)dispatch_div, cont, cont, args); }
static void _Cyc_91cvar_127(object cont, object args) {
    return_funcall1(cont, Cyc_is_cvar(car(args))); }
static void _boolean_127(object cont, object args) {
    return_funcall1(cont, Cyc_is_boolean(car(args))); }
static void _char_127(object cont, object args) {
    return_funcall1(cont, Cyc_is_char(car(args))); }
static void _eof_91object_127(object cont, object args) {
    return_funcall1(cont, Cyc_is_eof_object(car(args))); }
static void _number_127(object cont, object args) {
    return_funcall1(cont, Cyc_is_number(car(args))); }
static void _real_127(object cont, object args) {
    return_funcall1(cont, Cyc_is_real(car(args))); }
static void _integer_127(object cont, object args) {
    return_funcall1(cont, Cyc_is_integer(car(args))); }
static void _pair_127(object cont, object args) {
    return_funcall1(cont, Cyc_is_cons(car(args))); }
static void _procedure_127(object cont, object args) {
    return_funcall1(cont, Cyc_is_procedure(car(args))); }
static void _string_127(object cont, object args) {
    return_funcall1(cont, Cyc_is_string(car(args))); }
static void _symbol_127(object cont, object args) {
    return_funcall1(cont, Cyc_is_symbol(car(args))); }

static void _Cyc_91get_91cvar(object cont, object args) {  
    printf("not implemented\n"); exit(1); }
static void _Cyc_91set_91cvar_67(object cont, object args) {  
    printf("not implemented\n"); exit(1); }
/* Note we cannot use _exit (per convention) because it is reserved by C */
static void _cyc_exit(object cont, object args) {  
    if(nullp(args))
        __halt(nil);
    __halt(car(args));
}
static void __75halt(object cont, object args) {  
    printf("not implemented\n"); exit(1); }
static void _cell_91get(object cont, object args) {  
    printf("not implemented\n"); exit(1); }
static void _set_91global_67(object cont, object args) {  
    printf("not implemented\n"); exit(1); }
static void _set_91cell_67(object cont, object args) {  
    printf("not implemented\n"); exit(1); }
static void _cell(object cont, object args) {  
    printf("not implemented\n"); exit(1); }

static void __123(object cont, object args) {  
    return_funcall1(cont, __num_eq(car(args), cadr(args)));}
static void __125(object cont, object args) {  
    return_funcall1(cont, __num_gt(car(args), cadr(args)));}
static void __121(object cont, object args) {
    return_funcall1(cont, __num_lt(car(args), cadr(args)));}
static void __125_123(object cont, object args) {
    return_funcall1(cont, __num_gte(car(args), cadr(args)));}
static void __121_123(object cont, object args) {
    return_funcall1(cont, __num_lte(car(args), cadr(args)));}

static void _apply(object cont, object args) {  
    apply(cont, car(args), cdr(args)); }
static void _assoc (object cont, object args) {  
    return_funcall1(cont, assoc(car(args), cadr(args)));}
static void _assq  (object cont, object args) {  
    return_funcall1(cont, assq(car(args), cadr(args)));}
static void _assv  (object cont, object args) {  
    return_funcall1(cont, assq(car(args), cadr(args)));}
static void _member(object cont, object args) {  
    return_funcall1(cont, memberp(car(args), cadr(args)));}
static void _memq(object cont, object args) {  
    return_funcall1(cont, memqp(car(args), cadr(args)));}
static void _memv(object cont, object args) {  
    return_funcall1(cont, memqp(car(args), cadr(args)));}
static void _char_91_125integer(object cont, object args) {  
    integer_type i = Cyc_char2integer(car(args));
    return_funcall1(cont, &i);}
static void _integer_91_125char(object cont, object args) {  
    return_funcall1(cont, Cyc_integer2char(car(args)));}
static void _string_91_125number(object cont, object args) {  
    common_type i = Cyc_string2number(car(args));
    return_funcall1(cont, &i);}
//static void _error(object cont, object args) {
//    integer_type argc = Cyc_length(args);
//    dispatch_va(argc.value, dispatch_error, cont, cont, args); }
static void _Cyc_91current_91exception_91handler(object cont, object args) {
    object handler = Cyc_current_exception_handler();
    return_funcall1(cont, handler); }
static void _Cyc_91default_91exception_91handler(object cont, object args) {
    // TODO: this is a quick-and-dirty implementation, may be a better way to write this
    Cyc_default_exception_handler(1, args, car(args));
}
static void _string_91append(object cont, object args) {  
    integer_type argc = Cyc_length(args);
    dispatch(argc.value, (function_type)dispatch_string_91append, cont, cont, args); }
static void _string_91_125list(object cont, object args) {  
    string2list(lst, car(args));
    return_funcall1(cont, &lst);}
static void _list_91_125string(object cont, object args) {  
    string_type s = Cyc_list2string(car(args));
    return_funcall1(cont, &s);}
static void _string_91_125symbol(object cont, object args) {  
    return_funcall1(cont, Cyc_string2symbol(car(args)));}
static void _symbol_91_125string(object cont, object args) {  
    string_type s = Cyc_symbol2string(car(args));
    return_funcall1(cont, &s);}
static void _number_91_125string(object cont, object args) {  
    string_type s = Cyc_number2string(car(args));
    return_funcall1(cont, &s);}
static void _current_91input_91port(object cont, object args) {  
    port_type p = Cyc_io_current_input_port();
    return_funcall1(cont, &p);}
static void _open_91input_91file(object cont, object args) {  
    port_type p = Cyc_io_open_input_file(car(args));
    return_funcall1(cont, &p);}
static void _close_91input_91port(object cont, object args) {  
    return_funcall1(cont, Cyc_io_close_input_port(car(args)));}
static void _read_91char(object cont, object args) {  
    return_funcall1(cont, Cyc_io_read_char(car(args)));}
static void _peek_91char(object cont, object args) {  
    return_funcall1(cont, Cyc_io_peek_char(car(args)));}
static void _write(object cont, object args) {  
    return_funcall1(cont, Cyc_write(car(args))); }
static void _display(object cont, object args) {  
    return_funcall1(cont, Cyc_display(car(args)));}

#ifdef CYC_EVAL
static void _call_95cc(object cont, object args){
    return_funcall2(__glo_call_95cc, cont, car(args));
}
defprimitive(call_95cc, call/cc, &_call_95cc); // Moved up here due to ifdef
#endif /* CYC_EVAL */

/* This section is auto-generated via --autogen */
defprimitive(Cyc_91global_91vars, Cyc-global-vars, &_Cyc_91global_91vars); /* Cyc-global-vars */
defprimitive(Cyc_91get_91cvar, Cyc-get-cvar, &_Cyc_91get_91cvar); /* Cyc-get-cvar */
defprimitive(Cyc_91set_91cvar_67, Cyc-set-cvar!, &_Cyc_91set_91cvar_67); /* Cyc-set-cvar! */
defprimitive(Cyc_91cvar_127, Cyc-cvar?, &_Cyc_91cvar_127); /* Cyc-cvar? */
defprimitive(Cyc_91has_91cycle_127, Cyc-has-cycle?, &_Cyc_91has_91cycle_127); /* Cyc-has-cycle? */
defprimitive(_87, +, &__87); /* + */
defprimitive(_91, -, &__91); /* - */
defprimitive(_85, *, &__85); /* * */
defprimitive(_95, /, &__95); /* / */
defprimitive(_123, =, &__123); /* = */
defprimitive(_125, >, &__125); /* > */
defprimitive(_121, <, &__121); /* < */
defprimitive(_125_123, >=, &__125_123); /* >= */
defprimitive(_121_123, <=, &__121_123); /* <= */
defprimitive(apply, apply, &_apply); /* apply */
defprimitive(_75halt, %halt, &__75halt); /* %halt */
defprimitive(exit, exit, &_cyc_exit); /* exit */
//defprimitive(error, error, &_error); /* error */
defprimitive(
    Cyc_91current_91exception_91handler,
    Cyc_current_exception_handler,
    &_Cyc_91current_91exception_91handler); /* Cyc-current-exception-handler */
defprimitive(
    Cyc_91default_91exception_91handler,
    Cyc_default_exception_handler,
    &_Cyc_91default_91exception_91handler); /* Cyc-default-exception-handler */
defprimitive(cons, cons, &_cons); /* cons */
defprimitive(cell_91get, cell-get, &_cell_91get); /* cell-get */
defprimitive(set_91global_67, set-global!, &_set_91global_67); /* set-global! */
defprimitive(set_91cell_67, set-cell!, &_set_91cell_67); /* set-cell! */
defprimitive(cell, cell, &_cell); /* cell */
defprimitive(eq_127, eq?, &_eq_127); /* eq? */
defprimitive(eqv_127, eqv?, &_eqv_127); /* eqv? */
defprimitive(equal_127, equal?, &_equal_127); /* equal? */
defprimitive(assoc, assoc, &_assoc); /* assoc */
defprimitive(assq, assq, &_assq); /* assq */
defprimitive(assv, assv, &_assv); /* assq */
defprimitive(member, member, &_member); /* member */
defprimitive(memq, memq, &_memq); /* memq */
defprimitive(memv, memv, &_memv); /* memv */
defprimitive(length, length, &_length); /* length */
defprimitive(set_91car_67, set-car!, &_set_91car_67); /* set-car! */
defprimitive(set_91cdr_67, set-cdr!, &_set_91cdr_67); /* set-cdr! */
defprimitive(car, car, &_car); /* car */
defprimitive(cdr, cdr, &_cdr); /* cdr */
defprimitive(caar, caar, &_caar); /* caar */
defprimitive(cadr, cadr, &_cadr); /* cadr */
defprimitive(cdar, cdar, &_cdar); /* cdar */
defprimitive(cddr, cddr, &_cddr); /* cddr */
defprimitive(caaar, caaar, &_caaar); /* caaar */
defprimitive(caadr, caadr, &_caadr); /* caadr */
defprimitive(cadar, cadar, &_cadar); /* cadar */
defprimitive(caddr, caddr, &_caddr); /* caddr */
defprimitive(cdaar, cdaar, &_cdaar); /* cdaar */
defprimitive(cdadr, cdadr, &_cdadr); /* cdadr */
defprimitive(cddar, cddar, &_cddar); /* cddar */
defprimitive(cdddr, cdddr, &_cdddr); /* cdddr */
defprimitive(caaaar, caaaar, &_caaaar); /* caaaar */
defprimitive(caaadr, caaadr, &_caaadr); /* caaadr */
defprimitive(caadar, caadar, &_caadar); /* caadar */
defprimitive(caaddr, caaddr, &_caaddr); /* caaddr */
defprimitive(cadaar, cadaar, &_cadaar); /* cadaar */
defprimitive(cadadr, cadadr, &_cadadr); /* cadadr */
defprimitive(caddar, caddar, &_caddar); /* caddar */
defprimitive(cadddr, cadddr, &_cadddr); /* cadddr */
defprimitive(cdaaar, cdaaar, &_cdaaar); /* cdaaar */
defprimitive(cdaadr, cdaadr, &_cdaadr); /* cdaadr */
defprimitive(cdadar, cdadar, &_cdadar); /* cdadar */
defprimitive(cdaddr, cdaddr, &_cdaddr); /* cdaddr */
defprimitive(cddaar, cddaar, &_cddaar); /* cddaar */
defprimitive(cddadr, cddadr, &_cddadr); /* cddadr */
defprimitive(cdddar, cdddar, &_cdddar); /* cdddar */
defprimitive(cddddr, cddddr, &_cddddr); /* cddddr */
defprimitive(char_91_125integer, char->integer, &_char_91_125integer); /* char->integer */
defprimitive(integer_91_125char, integer->char, &_integer_91_125char); /* integer->char */
defprimitive(string_91_125number, string->number, &_string_91_125number); /* string->number */
defprimitive(string_91append, string-append, &_string_91append); /* string-append */
defprimitive(string_91_125list, string->list, &_string_91_125list); /* string->list */
defprimitive(list_91_125string, list->string, &_list_91_125string); /* list->string */
defprimitive(string_91_125symbol, string->symbol, &_string_91_125symbol); /* string->symbol */
defprimitive(symbol_91_125string, symbol->string, &_symbol_91_125string); /* symbol->string */
defprimitive(number_91_125string, number->string, &_number_91_125string); /* number->string */
defprimitive(boolean_127, boolean?, &_boolean_127); /* boolean? */
defprimitive(char_127, char?, &_char_127); /* char? */
defprimitive(eof_91object_127, eof-object?, &_eof_91object_127); /* eof-object? */
defprimitive(null_127, null?, &_null_127); /* null? */
defprimitive(number_127, number?, &_number_127); /* number? */
defprimitive(real_127, real?, &_real_127); /* real? */
defprimitive(integer_127, integer?, &_integer_127); /* integer? */
defprimitive(pair_127, pair?, &_pair_127); /* pair? */
defprimitive(procedure_127, procedure?, &_procedure_127); /* procedure? */
defprimitive(string_127, string?, &_string_127); /* string? */
defprimitive(symbol_127, symbol?, &_symbol_127); /* symbol? */
defprimitive(current_91input_91port, current-input-port, &_current_91input_91port); /* current-input-port */
defprimitive(open_91input_91file, open-input-file, &_open_91input_91file); /* open-input-file */
defprimitive(close_91input_91port, close-input-port, &_close_91input_91port); /* close-input-port */
defprimitive(read_91char, read-char, &_read_91char); /* read-char */
defprimitive(peek_91char, peek-char, &_peek_91char); /* peek-char */
defprimitive(write, write, &_write); /* write */
defprimitive(display, display, &_display); /* display */
/* -------------------------------------------- */

/* Exception handler */
object Cyc_exception_handler_stack = nil;

// Special case, use this one instead since we need it in the runtime
// This also seems to "shadow" the corresponding C var definition, as
// subsequent instances of it are replaced during preprocessing. Is that
// behavior portable? If not, will have to modify cgen to not emit the var.
#define __glo__85exception_91handler_91stack_85 Cyc_exception_handler_stack

static object Cyc_default_exception_handler(int argc, closure _, object err) {
    printf("Error: ");
    Cyc_display(err);
    printf("\n");
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
void Cyc_rt_raise(object err) {
    make_cons(c2, err, nil);
    make_cons(c1, boolean_f, &c2);
    make_cons(c0, &c1, nil);
    apply(nil, Cyc_current_exception_handler(), &c0);
    // Should never get here
    fprintf(stderr, "Internal error in Cyc_rt_raise\n");
    exit(1);
}
void Cyc_rt_raise_msg(const char *err) {
    make_string(s, err);
    Cyc_rt_raise(&s);
}

/* END exception handler */

/*
 *
 * @param cont - Continuation for the function to call into
 * @param func - Function to execute
 * @param args - A list of arguments to the function
 */
static object apply(object cont, object func, object args){
  common_type buf;

//printf("DEBUG apply: ");
//Cyc_display(args);
//printf("\n");
  if (!is_object_type(func)) {
     printf("Call of non-procedure: ");
     Cyc_display(func);
     exit(1);
  }

  switch(type_of(func)) {
    case primitive_tag:
      // TODO: should probably check arg counts and error out if needed
      ((primitive_type *)func)->fn(cont, args);
      break;
    case closure0_tag:
    case closure1_tag:
    case closure2_tag:
    case closure3_tag:
    case closure4_tag:
    case closureN_tag:
      buf.integer_t = Cyc_length(args);
      dispatch(buf.integer_t.value, ((closure)func)->fn, func, cont, args);
      break;

#ifdef CYC_EVAL
    case cons_tag:
    {
      make_cons(c, func, args);

      if (!nullp(func) && eq(quote_Cyc_191procedure, car(func))) {
          ((closure)__glo_eval)->fn(3, __glo_eval, cont, &c, nil);
      } else {
          printf("Unable to evaluate: ");
          Cyc_display(&c);
          printf("\n");
          exit(1);
      }
    }
#endif /* CYC_EVAL */
      
    default:
      printf("Invalid object type %ld\n", type_of(func));
      exit(1);
  }
  return nil; // Never reached
}

// Version of apply meant to be called from within compiled code
static void Cyc_apply(int argc, closure cont, object prim, ...){
    va_list ap;
    object tmp;
    int i;
    list args = alloca(sizeof(cons_type) * argc);
    
    va_start(ap, prim);

    for (i = 0; i < argc; i++) {
        tmp = va_arg(ap, object);
        args[i].tag = cons_tag;
        args[i].cons_car = tmp;
        args[i].cons_cdr = (i == (argc-1)) ? nil : &args[i + 1];
    }
    //printf("DEBUG applying primitive to ");
    //Cyc_display((object)&args[0]);
    //printf("\n");

    va_end(ap);
    apply(cont, prim, (object)&args[0]);
}
// END apply

/* Extract args from given array, assuming cont is the first arg in buf */
static void Cyc_apply_from_buf(int argc, object prim, object *buf) {
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
        args[i - 1].tag = cons_tag;
        args[i - 1].cons_car = buf[i];
        args[i - 1].cons_cdr = (i == (argc-1)) ? nil : &args[i];
    }

    apply(cont, prim, (object)&args[0]);
}

static char *transport(x, gcgen) char *x; int gcgen;
/* Transport one object.  WARNING: x cannot be nil!!! */
{
 if (nullp(x)) return x;
 if (obj_is_char(x)) return x;
#if DEBUG_GC
 printf("entered transport ");
 printf("transport %ld\n", type_of(x));
#endif
 switch (type_of(x))
   {case cons_tag:
      {register list nx = (list) allocp;
       type_of(nx) = cons_tag; car(nx) = car(x); cdr(nx) = cdr(x);
       forward(x) = nx; type_of(x) = forward_tag;
       allocp = ((char *) nx)+sizeof(cons_type);
       return (char *) nx;}
    case closure0_tag:
      {register closure0 nx = (closure0) allocp;
       type_of(nx) = closure0_tag; nx->fn = ((closure0) x)->fn;
       forward(x) = nx; type_of(x) = forward_tag;
       allocp = ((char *) nx)+sizeof(closure0_type);
       return (char *) nx;}
    case closure1_tag:
      {register closure1 nx = (closure1) allocp;
       type_of(nx) = closure1_tag; nx->fn = ((closure1) x)->fn;
       nx->elt1 = ((closure1) x)->elt1;
       forward(x) = nx; type_of(x) = forward_tag;
       x = (char *) nx; allocp = ((char *) nx)+sizeof(closure1_type);
       return (char *) nx;}
    case closure2_tag:
      {register closure2 nx = (closure2) allocp;
       type_of(nx) = closure2_tag; nx->fn = ((closure2) x)->fn;
       nx->elt1 = ((closure2) x)->elt1;
       nx->elt2 = ((closure2) x)->elt2;
       forward(x) = nx; type_of(x) = forward_tag;
       x = (char *) nx; allocp = ((char *) nx)+sizeof(closure2_type);
       return (char *) nx;}
    case closure3_tag:
      {register closure3 nx = (closure3) allocp;
       type_of(nx) = closure3_tag; nx->fn = ((closure3) x)->fn;
       nx->elt1 = ((closure3) x)->elt1;
       nx->elt2 = ((closure3) x)->elt2;
       nx->elt3 = ((closure3) x)->elt3;
       forward(x) = nx; type_of(x) = forward_tag;
       x = (char *) nx; allocp = ((char *) nx)+sizeof(closure3_type);
       return (char *) nx;}
    case closure4_tag:
      {register closure4 nx = (closure4) allocp;
       type_of(nx) = closure4_tag; nx->fn = ((closure4) x)->fn;
       nx->elt1 = ((closure4) x)->elt1;
       nx->elt2 = ((closure4) x)->elt2;
       nx->elt3 = ((closure4) x)->elt3;
       nx->elt4 = ((closure4) x)->elt4;
       forward(x) = nx; type_of(x) = forward_tag;
       x = (char *) nx; allocp = ((char *) nx)+sizeof(closure4_type);
       return (char *) nx;}
    case closureN_tag:
      {register closureN nx = (closureN) allocp;
       int i;
       type_of(nx) = closureN_tag; nx->fn = ((closureN) x)->fn;
       nx->num_elt = ((closureN) x)->num_elt;
       nx->elts = (object *)(((char *)nx) + sizeof(closureN_type));
       for (i = 0; i < nx->num_elt; i++) {
         nx->elts[i] = ((closureN) x)->elts[i];
       }
       forward(x) = nx; type_of(x) = forward_tag;
       x = (char *) nx; allocp = ((char *) nx)+sizeof(closureN_type) + sizeof(object) * nx->num_elt;
       return (char *) nx;}
    case string_tag:
      {register string_type *nx = (string_type *) allocp;
       type_of(nx) = string_tag; 
       if (gcgen == 0) {
         // Minor, data heap is not relocated
         nx->str = ((string_type *)x)->str;
       } else {
         // Major collection, data heap is moving
         nx->str = dhallocp;
         int len = strlen(((string_type *) x)->str);
         memcpy(dhallocp, ((string_type *) x)->str, len + 1);
         dhallocp += len + 1;
       }
       forward(x) = nx; type_of(x) = forward_tag;
       x = (char *) nx; allocp = ((char *) nx)+sizeof(integer_type);
       return (char *) nx;}
    case integer_tag:
      {register integer_type *nx = (integer_type *) allocp;
       type_of(nx) = integer_tag; nx->value = ((integer_type *) x)->value;
       forward(x) = nx; type_of(x) = forward_tag;
       x = (char *) nx; allocp = ((char *) nx)+sizeof(integer_type);
       return (char *) nx;}
    case double_tag:
      {register double_type *nx = (double_type *) allocp;
       type_of(nx) = double_tag; nx->value = ((double_type *) x)->value;
       forward(x) = nx; type_of(x) = forward_tag;
       x = (char *) nx; allocp = ((char *) nx)+sizeof(double_type);
       return (char *) nx;}
    case port_tag:
      {register port_type *nx = (port_type *) allocp;
       type_of(nx) = port_tag; nx->fp = ((port_type *) x)->fp;
       nx->mode = ((port_type *) x)->mode;
       forward(x) = nx; type_of(x) = forward_tag;
       x = (char *) nx; allocp = ((char *) nx)+sizeof(port_type);
       return (char *) nx;}
    case cvar_tag:
      {register cvar_type *nx = (cvar_type *) allocp;
       type_of(nx) = cvar_tag; nx->pvar = ((cvar_type *) x)->pvar;
       forward(x) = nx; type_of(x) = forward_tag;
       x = (char *) nx; allocp = ((char *) nx)+sizeof(cvar_type);
       return (char *) nx;}
    case forward_tag:
       return (char *) forward(x);
    case eof_tag: break;
    case primitive_tag: break;
    case boolean_tag: break;
    case symbol_tag: break; // JAE TODO: raise an error here? Should not be possible in real code, though (IE, without GC DEBUG flag)
    default:
      printf("transport: bad tag x=%p x.tag=%ld\n",(void *)x,type_of(x)); exit(0);}
 return x;}

/* Use overflow macro which already knows which way the stack goes. */
/* Major collection, transport objects on stack or old heap */
#define transp(p) \
temp = (p); \
if ((check_overflow(low_limit,temp) && \
     check_overflow(temp,high_limit)) || \
    (check_overflow(old_heap_low_limit - 1, temp) && \
     check_overflow(temp,old_heap_high_limit + 1))) \
   (p) = (object) transport(temp,major);

static void GC_loop(int major, closure cont, object *ans, int num_ans)
{char foo;
 int i;
 register object temp;
 register object low_limit = &foo; /* Move live data above us. */
 register object high_limit = stack_begin;
 register char *scanp = allocp; /* Cheney scan pointer. */
 register object old_heap_low_limit = low_limit; // Minor-GC default
 register object old_heap_high_limit = high_limit; // Minor-GC default

 char *tmp_bottom = bottom;    /* Bottom of tospace. */
 char *tmp_allocp = allocp;    /* Cheney allocate pointer. */
 char *tmp_alloc_end = alloc_end;
 char *tmp_dhbottom = dhbottom;
 char *tmp_dhallocp = dhallocp;
 char *tmp_dhallocp_end = dhalloc_end;
 if (major) {
    // Initialize new heap (TODO: make a function for this)
    bottom = calloc(1,global_heap_size);
    allocp = (char *) ((((long) bottom)+7) & -8);
    alloc_end = allocp + global_heap_size - 8;
    scanp = allocp;
    old_heap_low_limit = tmp_bottom;
    old_heap_high_limit = tmp_alloc_end;
    
    dhallocp = dhbottom = calloc(1, global_heap_size);
    dhalloc_end = dhallocp + global_heap_size - 8;
 }

#if DEBUG_GC
 printf("\n=== started GC type = %d === \n", major);
#endif
 /* Transport GC's continuation and its argument. */
 transp(cont);
 gc_cont = cont;
 gc_num_ans = num_ans;
#if DEBUG_GC
 printf("DEBUG done transporting cont\n");
#endif

 /* Prevent overrunning buffer */
 if (num_ans > NUM_GC_ANS) {
   printf("Fatal error - too many arguments (%d) to GC\n", num_ans);
   exit(1);
 }

 for (i = 0; i < num_ans; i++){ 
     transp(ans[i]);
     gc_ans[i] = ans[i];
 }
#if DEBUG_GC
 printf("DEBUG done transporting gc_ans\n");
#endif

 /* Transport mutations. */
 {
   list l;
   for (l = mutation_table; !nullp(l); l = cdr(l)) {
     object o = car(l);
     if (type_of(o) == cons_tag) {
         // Transport, if necessary
         // TODO: need to test this with major GC, and 
         //       GC's of list/car-cdr from same generation
         transp(car(o));
         transp(cdr(o));
     } else if (type_of(o) == forward_tag) {
         // Already transported, skip
     } else {
         printf("Unexpected type %ld transporting mutation\n", type_of(o));
         exit(1);
     }
   }
 }
 clear_mutations(); /* Reset for next time */

 /* Transport global variables. */
 transp(Cyc_global_variables); /* Internal global used by the runtime */
 GC_GLOBALS
 while (scanp<allocp)       /* Scan the newspace. */
   switch (type_of(scanp))
     {case cons_tag:
#if DEBUG_GC
 printf("DEBUG transport cons_tag\n");
#endif
        transp(car(scanp)); transp(cdr(scanp));
        scanp += sizeof(cons_type); break;
      case closure0_tag:
#if DEBUG_GC
 printf("DEBUG transport closure0 \n");
#endif
        scanp += sizeof(closure0_type); break;
      case closure1_tag:
#if DEBUG_GC
 printf("DEBUG transport closure1 \n");
#endif
        transp(((closure1) scanp)->elt1);
        scanp += sizeof(closure1_type); break;
      case closure2_tag:
#if DEBUG_GC
 printf("DEBUG transport closure2 \n");
#endif
        transp(((closure2) scanp)->elt1); transp(((closure2) scanp)->elt2);
        scanp += sizeof(closure2_type); break;
      case closure3_tag:
#if DEBUG_GC
 printf("DEBUG transport closure3 \n");
#endif
        transp(((closure3) scanp)->elt1); transp(((closure3) scanp)->elt2);
        transp(((closure3) scanp)->elt3);
        scanp += sizeof(closure3_type); break;
      case closure4_tag:
#if DEBUG_GC
 printf("DEBUG transport closure4 \n");
#endif
        transp(((closure4) scanp)->elt1); transp(((closure4) scanp)->elt2);
        transp(((closure4) scanp)->elt3); transp(((closure4) scanp)->elt4);
        scanp += sizeof(closure4_type); break;
      case closureN_tag:
#if DEBUG_GC
 printf("DEBUG transport closureN \n");
#endif
       {int i; int n = ((closureN) scanp)->num_elt;
        for (i = 0; i < n; i++) {
          transp(((closureN) scanp)->elts[i]);
        }
        scanp += sizeof(closureN_type) + sizeof(object) * n;
       }
       break;
      case string_tag:
#if DEBUG_GC
 printf("DEBUG transport string \n");
#endif
        scanp += sizeof(string_type); break;
      case integer_tag:
#if DEBUG_GC
 printf("DEBUG transport integer \n");
#endif
        scanp += sizeof(integer_type); break;
      case double_tag:
#if DEBUG_GC
 printf("DEBUG transport double \n");
#endif
        scanp += sizeof(double_type); break;
      case port_tag:
#if DEBUG_GC
 printf("DEBUG transport port \n");
#endif
        scanp += sizeof(port_type); break;
      case cvar_tag:
#if DEBUG_GC
 printf("DEBUG transport cvar \n");
#endif
        scanp += sizeof(cvar_type); break;
      case eof_tag:
      case primitive_tag:
      case symbol_tag: 
      case boolean_tag:
      default:
        printf("GC: bad tag scanp=%p scanp.tag=%ld\n",(void *)scanp,type_of(scanp));
        exit(0);}

 if (major) {
     free(tmp_bottom);
     free(tmp_dhbottom);
 }
}

static void GC(cont,ans,num_ans) closure cont; object *ans; int num_ans;
{
 /* Only room for one more minor-GC, so do a major one.
  * Not sure this is the best strategy, it may be better to do major
  * ones sooner, perhaps after every x minor GC's.
  *
  * Also may need to consider dynamically increasing heap size, but
  * by how much (1.3x, 1.5x, etc) and when? I suppose when heap usage
  * after a collection is above a certain percentage, then it would be 
  * necessary to increase heap size the next time.
  */
 if (allocp >= (bottom + (global_heap_size - global_stack_size))) {
     //printf("Possibly only room for one more minor GC. no_gcs = %ld\n", no_gcs);
     no_major_gcs++;
     GC_loop(1, cont, ans, num_ans);
 } else {
     no_gcs++; /* Count the number of minor GC's. */
     GC_loop(0, cont, ans, num_ans);
 }

 /* You have to let it all go, Neo. Fear, doubt, and disbelief. Free your mind... */
 longjmp(jmp_main,1); /* Return globals gc_cont, gc_ans. */
}

/* This heap cons is used only for initialization. */
static list mcons(a,d) object a,d;
{register cons_type *c = malloc(sizeof(cons_type));
 c->tag = cons_tag; c->cons_car = a; c->cons_cdr = d;
 return c;}

static void c_entry_pt(int,closure,closure);
static void main_main (stack_size,heap_size,stack_base)
     long stack_size,heap_size; char *stack_base;
{char in_my_frame;
 mclosure0(clos_exit,&my_exit);  /* Create a closure for exit function. */
 gc_ans[0] = &clos_exit;            /* It becomes the argument to test. */
 gc_num_ans = 1;
 /* Allocate stack buffer. */
 stack_begin = stack_base;
#if STACK_GROWS_DOWNWARD
 stack_limit1 = stack_begin - stack_size;
 stack_limit2 = stack_limit1 - 2000;
#else
 stack_limit1 = stack_begin + stack_size;
 stack_limit2 = stack_limit1 + 2000;
#endif
#if DEBUG_SHOW_DIAG
 printf("main: sizeof(cons_type)=%ld\n",(long) sizeof(cons_type));
#endif
 if (check_overflow(stack_base,&in_my_frame))
   {printf("main: Recompile with STACK_GROWS_DOWNWARD set to %ld\n",
           (long) (1-STACK_GROWS_DOWNWARD)); exit(0);}
#if DEBUG_SHOW_DIAG
 printf("main: stack_size=%ld  stack_base=%p  stack_limit1=%p\n",
        stack_size,(void *)stack_base,(void *)stack_limit1);
 printf("main: Try different stack sizes from 4 K to 1 Meg.\n");
#endif
 /* Do initializations of Lisp objects and rewrite rules.
 quote_list_f = mlist1(boolean_f); quote_list_t = mlist1(boolean_t); */

 /* Make temporary short names for certain atoms. */
 {

  /* Define the rules, but only those that are actually referenced. */

  /* Create closure for the test function. */
  mclosure0(run_test,&c_entry_pt);
  gc_cont = &run_test;
  /* Initialize constant expressions for the test runs. */

  /* Allocate heap area for second generation. */
  /* Use calloc instead of malloc to assure pages are in main memory. */
#if DEBUG_SHOW_DIAG
  printf("main: Allocating and initializing heap...\n");
#endif
  bottom = calloc(1,heap_size);
  allocp = (char *) ((((long) bottom)+7) & -8);
  alloc_end = allocp + heap_size - 8;
  
  dhallocp = dhbottom = calloc(1, heap_size);
  dhalloc_end = dhallocp + heap_size - 8;
#if DEBUG_SHOW_DIAG
  printf("main: heap_size=%ld  allocp=%p  alloc_end=%p\n",
         (long) heap_size,(void *)allocp,(void *)alloc_end);
  printf("main: Try a larger heap_size if program bombs.\n");
  printf("Starting...\n");
#endif
  start = clock(); /* Start the timing clock. */

  /* Tank, load the jump program... */
  setjmp(jmp_main);
#if DEBUG_GC
  printf("Done with GC\n");
#endif

  if (type_of(gc_cont) == cons_tag || prim(gc_cont)) {
    Cyc_apply_from_buf(gc_num_ans, gc_cont, gc_ans);
  } else {
    do_dispatch(gc_num_ans, ((closure)gc_cont)->fn, gc_cont, gc_ans);
  }

  /*                                                                      */
  printf("main: your setjmp and/or longjmp are broken.\n"); exit(0);}}

static long long_arg(argc,argv,name,dval)
     int argc; char **argv; char *name; long dval;
{int j;
 for(j=1;(j+1)<argc;j += 2)
   if (strcmp(name,argv[j]) == 0)
     return(atol(argv[j+1]));
 return(dval);}

#endif /* CYCLONE_RUNTIME_H */

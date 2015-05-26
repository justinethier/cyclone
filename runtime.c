/** 
 * Cyclone Scheme
 * Copyright (c) 2014, Justin Ethier
 * All rights reserved.
 *
 * This file contains the C runtime used by compiled programs.
 */

#include "cyclone.h"
#include "runtime.h"

/* Funcall section, these are hardcoded here to support
   functions in this module. */
#define funcall1(cfn,a1) if (type_of(cfn) == cons_tag || prim(cfn)) { Cyc_apply(0, (closure)a1, cfn); } else { ((cfn)->fn)(1,cfn,a1);}
/* Return to continuation after checking for stack overflow. */
#define return_funcall1(cfn,a1) \
{char stack; \
 if (check_overflow(&stack,stack_limit1)) { \
     object buf[1]; buf[0] = a1;\
     GC(cfn,buf,1); return; \
 } else {funcall1((closure) (cfn),a1); return;}}
#define funcall2(cfn,a1,a2) if (type_of(cfn) == cons_tag || prim(cfn)) { Cyc_apply(1, (closure)a1, cfn,a2); } else { ((cfn)->fn)(2,cfn,a1,a2);}
/* Return to continuation after checking for stack overflow. */
#define return_funcall2(cfn,a1,a2) \
{char stack; \
 if (check_overflow(&stack,stack_limit1)) { \
     object buf[2]; buf[0] = a1;buf[1] = a2;\
     GC(cfn,buf,2); return; \
 } else {funcall2((closure) (cfn),a1,a2); return;}}
/*END funcall section */

/* Global variables. */
clock_t start;   /* Starting time. */
char *stack_begin;   /* Initialized by main. */
char *stack_limit1;  /* Initialized by main. */
char *stack_limit2;
char *bottom;    /* Bottom of tospace. */
char *allocp;    /* Cheney allocate pointer. */
char *alloc_end;
/* TODO: not sure this is the best strategy for strings, especially if there 
   are a lot of long, later gen strings because that will cause a lot of
   copying to occur during GC */
char *dhbottom; /* Bottom of data heap */
char *dhallocp; /* Current place in data heap */
char *dhalloc_end;
long no_gcs = 0; /* Count the number of GC's. */
long no_major_gcs = 0; /* Count the number of GC's. */
object gc_cont;   /* GC continuation closure. */
object gc_ans[NUM_GC_ANS];    /* argument for GC continuation closure. */
int gc_num_ans;
jmp_buf jmp_main; /* Where to jump to. */

//static object test_exp1, test_exp2; /* Expressions used within test. */
object Cyc_global_variables = nil;

static symbol_type __EOF = {eof_tag, "", nil}; // symbol_type in lieu of custom type
const object Cyc_EOF = &__EOF;

object cell_get(object cell){
    return car(cell);
}

static boolean_type t_boolean = {boolean_tag, "t"};
static boolean_type f_boolean = {boolean_tag, "f"};
const object boolean_t = &t_boolean;
const object boolean_f = &f_boolean;

static symbol_type Cyc_191procedure_symbol = {symbol_tag, "procedure", nil};
const object quote_Cyc_191procedure = &Cyc_191procedure_symbol;

/* Symbol Table */

/* Notes for the symbol table

 string->symbol can:
  - lookup symbol in the table
  - if found, return that pointer
  - otherwise, allocate symbol in table and return ptr to it

 For now, GC of symbols is missing. long-term it probably would be desirable
*/
list symbol_table = nil;

char *_strdup (const char *s) {
    char *d = malloc (strlen (s) + 1);
    if (d) { strcpy (d,s); }
    return d;
}

object find_symbol_by_name(const char *name) {
  list l = symbol_table;
  for (; !nullp(l); l = cdr(l)) {
    const char *str = symbol_pname(car(l));
    if (strcmp(str, name) == 0) return car(l);
  }
  return nil;
}

object add_symbol(symbol_type *psym) {
  symbol_table = mcons(psym, symbol_table);
  return psym;
}

object add_symbol_by_name(const char *name) {
  symbol_type sym = {symbol_tag, _strdup(name), nil};
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

object Cyc_default_exception_handler(int argc, closure _, object err) {
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

object terpri() {printf("\n"); return nil;}

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

object Cyc_display(x) object x;
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

object Cyc_write(x) object x;
{object y = _Cyc_write(x);
 printf("\n");
 return y;}

/* Some of these non-consing functions have been optimized from CPS. */

// TODO: should not be a predicate, may end up moving these to Scheme code
object memberp(x,l) object x; list l;
{for (; !nullp(l); l = cdr(l)) if (boolean_f != equalp(x,car(l))) return boolean_t;
 return boolean_f;}

object memqp(x,l) object x; list l;
{for (; !nullp(l); l = cdr(l)) if (eq(x,car(l))) return boolean_t;
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

list assq(x,l) object x; list l;
{for (; !nullp(l); l = cdr(l))
   {register list la = car(l); if (eq(x,car(la))) return la;}
 return boolean_f;}

list assoc(x,l) object x; list l;
{if (nullp(l) || is_value_type(l) || type_of(l) != cons_tag) return boolean_f;
 for (; !nullp(l); l = cdr(l))
   {register list la = car(l); if (boolean_f != equalp(x,car(la))) return la;}
 return boolean_f;}


// TODO: generate these using macros???
object __num_eq(x, y) object x, y;
{if (x && y && ((integer_type *)x)->value == ((integer_type *)y)->value)
    return boolean_t;
 return boolean_f;}

object __num_gt(x, y) object x, y;
{//printf("DEBUG cmp %d, x %d, y %d, x tag %d, y tag %d\n", 
 //   (((integer_type *)x)->value > ((integer_type *)y)->value),
 //   ((integer_type *)x)->value, ((integer_type *)y)->value,
 //   ((list)x)->tag, ((list)y)->tag);
 //exit(1);
 if (((integer_type *)x)->value > ((integer_type *)y)->value)
    return boolean_t;
 return boolean_f;}

object __num_lt(x, y) object x, y;
{if (((integer_type *)x)->value < ((integer_type *)y)->value)
    return boolean_t;
 return boolean_f;}

object __num_gte(x, y) object x, y;
{if (((integer_type *)x)->value >= ((integer_type *)y)->value)
    return boolean_t;
 return boolean_f;}

object __num_lte(x, y) object x, y;
{if (((integer_type *)x)->value <= ((integer_type *)y)->value)
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

object Cyc_is_string(object o){
    if (!nullp(o) && !is_value_type(o) && ((list)o)->tag == string_tag)
        return boolean_t;
    return boolean_f;}

object Cyc_is_char(object o){
    if (obj_is_char(o))
        return boolean_t;
    return boolean_f;}

object Cyc_is_procedure(object o) {
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

object Cyc_set_car(object l, object val) {
    car(l) = val;
    add_mutation(l, val);
    return l;
}

object Cyc_set_cdr(object l, object val) {
    cdr(l) = val;
    add_mutation(l, val);
    return l;
}

integer_type Cyc_length(object l){
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

string_type Cyc_number2string(object n) {
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

string_type Cyc_symbol2string(object sym) {
    make_string(str, symbol_pname(sym));
    return str;
}

object Cyc_string2symbol(object str) {
    object sym = find_symbol_by_name(symbol_pname(str));
    if (!sym) {
        sym = add_symbol_by_name(symbol_pname(str));
    }
    return sym;
}

string_type Cyc_list2string(object lst){
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

void __string2list(const char *str, cons_type *buf, int buflen){
    int i = 0;
    while (str[i]){
        buf[i].tag = cons_tag;
        buf[i].cons_car = obj_char2obj(str[i]);
        buf[i].cons_cdr = (i == buflen - 1) ? nil : buf + (i + 1); 
        i++;
    }
}

common_type Cyc_string2number(object str){
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

void dispatch_string_91append(int argc, object clo, object cont, object str1, ...) {
    string_type result;
    va_list ap;
    va_start(ap, str1);
    result = Cyc_string_append_va_list(argc - 1, str1, ap);
    va_end(ap);
    return_funcall1(cont, &result);
}

string_type Cyc_string_append(int argc, object str1, ...) {
    string_type result;
    va_list ap;
    va_start(ap, str1);
    result = Cyc_string_append_va_list(argc, str1, ap);
    va_end(ap);
    return result;
}

string_type Cyc_string_append_va_list(int argc, object str1, va_list ap) {
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

object Cyc_integer2char(object n){
    int val = 0;

    if (!nullp(n)) {
        val = ((integer_type *) n)->value;
    }

    return obj_char2obj(val);
}

void my_exit(closure) never_returns;
void my_exit(env) closure env; {
#if DEBUG_SHOW_DIAG
    printf("my_exit: heap bytes allocated=%d  time=%ld ticks  no_gcs=%ld no_m_gcs=%ld\n",
        allocp-bottom,clock()-start,no_gcs,no_major_gcs);
 printf("my_exit: ticks/second=%ld\n",(long) CLOCKS_PER_SEC);
#endif
 exit(0);}

object __halt(object obj) {
#if DEBUG_SHOW_DIAG
    printf("\nhalt: ");
    Cyc_display(obj);
    printf("\n");
#endif
    my_exit(obj);
    return nil;
}

#define declare_num_op(FUNC, FUNC_OP, FUNC_APPLY, OP) \
common_type FUNC_OP(object x, object y) { \
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
        make_string(s, "Bad argument type"); \
        make_cons(c1, y, nil); \
        make_cons(c0, &s, &c1); \
        Cyc_rt_raise(&c0); \
    } \
    return s; \
} \
common_type FUNC(int argc, object n, ...) { \
    va_list ap; \
    va_start(ap, n); \
    common_type result = Cyc_num_op_va_list(argc, FUNC_OP, n, ap); \
    va_end(ap); \
    return result; \
} \
void FUNC_APPLY(int argc, object clo, object cont, object n, ...) { \
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

common_type Cyc_num_op_va_list(int argc, common_type (fn_op(object, object)), object n, va_list ns) {
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
      make_string(s, "Bad argument type");
      make_cons(c1, n, nil);
      make_cons(c0, &s, &c1);
      Cyc_rt_raise(&c0);
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
        Cyc_rt_raise_msg("Internal error, invalid tag in Cyc_num_op_va_list");
    }
  }

  return sum;
}

/* I/O functions */

port_type Cyc_io_current_input_port() {
    make_port(p, stdin, 0);
    return p;
}

port_type Cyc_io_open_input_file(object str) {
    const char *fname = ((string_type *)str)->str;
    make_port(p, NULL, 0);
    p.fp = fopen(fname, "r");
    return p;
}

object Cyc_io_close_input_port(object port) {
    if (port && type_of(port) == port_tag) {
       FILE *stream = ((port_type *)port)->fp;
       if (stream) fclose(stream);
       ((port_type *)port)->fp = NULL;
    }
    return port;
}

//  TODO: port arg is optional! (maybe handle that in expansion section??)
object Cyc_io_read_char(object port) {
    if (type_of(port) == port_tag) {
        int c = fgetc(((port_type *) port)->fp);
        if (c != EOF) {
            return obj_char2obj(c);
        }
    }
    return Cyc_EOF;
}

object Cyc_io_peek_char(object port) {
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

/* This heap cons is used only for initialization. */
list mcons(a,d) object a,d;
{register cons_type *c = malloc(sizeof(cons_type));
 c->tag = cons_tag; c->cons_car = a; c->cons_cdr = d;
 return c;}

cvar_type *mcvar(object *var) {
  cvar_type *c = malloc(sizeof(cvar_type));
  c->tag = cvar_tag; 
  c->pvar = var;
  return c;}

void _Cyc_91global_91vars(object cont, object args){ 
    return_funcall1(cont, Cyc_global_variables); } 
void _car(object cont, object args) { 
    return_funcall1(cont, car(car(args))); }
void _cdr(object cont, object args) { 
    return_funcall1(cont, cdr(car(args))); }
void _caar(object cont, object args) { 
    return_funcall1(cont, caar(car(args))); }
void _cadr(object cont, object args) { 
    return_funcall1(cont, cadr(car(args))); }
void _cdar(object cont, object args) { 
    return_funcall1(cont, cdar(car(args))); }
void _cddr(object cont, object args) { 
    return_funcall1(cont, cddr(car(args))); }
void _caaar(object cont, object args) { 
    return_funcall1(cont, caaar(car(args))); }
void _caadr(object cont, object args) { 
    return_funcall1(cont, caadr(car(args))); }
void _cadar(object cont, object args) { 
    return_funcall1(cont, cadar(car(args))); }
void _caddr(object cont, object args) { 
    return_funcall1(cont, caddr(car(args))); }
void _cdaar(object cont, object args) { 
    return_funcall1(cont, cdaar(car(args))); }
void _cdadr(object cont, object args) { 
    return_funcall1(cont, cdadr(car(args))); }
void _cddar(object cont, object args) { 
    return_funcall1(cont, cddar(car(args))); }
void _cdddr(object cont, object args) { 
    return_funcall1(cont, cdddr(car(args))); }
void _caaaar(object cont, object args) { 
    return_funcall1(cont, caaaar(car(args))); }
void _caaadr(object cont, object args) { 
    return_funcall1(cont, caaadr(car(args))); }
void _caadar(object cont, object args) { 
    return_funcall1(cont, caadar(car(args))); }
void _caaddr(object cont, object args) { 
    return_funcall1(cont, caaddr(car(args))); }
void _cadaar(object cont, object args) { 
    return_funcall1(cont, cadaar(car(args))); }
void _cadadr(object cont, object args) { 
    return_funcall1(cont, cadadr(car(args))); }
void _caddar(object cont, object args) { 
    return_funcall1(cont, caddar(car(args))); }
void _cadddr(object cont, object args) { 
    return_funcall1(cont, cadddr(car(args))); }
void _cdaaar(object cont, object args) { 
    return_funcall1(cont, cdaaar(car(args))); }
void _cdaadr(object cont, object args) { 
    return_funcall1(cont, cdaadr(car(args))); }
void _cdadar(object cont, object args) { 
    return_funcall1(cont, cdadar(car(args))); }
void _cdaddr(object cont, object args) { 
    return_funcall1(cont, cdaddr(car(args))); }
void _cddaar(object cont, object args) { 
    return_funcall1(cont, cddaar(car(args))); }
void _cddadr(object cont, object args) { 
    return_funcall1(cont, cddadr(car(args))); }
void _cdddar(object cont, object args) { 
    return_funcall1(cont, cdddar(car(args))); }
void _cddddr(object cont, object args) { 
    return_funcall1(cont, cddddr(car(args))); }
void _cons(object cont, object args) { 
    make_cons(c, car(args), cadr(args));
    return_funcall1(cont, &c); }
void _eq_127(object cont, object args){ 
    return_funcall1(cont, Cyc_eq(car(args), cadr(args))); }
void _eqv_127(object cont, object args){ 
    _eq_127(cont, args); }
void _equal_127(object cont, object args){ 
    return_funcall1(cont, equalp(car(args), cadr(args))); }
void _length(object cont, object args){ 
    integer_type i = Cyc_length(car(args));
    return_funcall1(cont, &i); }
void _null_127(object cont, object args) { 
    return_funcall1(cont, Cyc_is_null(car(args))); }
void _set_91car_67(object cont, object args) { 
    return_funcall1(cont, Cyc_set_car(car(args), cadr(args))); }
void _set_91cdr_67(object cont, object args) { 
    return_funcall1(cont, Cyc_set_cdr(car(args), cadr(args))); }
void _Cyc_91has_91cycle_127(object cont, object args) { 
    return_funcall1(cont, Cyc_has_cycle(car(args))); }
void __87(object cont, object args) {
    integer_type argc = Cyc_length(args);
    dispatch(argc.value, (function_type)dispatch_sum, cont, cont, args); }
void __91(object cont, object args) {
    integer_type argc = Cyc_length(args);
    dispatch(argc.value, (function_type)dispatch_sub, cont, cont, args); }
void __85(object cont, object args) {
    integer_type argc = Cyc_length(args);
    dispatch(argc.value, (function_type)dispatch_mul, cont, cont, args); }
void __95(object cont, object args) {
    integer_type argc = Cyc_length(args);
    dispatch(argc.value, (function_type)dispatch_div, cont, cont, args); }
void _Cyc_91cvar_127(object cont, object args) {
    return_funcall1(cont, Cyc_is_cvar(car(args))); }
void _boolean_127(object cont, object args) {
    return_funcall1(cont, Cyc_is_boolean(car(args))); }
void _char_127(object cont, object args) {
    return_funcall1(cont, Cyc_is_char(car(args))); }
void _eof_91object_127(object cont, object args) {
    return_funcall1(cont, Cyc_is_eof_object(car(args))); }
void _number_127(object cont, object args) {
    return_funcall1(cont, Cyc_is_number(car(args))); }
void _real_127(object cont, object args) {
    return_funcall1(cont, Cyc_is_real(car(args))); }
void _integer_127(object cont, object args) {
    return_funcall1(cont, Cyc_is_integer(car(args))); }
void _pair_127(object cont, object args) {
    return_funcall1(cont, Cyc_is_cons(car(args))); }
void _procedure_127(object cont, object args) {
    return_funcall1(cont, Cyc_is_procedure(car(args))); }
void _string_127(object cont, object args) {
    return_funcall1(cont, Cyc_is_string(car(args))); }
void _symbol_127(object cont, object args) {
    return_funcall1(cont, Cyc_is_symbol(car(args))); }

void _Cyc_91get_91cvar(object cont, object args) {  
    printf("not implemented\n"); exit(1); }
void _Cyc_91set_91cvar_67(object cont, object args) {  
    printf("not implemented\n"); exit(1); }
/* Note we cannot use _exit (per convention) because it is reserved by C */
void _cyc_exit(object cont, object args) {  
    if(nullp(args))
        __halt(nil);
    __halt(car(args));
}
void __75halt(object cont, object args) {  
    exit(0); }
void _cell_91get(object cont, object args) {  
    printf("not implemented\n"); exit(1); }
void _set_91global_67(object cont, object args) {  
    printf("not implemented\n"); exit(1); }
void _set_91cell_67(object cont, object args) {  
    printf("not implemented\n"); exit(1); }
void _cell(object cont, object args) {  
    printf("not implemented\n"); exit(1); }

void __123(object cont, object args) {  
    return_funcall1(cont, __num_eq(car(args), cadr(args)));}
void __125(object cont, object args) {  
    return_funcall1(cont, __num_gt(car(args), cadr(args)));}
void __121(object cont, object args) {
    return_funcall1(cont, __num_lt(car(args), cadr(args)));}
void __125_123(object cont, object args) {
    return_funcall1(cont, __num_gte(car(args), cadr(args)));}
void __121_123(object cont, object args) {
    return_funcall1(cont, __num_lte(car(args), cadr(args)));}

void _apply(object cont, object args) {  
    apply(cont, car(args), cdr(args)); }
void _assoc (object cont, object args) {  
    return_funcall1(cont, assoc(car(args), cadr(args)));}
void _assq  (object cont, object args) {  
    return_funcall1(cont, assq(car(args), cadr(args)));}
void _assv  (object cont, object args) {  
    return_funcall1(cont, assq(car(args), cadr(args)));}
void _member(object cont, object args) {  
    return_funcall1(cont, memberp(car(args), cadr(args)));}
void _memq(object cont, object args) {  
    return_funcall1(cont, memqp(car(args), cadr(args)));}
void _memv(object cont, object args) {  
    return_funcall1(cont, memqp(car(args), cadr(args)));}
void _char_91_125integer(object cont, object args) {  
    integer_type i = Cyc_char2integer(car(args));
    return_funcall1(cont, &i);}
void _integer_91_125char(object cont, object args) {  
    return_funcall1(cont, Cyc_integer2char(car(args)));}
void _string_91_125number(object cont, object args) {  
    common_type i = Cyc_string2number(car(args));
    return_funcall1(cont, &i);}
//void _error(object cont, object args) {
//    integer_type argc = Cyc_length(args);
//    dispatch_va(argc.value, dispatch_error, cont, cont, args); }
void _Cyc_91current_91exception_91handler(object cont, object args) {
    object handler = Cyc_current_exception_handler();
    return_funcall1(cont, handler); }
void _Cyc_91default_91exception_91handler(object cont, object args) {
    // TODO: this is a quick-and-dirty implementation, may be a better way to write this
    Cyc_default_exception_handler(1, args, car(args));
}
void _string_91append(object cont, object args) {  
    integer_type argc = Cyc_length(args);
    dispatch(argc.value, (function_type)dispatch_string_91append, cont, cont, args); }
void _string_91_125list(object cont, object args) {  
    string2list(lst, car(args));
    return_funcall1(cont, &lst);}
void _list_91_125string(object cont, object args) {  
    string_type s = Cyc_list2string(car(args));
    return_funcall1(cont, &s);}
void _string_91_125symbol(object cont, object args) {  
    return_funcall1(cont, Cyc_string2symbol(car(args)));}
void _symbol_91_125string(object cont, object args) {  
    string_type s = Cyc_symbol2string(car(args));
    return_funcall1(cont, &s);}
void _number_91_125string(object cont, object args) {  
    string_type s = Cyc_number2string(car(args));
    return_funcall1(cont, &s);}
void _current_91input_91port(object cont, object args) {  
    port_type p = Cyc_io_current_input_port();
    return_funcall1(cont, &p);}
void _open_91input_91file(object cont, object args) {  
    port_type p = Cyc_io_open_input_file(car(args));
    return_funcall1(cont, &p);}
void _close_91input_91port(object cont, object args) {  
    return_funcall1(cont, Cyc_io_close_input_port(car(args)));}
void _read_91char(object cont, object args) {  
    return_funcall1(cont, Cyc_io_read_char(car(args)));}
void _peek_91char(object cont, object args) {  
    return_funcall1(cont, Cyc_io_peek_char(car(args)));}
void _write(object cont, object args) {  
    return_funcall1(cont, Cyc_write(car(args))); }
void _display(object cont, object args) {  
    return_funcall1(cont, Cyc_display(car(args)));}
void _call_95cc(object cont, object args){
    return_funcall2(__glo_call_95cc, cont, car(args));
}

/*
 * @param cont - Continuation for the function to call into
 * @param func - Function to execute
 * @param args - A list of arguments to the function
 */
object apply(object cont, object func, object args){
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
      
    default:
      printf("Invalid object type %ld\n", type_of(func));
      exit(1);
  }
  return nil; // Never reached
}

// Version of apply meant to be called from within compiled code
void Cyc_apply(int argc, closure cont, object prim, ...){
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
void Cyc_apply_from_buf(int argc, object prim, object *buf) {
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

char *transport(x, gcgen) char *x; int gcgen;
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

void GC_loop(int major, closure cont, object *ans, int num_ans)
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
 {
   list l = global_table;
   for(; !nullp(l); l = cdr(l)){
    cvar_type *c = (cvar_type *)car(l);
    transp(*(c->pvar)); // GC global, not the pvar
   }
 }
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

void GC(cont,ans,num_ans) closure cont; object *ans; int num_ans;
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


/**
 * Receive a list of arguments and apply them to the given function
 */
void dispatch(int argc, function_type func, object clo, object cont, object args) {
  object b[argc + 1]; // OK to do this? Is this portable?
  int i; 

  argc++;
  b[0] = cont;
  for (i = 1; i < argc; i++){ 
    b[i] = car(args); 
    args = cdr(args); 
  } 

  do_dispatch(argc, func, clo, b);
}

/**
 * Same as above but for a varargs C function
 */
void dispatch_va(int argc, function_type_va func, object clo, object cont, object args) {
  object b[argc + 1]; // OK to do this? Is this portable?
  int i; 
 
  argc++;
  b[0] = cont;
  for (i = 1; i < argc; i++){ 
    b[i] = car(args); 
    args = cdr(args); 
  } 

  do_dispatch(argc, (function_type)func, clo, b);
}

/**
 * Take list of args and call a function with them as params.
 *
 * All of the clever C macro expansions were taken from the 
 * macro expansion code from CHICKEN's do_apply. TBD if this will
 * remain. On the one hand it reduces the size of the C code, but
 * on the other I think it ties this code to CHICKEN's license terms.
 *
 * Note memory scheme we are using is not compatible with GC, 
 * so macro calls funcs directly.
 */
void do_dispatch(int argc, function_type func, object clo, object *b) {

/* PTR_O_p<P>_<B>(o): list of COUNT = ((2 ** P) * B) '*(b+I)' arguments,
 * with offset I in range [o, o+COUNT-1].
 */
#define PTR_O_p0_0(o)
#define PTR_O_p1_0(o)
#define PTR_O_p2_0(o)
#define PTR_O_p3_0(o)
#define PTR_O_p4_0(o)
#define PTR_O_p5_0(o)
#define PTR_O_p6_0(o)
#define PTR_O_p7_0(o)
#define PTR_O_p0_1(o)   , *(b+(o))
#define PTR_O_p1_1(o)   , *(b+(o)), *(b+(o+1))
#define PTR_O_p2_1(o)   PTR_O_p1_1(o) PTR_O_p1_1(o+2)
#define PTR_O_p3_1(o)   PTR_O_p2_1(o) PTR_O_p2_1(o+4)
#define PTR_O_p4_1(o)   PTR_O_p3_1(o) PTR_O_p3_1(o+8)
#define PTR_O_p5_1(o)   PTR_O_p4_1(o) PTR_O_p4_1(o+16)
#define PTR_O_p6_1(o)   PTR_O_p5_1(o) PTR_O_p5_1(o+32)
#define PTR_O_p7_1(o)   PTR_O_p6_1(o) PTR_O_p6_1(o+64)

/* CASE_C_PROC_p0 (n0,  p6,p5,p4,p3,p2,p1,p0):
 *  let's note <N> = <n0> - 2; the macro inserts:
 *      case <N>: ((C_cproc<n0>)pr) (<n0>, fn, k, <rest>);
 *  where <rest> is:    *(b+1), ..., *(b+<N>)
 *  (<rest> is empty for <n0> == 2).
 *  We must have:   n0 = SUM (i = 7 to 0, p<i> * (1 << i)).
 * CASE_C_PROC_p<N+1> (...):
 *  like CASE_C_PROC_p<N>, but with doubled output...
 */
//    case (n0-2): (Cyc_proc##n0)func(n0-2, clo 
#define CASE_C_PROC_p0(n0,  p6,p5,p4,p3,p2,p1,p0) \
    case (n0-2): func(n0-2, clo \
PTR_O_p6_##p6(((n0-2)&0x80)+0)\
PTR_O_p5_##p5(((n0-2)&0xC0)+0)\
PTR_O_p4_##p4(((n0-2)&0xE0)+0)\
PTR_O_p3_##p3(((n0-2)&0xF0)+0)\
PTR_O_p2_##p2(((n0-2)&0xF8)+0)\
PTR_O_p1_##p1(((n0-2)&0xFC)+0)\
PTR_O_p0_##p0(((n0-2)&0xFE)+0));
#define CASE_C_PROC_p1( n0,n1,  p6,p5,p4,p3,p2,p1) \
        CASE_C_PROC_p0 (n0,  p6,p5,p4,p3,p2,p1,0) \
        CASE_C_PROC_p0 (n1,  p6,p5,p4,p3,p2,p1,1)
#define CASE_C_PROC_p2( n0,n1,n2,n3,  p6,p5,p4,p3,p2) \
        CASE_C_PROC_p1 (n0,n1,  p6,p5,p4,p3,p2,0) \
        CASE_C_PROC_p1 (n2,n3,  p6,p5,p4,p3,p2,1)
#define CASE_C_PROC_p3( n0,n1,n2,n3,n4,n5,n6,n7,  p6,p5,p4,p3) \
        CASE_C_PROC_p2 (n0,n1,n2,n3,  p6,p5,p4,p3,0) \
        CASE_C_PROC_p2 (n4,n5,n6,n7,  p6,p5,p4,p3,1)
  switch(argc) {
    CASE_C_PROC_p3 (2,3,4,5,6,7,8,9,  0,0,0,0)
    CASE_C_PROC_p3 (10,11,12,13,14,15,16,17,  0,0,0,1)
    CASE_C_PROC_p3 (18,19,20,21,22,23,24,25,  0,0,1,0)
    CASE_C_PROC_p3 (26,27,28,29,30,31,32,33,  0,0,1,1)
    CASE_C_PROC_p3 (34,35,36,37,38,39,40,41,  0,1,0,0)
    CASE_C_PROC_p3 (42,43,44,45,46,47,48,49,  0,1,0,1)
    CASE_C_PROC_p3 (50,51,52,53,54,55,56,57,  0,1,1,0)
    CASE_C_PROC_p3 (58,59,60,61,62,63,64,65,  0,1,1,1)
    CASE_C_PROC_p0 (66,  1,0,0,0,0,0,0)
    CASE_C_PROC_p0 (67,  1,0,0,0,0,0,1)
    CASE_C_PROC_p1 (68,69,  1,0,0,0,0,1)
    CASE_C_PROC_p2 (70,71,72,73,  1,0,0,0,1)
    CASE_C_PROC_p3 (74,75,76,77,78,79,80,81,  1,0,0,1)
    CASE_C_PROC_p3 (82,83,84,85,86,87,88,89,  1,0,1,0)
    CASE_C_PROC_p3 (90,91,92,93,94,95,96,97,  1,0,1,1)
    CASE_C_PROC_p3 (98,99,100,101,102,103,104,105,  1,1,0,0)
    CASE_C_PROC_p3 (106,107,108,109,110,111,112,113,  1,1,0,1)
    CASE_C_PROC_p3 (114,115,116,117,118,119,120,121,  1,1,1,0)
    CASE_C_PROC_p2 (122,123,124,125,  1,1,1,1,0)
    CASE_C_PROC_p1 (126,127,  1,1,1,1,1,0)
    CASE_C_PROC_p0 (128,  1,1,1,1,1,1,0)
  default:
  {
   char buf[1024];
   snprintf(buf, 1023, "Unhandled number of function arguments: %d\n", argc); 
   Cyc_rt_raise_msg(buf);
  }
  }
}

static primitive_type Cyc_91global_91vars_primitive = {primitive_tag, "Cyc-global-vars", &_Cyc_91global_91vars};
static primitive_type Cyc_91get_91cvar_primitive = {primitive_tag, "Cyc-get-cvar", &_Cyc_91get_91cvar};
static primitive_type Cyc_91set_91cvar_67_primitive = {primitive_tag, "Cyc-set-cvar!", &_Cyc_91set_91cvar_67};
static primitive_type Cyc_91cvar_127_primitive = {primitive_tag, "Cyc-cvar?", &_Cyc_91cvar_127};
static primitive_type Cyc_91has_91cycle_127_primitive = {primitive_tag, "Cyc-has-cycle?", &_Cyc_91has_91cycle_127};
static primitive_type _87_primitive = {primitive_tag, "+", &__87};
static primitive_type _91_primitive = {primitive_tag, "-", &__91};
static primitive_type _85_primitive = {primitive_tag, "*", &__85};
static primitive_type _95_primitive = {primitive_tag, "/", &__95};
static primitive_type _123_primitive = {primitive_tag, "=", &__123};
static primitive_type _125_primitive = {primitive_tag, ">", &__125};
static primitive_type _121_primitive = {primitive_tag, "<", &__121};
static primitive_type _125_123_primitive = {primitive_tag, ">=", &__125_123};
static primitive_type _121_123_primitive = {primitive_tag, "<=", &__121_123};
static primitive_type apply_primitive = {primitive_tag, "apply", &_apply};
static primitive_type _75halt_primitive = {primitive_tag, "%halt", &__75halt};
static primitive_type exit_primitive = {primitive_tag, "exit", &_cyc_exit};
static primitive_type Cyc_91current_91exception_91handler_primitive = {primitive_tag, "Cyc_current_exception_handler", &_Cyc_91current_91exception_91handler};
static primitive_type Cyc_91default_91exception_91handler_primitive = {primitive_tag, "Cyc_default_exception_handler", &_Cyc_91default_91exception_91handler};
static primitive_type cons_primitive = {primitive_tag, "cons", &_cons};
static primitive_type cell_91get_primitive = {primitive_tag, "cell-get", &_cell_91get};
static primitive_type set_91global_67_primitive = {primitive_tag, "set-global!", &_set_91global_67};
static primitive_type set_91cell_67_primitive = {primitive_tag, "set-cell!", &_set_91cell_67};
static primitive_type cell_primitive = {primitive_tag, "cell", &_cell};
static primitive_type eq_127_primitive = {primitive_tag, "eq?", &_eq_127};
static primitive_type eqv_127_primitive = {primitive_tag, "eqv?", &_eqv_127};
static primitive_type equal_127_primitive = {primitive_tag, "equal?", &_equal_127};
static primitive_type assoc_primitive = {primitive_tag, "assoc", &_assoc};
static primitive_type assq_primitive = {primitive_tag, "assq", &_assq};
static primitive_type assv_primitive = {primitive_tag, "assv", &_assv};
static primitive_type member_primitive = {primitive_tag, "member", &_member};
static primitive_type memq_primitive = {primitive_tag, "memq", &_memq};
static primitive_type memv_primitive = {primitive_tag, "memv", &_memv};
static primitive_type length_primitive = {primitive_tag, "length", &_length};
static primitive_type set_91car_67_primitive = {primitive_tag, "set-car!", &_set_91car_67};
static primitive_type set_91cdr_67_primitive = {primitive_tag, "set-cdr!", &_set_91cdr_67};
static primitive_type car_primitive = {primitive_tag, "car", &_car};
static primitive_type cdr_primitive = {primitive_tag, "cdr", &_cdr};
static primitive_type caar_primitive = {primitive_tag, "caar", &_caar};
static primitive_type cadr_primitive = {primitive_tag, "cadr", &_cadr};
static primitive_type cdar_primitive = {primitive_tag, "cdar", &_cdar};
static primitive_type cddr_primitive = {primitive_tag, "cddr", &_cddr};
static primitive_type caaar_primitive = {primitive_tag, "caaar", &_caaar};
static primitive_type caadr_primitive = {primitive_tag, "caadr", &_caadr};
static primitive_type cadar_primitive = {primitive_tag, "cadar", &_cadar};
static primitive_type caddr_primitive = {primitive_tag, "caddr", &_caddr};
static primitive_type cdaar_primitive = {primitive_tag, "cdaar", &_cdaar};
static primitive_type cdadr_primitive = {primitive_tag, "cdadr", &_cdadr};
static primitive_type cddar_primitive = {primitive_tag, "cddar", &_cddar};
static primitive_type cdddr_primitive = {primitive_tag, "cdddr", &_cdddr};
static primitive_type caaaar_primitive = {primitive_tag, "caaaar", &_caaaar};
static primitive_type caaadr_primitive = {primitive_tag, "caaadr", &_caaadr};
static primitive_type caadar_primitive = {primitive_tag, "caadar", &_caadar};
static primitive_type caaddr_primitive = {primitive_tag, "caaddr", &_caaddr};
static primitive_type cadaar_primitive = {primitive_tag, "cadaar", &_cadaar};
static primitive_type cadadr_primitive = {primitive_tag, "cadadr", &_cadadr};
static primitive_type caddar_primitive = {primitive_tag, "caddar", &_caddar};
static primitive_type cadddr_primitive = {primitive_tag, "cadddr", &_cadddr};
static primitive_type cdaaar_primitive = {primitive_tag, "cdaaar", &_cdaaar};
static primitive_type cdaadr_primitive = {primitive_tag, "cdaadr", &_cdaadr};
static primitive_type cdadar_primitive = {primitive_tag, "cdadar", &_cdadar};
static primitive_type cdaddr_primitive = {primitive_tag, "cdaddr", &_cdaddr};
static primitive_type cddaar_primitive = {primitive_tag, "cddaar", &_cddaar};
static primitive_type cddadr_primitive = {primitive_tag, "cddadr", &_cddadr};
static primitive_type cdddar_primitive = {primitive_tag, "cdddar", &_cdddar};
static primitive_type cddddr_primitive = {primitive_tag, "cddddr", &_cddddr};
static primitive_type char_91_125integer_primitive = {primitive_tag, "char->integer", &_char_91_125integer};
static primitive_type integer_91_125char_primitive = {primitive_tag, "integer->char", &_integer_91_125char};
static primitive_type string_91_125number_primitive = {primitive_tag, "string->number", &_string_91_125number};
static primitive_type string_91append_primitive = {primitive_tag, "string-append", &_string_91append};
static primitive_type string_91_125list_primitive = {primitive_tag, "string->list", &_string_91_125list};
static primitive_type list_91_125string_primitive = {primitive_tag, "list->string", &_list_91_125string};
static primitive_type string_91_125symbol_primitive = {primitive_tag, "string->symbol", &_string_91_125symbol};
static primitive_type symbol_91_125string_primitive = {primitive_tag, "symbol->string", &_symbol_91_125string};
static primitive_type number_91_125string_primitive = {primitive_tag, "number->string", &_number_91_125string};
static primitive_type boolean_127_primitive = {primitive_tag, "boolean?", &_boolean_127};
static primitive_type char_127_primitive = {primitive_tag, "char?", &_char_127};
static primitive_type eof_91object_127_primitive = {primitive_tag, "eof-object?", &_eof_91object_127};
static primitive_type null_127_primitive = {primitive_tag, "null?", &_null_127};
static primitive_type number_127_primitive = {primitive_tag, "number?", &_number_127};
static primitive_type real_127_primitive = {primitive_tag, "real?", &_real_127};
static primitive_type integer_127_primitive = {primitive_tag, "integer?", &_integer_127};
static primitive_type pair_127_primitive = {primitive_tag, "pair?", &_pair_127};
static primitive_type procedure_127_primitive = {primitive_tag, "procedure?", &_procedure_127};
static primitive_type string_127_primitive = {primitive_tag, "string?", &_string_127};
static primitive_type symbol_127_primitive = {primitive_tag, "symbol?", &_symbol_127};
static primitive_type current_91input_91port_primitive = {primitive_tag, "current-input-port", &_current_91input_91port};
static primitive_type open_91input_91file_primitive = {primitive_tag, "open-input-file", &_open_91input_91file};
static primitive_type close_91input_91port_primitive = {primitive_tag, "close-input-port", &_close_91input_91port};
static primitive_type read_91char_primitive = {primitive_tag, "read-char", &_read_91char};
static primitive_type peek_91char_primitive = {primitive_tag, "peek-char", &_peek_91char};
static primitive_type write_primitive = {primitive_tag, "write", &_write};
static primitive_type display_primitive = {primitive_tag, "display", &_display};
static primitive_type call_95cc_primitive = {primitive_tag, "call/cc", &_call_95cc};

const object primitive_Cyc_91global_91vars = &Cyc_91global_91vars_primitive;
const object primitive_Cyc_91get_91cvar = &Cyc_91get_91cvar_primitive;
const object primitive_Cyc_91set_91cvar_67 = &Cyc_91set_91cvar_67_primitive;
const object primitive_Cyc_91cvar_127 = &Cyc_91cvar_127_primitive;
const object primitive_Cyc_91has_91cycle_127 = &Cyc_91has_91cycle_127_primitive;
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
const object primitive_string_91append = &string_91append_primitive;
const object primitive_string_91_125list = &string_91_125list_primitive;
const object primitive_list_91_125string = &list_91_125string_primitive;
const object primitive_string_91_125symbol = &string_91_125symbol_primitive;
const object primitive_symbol_91_125string = &symbol_91_125string_primitive;
const object primitive_number_91_125string = &number_91_125string_primitive;
const object primitive_boolean_127 = &boolean_127_primitive;
const object primitive_char_127 = &char_127_primitive;
const object primitive_eof_91object_127 = &eof_91object_127_primitive;
const object primitive_null_127 = &null_127_primitive;
const object primitive_number_127 = &number_127_primitive;
const object primitive_real_127 = &real_127_primitive;
const object primitive_integer_127 = &integer_127_primitive;
const object primitive_pair_127 = &pair_127_primitive;
const object primitive_procedure_127 = &procedure_127_primitive;
const object primitive_string_127 = &string_127_primitive;
const object primitive_symbol_127 = &symbol_127_primitive;
const object primitive_current_91input_91port = &current_91input_91port_primitive;
const object primitive_open_91input_91file = &open_91input_91file_primitive;
const object primitive_close_91input_91port = &close_91input_91port_primitive;
const object primitive_read_91char = &read_91char_primitive;
const object primitive_peek_91char = &peek_91char_primitive;
const object primitive_write = &write_primitive;
const object primitive_display = &display_primitive;
const object primitive_call_95cc = &call_95cc_primitive;


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

extern long global_stack_size;
extern long global_heap_size;
extern const object Cyc_EOF;

object cell_get(object cell);

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
#define load_varargs(var, arg_var, count) \
  list var = (count > 0) ? alloca(sizeof(cons_type)*count) : nil; \
  { \
    int i; \
    object tmp; \
    va_list va; \
    if (count > 0) { \
      va_start(va, arg_var); \
      for (i = 0; i < count; i++) { \
        if (i) { \
            tmp = va_arg(va, object); \
        } else { \
            tmp = arg_var; \
        } \
        var[i].tag = cons_tag; \
        var[i].cons_car = tmp; \
        var[i].cons_cdr = (i == (count-1)) ? nil : &var[i + 1]; \
      } \
      va_end(va); \
    } \
  }

/* Prototypes for Lisp built-in functions. */

extern object Cyc_global_variables;
object Cyc_get_global_variables();
object Cyc_get_cvar(object var);
object Cyc_set_cvar(object var, object value);
object apply(object cont, object func, object args);
void Cyc_apply(int argc, closure cont, object prim, ...);
void dispatch_string_91append(int argc, object clo, object cont, object str1, ...);
string_type Cyc_string_append(int argc, object str1, ...);
string_type Cyc_string_append_va_list(int, object, va_list);
list mcons(object,object);
object terpri(void);
object Cyc_display(object);
object Cyc_write(object);

object Cyc_has_cycle(object lst);
list assoc(object x, list l);
object __num_eq(object x, object y);
object __num_gt(object x, object y);
object __num_lt(object x, object y);
object __num_gte(object x, object y);
object __num_lte(object x, object y);
object Cyc_eq(object x, object y);
object Cyc_set_car(object l, object val) ;
object Cyc_set_cdr(object l, object val) ;
integer_type Cyc_length(object l);
string_type Cyc_number2string(object n) ;
string_type Cyc_symbol2string(object sym) ;
object Cyc_string2symbol(object str);
string_type Cyc_list2string(object lst);
void __string2list(const char *str, cons_type *buf, int buflen);
common_type Cyc_string2number(object str);
void dispatch_string_91append(int argc, object clo, object cont, object str1, ...);
string_type Cyc_string_append(int argc, object str1, ...);
string_type Cyc_string_append_va_list(int argc, object str1, va_list ap);
integer_type Cyc_char2integer(object chr);
object Cyc_integer2char(object n);
void my_exit(closure) never_returns;
port_type Cyc_io_current_input_port();
port_type Cyc_io_open_input_file(object str);
object Cyc_io_close_input_port(object port);
object Cyc_io_read_char(object port);
object Cyc_io_peek_char(object port);

object Cyc_is_boolean(object o);
object Cyc_is_cons(object o);
object Cyc_is_null(object o);
object Cyc_is_number(object o);
object Cyc_is_real(object o);
object Cyc_is_integer(object o);
object Cyc_is_symbol(object o);
object Cyc_is_string(object o);
object Cyc_is_char(object o);
object Cyc_is_procedure(object o);
object Cyc_is_eof_object(object o);
object Cyc_is_cvar(object o);
common_type Cyc_sum_op(object x, object y);
common_type Cyc_sub_op(object x, object y);
common_type Cyc_mul_op(object x, object y);
common_type Cyc_div_op(object x, object y);
common_type Cyc_sum(int argc, object n, ...);
common_type Cyc_sub(int argc, object n, ...);
common_type Cyc_mul(int argc, object n, ...);
common_type Cyc_div(int argc, object n, ...);
common_type Cyc_num_op_va_list(int argc, common_type (fn_op(object, object)), object n, va_list ns);
int equal(object,object);
list assq(object,list);
object get(object,object);
object equalp(object,object);
object memberp(object,list);
object memqp(object,list);
char *transport(char *,int);
void GC(closure,object*,int) never_returns;

char *_strdup (const char *s);
object add_symbol(symbol_type *psym);
object add_symbol_by_name(const char *name);
object find_symbol_by_name(const char *name);
object find_or_add_symbol(const char *name);
extern list symbol_table;

void add_mutation(object var, object value);
void clear_mutations();
extern list mutation_table;

void dispatch(int argc, function_type func, object clo, object cont, object args);
void dispatch_va(int argc, function_type_va func, object clo, object cont, object args);
void do_dispatch(int argc, function_type func, object clo, object *buffer);

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


// JAE TODO: not sure how to refactor global section yet
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
extern const object boolean_t;
extern const object boolean_f;
extern const object quote_Cyc_191procedure;

// JAE TODO: will probably need to refactor this, since modules (libs)
// can have globals, too
//TODO: DECLARE_GLOBALS

#ifdef CYC_EVAL
static void _call_95cc(object cont, object args){
    return_funcall2(__glo_call_95cc, cont, car(args));
}
defprimitive(call_95cc, call/cc, &_call_95cc); // Moved up here due to ifdef
#endif /* CYC_EVAL */

/* This section is auto-generated via --autogen */
extern const object primitive_Cyc_91global_91vars;
// TODO:
//defprimitive(Cyc_91get_91cvar, Cyc-get-cvar, &_Cyc_91get_91cvar); /* Cyc-get-cvar */
//defprimitive(Cyc_91set_91cvar_67, Cyc-set-cvar!, &_Cyc_91set_91cvar_67); /* Cyc-set-cvar! */
//defprimitive(Cyc_91cvar_127, Cyc-cvar?, &_Cyc_91cvar_127); /* Cyc-cvar? */
//defprimitive(Cyc_91has_91cycle_127, Cyc-has-cycle?, &_Cyc_91has_91cycle_127); /* Cyc-has-cycle? */
//defprimitive(_87, +, &__87); /* + */
//defprimitive(_91, -, &__91); /* - */
//defprimitive(_85, *, &__85); /* * */
//defprimitive(_95, /, &__95); /* / */
//defprimitive(_123, =, &__123); /* = */
//defprimitive(_125, >, &__125); /* > */
//defprimitive(_121, <, &__121); /* < */
//defprimitive(_125_123, >=, &__125_123); /* >= */
//defprimitive(_121_123, <=, &__121_123); /* <= */
//defprimitive(apply, apply, &_apply); /* apply */
//defprimitive(_75halt, %halt, &__75halt); /* %halt */
//defprimitive(exit, exit, &_cyc_exit); /* exit */
////defprimitive(error, error, &_error); /* error */
//defprimitive(
//    Cyc_91current_91exception_91handler,
//    Cyc_current_exception_handler,
//    &_Cyc_91current_91exception_91handler); /* Cyc-current-exception-handler */
//defprimitive(
//    Cyc_91default_91exception_91handler,
//    Cyc_default_exception_handler,
//    &_Cyc_91default_91exception_91handler); /* Cyc-default-exception-handler */
//defprimitive(cons, cons, &_cons); /* cons */
//defprimitive(cell_91get, cell-get, &_cell_91get); /* cell-get */
//defprimitive(set_91global_67, set-global!, &_set_91global_67); /* set-global! */
//defprimitive(set_91cell_67, set-cell!, &_set_91cell_67); /* set-cell! */
//defprimitive(cell, cell, &_cell); /* cell */
//defprimitive(eq_127, eq?, &_eq_127); /* eq? */
//defprimitive(eqv_127, eqv?, &_eqv_127); /* eqv? */
//defprimitive(equal_127, equal?, &_equal_127); /* equal? */
//defprimitive(assoc, assoc, &_assoc); /* assoc */
//defprimitive(assq, assq, &_assq); /* assq */
//defprimitive(assv, assv, &_assv); /* assq */
//defprimitive(member, member, &_member); /* member */
//defprimitive(memq, memq, &_memq); /* memq */
//defprimitive(memv, memv, &_memv); /* memv */
//defprimitive(length, length, &_length); /* length */
//defprimitive(set_91car_67, set-car!, &_set_91car_67); /* set-car! */
//defprimitive(set_91cdr_67, set-cdr!, &_set_91cdr_67); /* set-cdr! */
//defprimitive(car, car, &_car); /* car */
//defprimitive(cdr, cdr, &_cdr); /* cdr */
//defprimitive(caar, caar, &_caar); /* caar */
//defprimitive(cadr, cadr, &_cadr); /* cadr */
//defprimitive(cdar, cdar, &_cdar); /* cdar */
//defprimitive(cddr, cddr, &_cddr); /* cddr */
//defprimitive(caaar, caaar, &_caaar); /* caaar */
//defprimitive(caadr, caadr, &_caadr); /* caadr */
//defprimitive(cadar, cadar, &_cadar); /* cadar */
//defprimitive(caddr, caddr, &_caddr); /* caddr */
//defprimitive(cdaar, cdaar, &_cdaar); /* cdaar */
//defprimitive(cdadr, cdadr, &_cdadr); /* cdadr */
//defprimitive(cddar, cddar, &_cddar); /* cddar */
//defprimitive(cdddr, cdddr, &_cdddr); /* cdddr */
//defprimitive(caaaar, caaaar, &_caaaar); /* caaaar */
//defprimitive(caaadr, caaadr, &_caaadr); /* caaadr */
//defprimitive(caadar, caadar, &_caadar); /* caadar */
//defprimitive(caaddr, caaddr, &_caaddr); /* caaddr */
//defprimitive(cadaar, cadaar, &_cadaar); /* cadaar */
//defprimitive(cadadr, cadadr, &_cadadr); /* cadadr */
//defprimitive(caddar, caddar, &_caddar); /* caddar */
//defprimitive(cadddr, cadddr, &_cadddr); /* cadddr */
//defprimitive(cdaaar, cdaaar, &_cdaaar); /* cdaaar */
//defprimitive(cdaadr, cdaadr, &_cdaadr); /* cdaadr */
//defprimitive(cdadar, cdadar, &_cdadar); /* cdadar */
//defprimitive(cdaddr, cdaddr, &_cdaddr); /* cdaddr */
//defprimitive(cddaar, cddaar, &_cddaar); /* cddaar */
//defprimitive(cddadr, cddadr, &_cddadr); /* cddadr */
//defprimitive(cdddar, cdddar, &_cdddar); /* cdddar */
//defprimitive(cddddr, cddddr, &_cddddr); /* cddddr */
//defprimitive(char_91_125integer, char->integer, &_char_91_125integer); /* char->integer */
//defprimitive(integer_91_125char, integer->char, &_integer_91_125char); /* integer->char */
//defprimitive(string_91_125number, string->number, &_string_91_125number); /* string->number */
//defprimitive(string_91append, string-append, &_string_91append); /* string-append */
//defprimitive(string_91_125list, string->list, &_string_91_125list); /* string->list */
//defprimitive(list_91_125string, list->string, &_list_91_125string); /* list->string */
//defprimitive(string_91_125symbol, string->symbol, &_string_91_125symbol); /* string->symbol */
//defprimitive(symbol_91_125string, symbol->string, &_symbol_91_125string); /* symbol->string */
//defprimitive(number_91_125string, number->string, &_number_91_125string); /* number->string */
//defprimitive(boolean_127, boolean?, &_boolean_127); /* boolean? */
//defprimitive(char_127, char?, &_char_127); /* char? */
//defprimitive(eof_91object_127, eof-object?, &_eof_91object_127); /* eof-object? */
//defprimitive(null_127, null?, &_null_127); /* null? */
//defprimitive(number_127, number?, &_number_127); /* number? */
//defprimitive(real_127, real?, &_real_127); /* real? */
//defprimitive(integer_127, integer?, &_integer_127); /* integer? */
//defprimitive(pair_127, pair?, &_pair_127); /* pair? */
//defprimitive(procedure_127, procedure?, &_procedure_127); /* procedure? */
//defprimitive(string_127, string?, &_string_127); /* string? */
//defprimitive(symbol_127, symbol?, &_symbol_127); /* symbol? */
//defprimitive(current_91input_91port, current-input-port, &_current_91input_91port); /* current-input-port */
//defprimitive(open_91input_91file, open-input-file, &_open_91input_91file); /* open-input-file */
//defprimitive(close_91input_91port, close-input-port, &_close_91input_91port); /* close-input-port */
//defprimitive(read_91char, read-char, &_read_91char); /* read-char */
//defprimitive(peek_91char, peek-char, &_peek_91char); /* peek-char */
//defprimitive(write, write, &_write); /* write */
//defprimitive(display, display, &_display); /* display */
/* -------------------------------------------- */

/* Exception handler */
extern object Cyc_exception_handler_stack;

// Special case, use this one instead since we need it in the runtime
// This also seems to "shadow" the corresponding C var definition, as
// subsequent instances of it are replaced during preprocessing. Is that
// behavior portable? If not, will have to modify cgen to not emit the var.
#define __glo__85exception_91handler_91stack_85 Cyc_exception_handler_stack

object Cyc_default_exception_handler(int argc, closure _, object err);
object Cyc_current_exception_handler();
void Cyc_rt_raise(object err);
void Cyc_rt_raise_msg(const char *err);
/* END exception handler */

/*
 *
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
 //TODO: GC_GLOBALS
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

/* This heap cons is used only for initialization. */
list mcons(a,d) object a,d;
{register cons_type *c = malloc(sizeof(cons_type));
 c->tag = cons_tag; c->cons_car = a; c->cons_cdr = d;
 return c;}


#endif /* CYCLONE_RUNTIME_H */

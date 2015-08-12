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

/* Debug GC flag */
#define DEBUG_GC 0

/* Show diagnostic information for the GC when program terminates */
#define DEBUG_SHOW_DIAG 0

/* Maximum number of args that GC will accept */
#define NUM_GC_ANS 128

/* STACK_GROWS_DOWNWARD is a machine-specific preprocessor switch. */
/* It is true for the Macintosh 680X0 and the Intel 80860. */
#define STACK_GROWS_DOWNWARD 1

/* STACK_SIZE is the size of the stack buffer, in bytes.           */
/* Some machines like a smallish stack--i.e., 4k-16k, while others */
/* like a biggish stack--i.e., 100k-500k.                          */
#define STACK_SIZE 100000

/* HEAP_SIZE is the size of the 2nd generation, in bytes. */
/* HEAP_SIZE should be at LEAST 225000*sizeof(cons_type). */
#define HEAP_SIZE 6000000

/* Define size of Lisp tags.  Options are "short" or "long". */
typedef long tag_type;

#ifndef CLOCKS_PER_SEC
/* gcc doesn't define this, even though ANSI requires it in <time.h>.. */
#define CLOCKS_PER_SEC 0
#define setjmp _setjmp
#define longjmp _longjmp
#endif

#if STACK_GROWS_DOWNWARD
#define check_overflow(x,y) ((x) < (y))
#else
#define check_overflow(x,y) ((x) > (y))
#endif

/* Define tag values. Could be an enum...
   Remember to update tag_names in runtime.c when adding new tags */
#define cons_tag 0
#define symbol_tag 1
#define forward_tag 2
#define closure0_tag 3
#define closure1_tag 4
#define closure2_tag 5
#define closure3_tag 6
#define closure4_tag 7
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

#define nil NULL
#define eq(x,y) (x == y)
#define nullp(x) (x == NULL)
#define or(x,y) (x || y)
#define and(x,y) (x && y)

/* Define general object type. */

typedef void *object;

#define type_of(x) (((list) x)->tag)
#define forward(x) (((list) x)->cons_car)

/* Define value types. 
   Depending on the underlying architecture, compiler, etc these types
   have extra least significant bits that can be used to mark them as
   values instead of objects (IE, pointer to a tagged object).
   On many machines, addresses are multiples of four, leaving the two
   least significant bits free - according to lisp in small pieces.
*/
#define obj_is_char(x)  ((unsigned long)(x) & (unsigned long)1)
#define obj_obj2char(x) (char)((long)(x)>>1)
#define obj_char2obj(c) ((void *)(((c)<<1) | 1))

#define is_value_type(x) obj_is_char(x)
#define is_object_type(x) (x && !is_value_type(x))

/* Define function type. */

typedef void (*function_type)();
typedef void (*function_type_va)(int, object, object, object, ...);

/* Define C-variable integration type */
typedef struct {tag_type tag; object *pvar;} cvar_type;
typedef cvar_type *cvar;
#define make_cvar(n,v) cvar_type n; n.tag = cvar_tag; n.pvar = v;

/* Define boolean type. */
typedef struct {const tag_type tag; const char *pname;} boolean_type;
typedef boolean_type *boolean;

#define boolean_pname(x) (((boolean_type *) x)->pname)

/* Define symbol type. */

typedef struct {const tag_type tag; const char *pname; object plist;} symbol_type;
typedef symbol_type *symbol;

#define symbol_pname(x) (((symbol_type *) x)->pname)
#define symbol_plist(x) (((symbol_type *) x)->plist)

#define defsymbol(name) \
static object quote_##name = nil;

/* Define numeric types */
typedef struct {tag_type tag; int value;} integer_type;
#define make_int(n,v) integer_type n; n.tag = integer_tag; n.value = v;
typedef struct {tag_type tag; double value;} double_type;
#define make_double(n,v) double_type n; n.tag = double_tag; n.value = v;

#define integer_value(x) (((integer_type *) x)->value)
#define double_value(x) (((double_type *) x)->value)

/* Define string type */
typedef struct {tag_type tag; char *str;} string_type;
#define make_string(cv,s) string_type cv; cv.tag = string_tag; \
{ int len = strlen(s); cv.str = dhallocp; \
  if ((dhallocp + len + 1) >= dhbottom + global_heap_size) { \
      printf("Fatal error: data heap overflow\n"); exit(1); } \
  memcpy(dhallocp, s, len + 1); dhallocp += len + 1; }
#define make_stringn(cv,s,len) string_type cv; cv.tag = string_tag; \
{ cv.str = dhallocp; \
  if ((dhallocp + len + 1) >= dhbottom + global_heap_size) { \
      printf("Fatal error: data heap overflow\n"); exit(1); } \
  memcpy(dhallocp, s, len); dhallocp += len; \
  *dhallocp = '\0'; dhallocp += 1;}

#define string_str(x) (((string_type *) x)->str)

/* I/O types */

// TODO: FILE* may not be good enough
//       consider http://stackoverflow.com/questions/6206893/how-to-implement-char-ready-in-c
// TODO: a simple wrapper around FILE may not be good enough long-term
// TODO: how exactly mode will be used. need to know r/w, bin/txt
typedef struct {tag_type tag; FILE *fp; int mode;} port_type;
#define make_port(p,f,m) port_type p; p.tag = port_tag; p.fp = f; p.mode = m;

/* Vector type */

typedef struct {tag_type tag; int num_elt; object *elts;} vector_type;
typedef vector_type *vector;

#define make_empty_vector(v) vector_type v; v.tag = vector_tag; v.num_elt = 0; v.elts = NULL;

/* Define cons type. */

typedef struct {tag_type tag; object cons_car,cons_cdr;} cons_type;
typedef cons_type *list;

#define car(x) (((list) x)->cons_car)
#define cdr(x) (((list) x)->cons_cdr)
#define caar(x) (car(car(x)))
#define cadr(x) (car(cdr(x)))
#define cdar(x) (cdr(car(x)))
#define cddr(x) (cdr(cdr(x)))
#define caaar(x) (car(car(car(x))))
#define caadr(x) (car(car(cdr(x))))
#define cadar(x) (car(cdr(car(x))))
#define caddr(x) (car(cdr(cdr(x))))
#define cdaar(x) (cdr(car(car(x))))
#define cdadr(x) (cdr(car(cdr(x))))
#define cddar(x) (cdr(cdr(car(x))))
#define cdddr(x) (cdr(cdr(cdr(x))))
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
cons_type n; n.tag = cons_tag; n.cons_car = a; n.cons_cdr = d;

/* Closure types */

typedef struct {tag_type tag; function_type fn; int num_args; } macro_type;
typedef struct {tag_type tag; function_type fn; int num_args; } closure0_type;
typedef struct {tag_type tag; function_type fn; int num_args; object elt1;} closure1_type;
typedef struct {tag_type tag; function_type fn; int num_args; object elt1,elt2;} closure2_type;
typedef struct {tag_type tag; function_type fn; int num_args; object elt1,elt2,elt3;} closure3_type;
typedef struct {tag_type tag; function_type fn; int num_args; object elt1,elt2,elt3,elt4;} closure4_type;
typedef struct {tag_type tag; function_type fn; int num_args; int num_elt; object *elts;} closureN_type;

typedef closure0_type *closure0;
typedef closure1_type *closure1;
typedef closure2_type *closure2;
typedef closure3_type *closure3;
typedef closure4_type *closure4;
typedef closureN_type *closureN;
typedef closure0_type *closure;
typedef closure0_type *macro;

#define mmacro(c,f) macro_type c; c.tag = macro_tag; c.fn = f; c.num_args = -1;
#define mclosure0(c,f) closure0_type c; c.tag = closure0_tag; c.fn = f; c.num_args = -1;
#define mclosure1(c,f,a) closure1_type c; c.tag = closure1_tag; \
   c.fn = f; c.num_args = -1; c.elt1 = a;
#define mclosure2(c,f,a1,a2) closure2_type c; c.tag = closure2_tag; \
   c.fn = f; c.num_args = -1; c.elt1 = a1; c.elt2 = a2;
#define mclosure3(c,f,a1,a2,a3) closure3_type c; c.tag = closure3_tag; \
   c.fn = f; c.num_args = -1; c.elt1 = a1; c.elt2 = a2; c.elt3 = a3;
#define mclosure4(c,f,a1,a2,a3,a4) closure4_type c; c.tag = closure4_tag; \
   c.fn = f; c.num_args = -1; c.elt1 = a1; c.elt2 = a2; c.elt3 = a3; c.elt4 = a4;
// #define setq(x,e) x = e

#define mlist1(e1) (mcons(e1,nil))
#define mlist2(e2,e1) (mcons(e2,mlist1(e1)))
#define mlist3(e3,e2,e1) (mcons(e3,mlist2(e2,e1)))
#define mlist4(e4,e3,e2,e1) (mcons(e4,mlist3(e3,e2,e1)))
#define mlist5(e5,e4,e3,e2,e1) (mcons(e5,mlist4(e4,e3,e2,e1)))
#define mlist6(e6,e5,e4,e3,e2,e1) (mcons(e6,mlist5(e5,e4,e3,e2,e1)))
#define mlist7(e7,e6,e5,e4,e3,e2,e1) (mcons(e7,mlist6(e6,e5,e4,e3,e2,e1)))

// #define rule(lhs,rhs) (mlist3(quote_equal,lhs,rhs))

#define make_cell(n,a) make_cons(n,a,nil);

/* Primitive types */
//typedef void (*prim_function_type)();
typedef struct {tag_type tag; const char *pname; function_type fn;} primitive_type;
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
  string_type string_t;
} common_type;


#endif /* CYCLONE_TYPES_H */

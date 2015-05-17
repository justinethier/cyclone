/**
 ** This file was automatically generated by the Cyclone scheme compiler
 **
 ** (c) 2014 Justin Ethier
 ** Version 0.0.1 (Pre-release)
 **
 **/

#define funcall0(cfn) ((cfn)->fn)(0,cfn)
/* Return to continuation after checking for stack overflow. */
#define return_funcall0(cfn) \
{char stack; \
 if (check_overflow(&stack,stack_limit1)) { \
     object buf[0]; \
     GC(cfn,buf,0); return; \
 } else {funcall0((closure) (cfn)); return;}}

/* Evaluate an expression after checking for stack overflow. */
#define return_check0(_fn) { \
 char stack; \
 if (check_overflow(&stack,stack_limit1)) { \
     object buf[0];  \
     mclosure0(c1, _fn); \
     GC(&c1, buf, 0); return; \
 } else { (_fn)(0,(closure)_fn); }}

#define funcall1(cfn,a1) if (type_of(cfn) == cons_tag || prim(cfn)) { Cyc_apply(0, (closure)a1, cfn); } else { ((cfn)->fn)(1,cfn,a1);}
/* Return to continuation after checking for stack overflow. */
#define return_funcall1(cfn,a1) \
{char stack; \
 if (check_overflow(&stack,stack_limit1)) { \
     object buf[1]; buf[0] = a1;\
     GC(cfn,buf,1); return; \
 } else {funcall1((closure) (cfn),a1); return;}}

/* Evaluate an expression after checking for stack overflow. */
#define return_check1(_fn,a1) { \
 char stack; \
 if (check_overflow(&stack,stack_limit1)) { \
     object buf[1]; buf[0] = a1; \
     mclosure0(c1, _fn); \
     GC(&c1, buf, 1); return; \
 } else { (_fn)(1,(closure)_fn,a1); }}

#define funcall2(cfn,a1,a2) if (type_of(cfn) == cons_tag || prim(cfn)) { Cyc_apply(1, (closure)a1, cfn,a2); } else { ((cfn)->fn)(2,cfn,a1,a2);}
/* Return to continuation after checking for stack overflow. */
#define return_funcall2(cfn,a1,a2) \
{char stack; \
 if (check_overflow(&stack,stack_limit1)) { \
     object buf[2]; buf[0] = a1;buf[1] = a2;\
     GC(cfn,buf,2); return; \
 } else {funcall2((closure) (cfn),a1,a2); return;}}

/* Evaluate an expression after checking for stack overflow. */
#define return_check2(_fn,a1,a2) { \
 char stack; \
 if (check_overflow(&stack,stack_limit1)) { \
     object buf[2]; buf[0] = a1;buf[1] = a2; \
     mclosure0(c1, _fn); \
     GC(&c1, buf, 2); return; \
 } else { (_fn)(2,(closure)_fn,a1,a2); }}

#include "cyclone.h"
extern object __glo__85Cyc_91version_91banner_85;
extern object __glo_call_91with_91current_91continuation;
extern object __glo_call_95cc;
extern object __glo_char_123_127;
extern object __glo_char_121_127;
extern object __glo_char_125_127;
extern object __glo_char_121_123_127;
extern object __glo_char_125_123_127;
extern object __glo_char_91upcase;
extern object __glo_char_91downcase;
extern object __glo_char_91alphabetic_127;
extern object __glo_char_91upper_91case_127;
extern object __glo_char_91lower_91case_127;
extern object __glo_char_91numeric_127;
extern object __glo_char_91whitespace_127;
extern object __glo_digit_91value;
extern object __glo_foldl;
extern object __glo_foldr;
extern object __glo_not;
extern object __glo_list_127;
extern object __glo_zero_127;
extern object __glo_positive_127;
extern object __glo_negative_127;
extern object __glo_append;
extern object __glo__list;
extern object __glo_make_91list;
extern object __glo_list_91copy;
extern object __glo_map;
extern object __glo_for_91each;
extern object __glo_list_91tail;
extern object __glo_list_91ref;
extern object __glo_list_91set_67;
extern object __glo_reverse;
extern object __glo_boolean_123_127;
extern object __glo_symbol_123_127;
extern object __glo_Cyc_91obj_123_127;
extern object __glo_make_91string;
extern object __glo_error;
extern object __glo_raise;
extern object __glo_raise_91continuable;
extern object __glo_with_91exception_91handler;
extern object __glo__85exception_91handler_91stack_85;
extern object __glo_Cyc_91add_91exception_91handler;
extern object __glo_Cyc_91remove_91exception_91handler;
extern object __glo_lib1_91hello;
extern object __glo_lib1_91test;
#include "runtime.h"
#include "runtime-main.h"
static void __lambda_3(int argc, closure _) ;
static void __lambda_2(int argc, closure _,object r_731) ;
static void __lambda_1(int argc, closure _,object r_732) ;
static void __lambda_0(int argc, closure _,object r_733) ;

static void __lambda_3(int argc, closure _) {
  make_int(c_7318, 0);
return_check1(__lambda_2,&c_7318);; 
}

static void __lambda_2(int argc, closure _,object r_731) {
  make_string(c_7317, "hello");
return_check1(__lambda_1,Cyc_write(&c_7317));; 
}

static void __lambda_1(int argc, closure _,object r_732) {
  return_check1(__lambda_0,Cyc_write(__glo_lib1_91hello));; 
}

static void __lambda_0(int argc, closure _,object r_733) {
  make_string(c_7312, "world");
  __halt(Cyc_write(&c_7312)); 
}

static void c_entry_pt(argc, env,cont) int argc; closure env,cont; { 

  c_schemebase_entry_pt(argc, env,cont);
  c_libslib1_entry_pt(argc, env, cont);
  c_libslib2_entry_pt(argc, env, cont);

  return_check0(__lambda_3);
}
main(int argc,char **argv)
{long stack_size = long_arg(argc,argv,"-s",STACK_SIZE);
 long heap_size = long_arg(argc,argv,"-h",HEAP_SIZE);
 global_stack_size = stack_size;
 global_heap_size = heap_size;
 main_main(stack_size,heap_size,(char *) &stack_size);
 return 0;}

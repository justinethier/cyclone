/** 
 * Cyclone Scheme
 * Copyright (c) 2014, Justin Ethier
 * All rights reserved.
 *
 * This file contains the C runtime used by compiled programs.
 */

#ifndef CYCLONE_RUNTIME_H
#define CYCLONE_RUNTIME_H

/* Error checking definitions */
#define Cyc_check_num_args(fnc_name, num_args, args) { \
  integer_type l = Cyc_length(args); \
  if (num_args > l.value) { \
    char buf[128]; \
    snprintf(buf, 127, "Expected %d arguments but received %d.", num_args, l.value);  \
    Cyc_rt_raise_msg(buf); \
  } \
}

#define Cyc_check_type(fnc_test, tag, obj) { \
  if (eq(boolean_f, fnc_test(obj))) Cyc_invalid_type_error(tag, obj); }

#define Cyc_check_cons_or_nil(obj) { if (!nullp(obj)) { Cyc_check_cons(obj); }}
#define Cyc_check_cons(obj) Cyc_check_type(Cyc_is_cons, cons_tag, obj);
#define Cyc_check_num(obj) Cyc_check_type(Cyc_is_number, integer_tag, obj);
#define Cyc_check_int(obj) Cyc_check_type(Cyc_is_integer, integer_tag, obj);
#define Cyc_check_str(obj) Cyc_check_type(Cyc_is_string, string_tag, obj);
#define Cyc_check_sym(obj) Cyc_check_type(Cyc_is_symbol, symbol_tag, obj);
#define Cyc_check_vec(obj) Cyc_check_type(Cyc_is_vector, vector_tag, obj);
#define Cyc_check_port(obj) Cyc_check_type(Cyc_is_port, port_tag, obj);
#define Cyc_check_fnc(obj) Cyc_check_type(Cyc_is_procedure, closure2_tag, obj);
void Cyc_invalid_type_error(int tag, object found);
void Cyc_check_obj(int tag, object obj);
void Cyc_check_bounds(const char *label, int len, int index);
/* END error checking */

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
int _cyc_argc;
char **_cyc_argv;
object Cyc_get_global_variables();
object Cyc_get_cvar(object var);
object Cyc_set_cvar(object var, object value);
object apply(object cont, object func, object args);
void Cyc_apply(int argc, closure cont, object prim, ...);
integer_type Cyc_string_cmp(object str1, object str2);
void dispatch_string_91append(int argc, object clo, object cont, object str1, ...);
string_type Cyc_string_append(int argc, object str1, ...);
string_type Cyc_string_append_va_list(int, object, va_list);
list mcons(object,object);
cvar_type *mcvar(object *var);
object Cyc_display(object, FILE *port);
object dispatch_display_va(int argc, object clo, object cont, object x, ...);
object Cyc_display_va(int argc, object x, ...);
object Cyc_display_va_list(int argc, object x, va_list ap);
object Cyc_write_char(object c, object port); 
object Cyc_write(object, FILE *port);
object dispatch_write_va(int argc, object clo, object cont, object x, ...);
object Cyc_write_va(int argc, object x, ...);
object Cyc_write_va_list(int argc, object x, va_list ap);

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
integer_type Cyc_vector_length(object v);
object Cyc_vector_ref(object v, object k);
object Cyc_vector_set(object v, object k, object obj);
object Cyc_make_vector(object cont, object len, object fill);
string_type Cyc_number2string(object n) ;
string_type Cyc_symbol2string(object sym) ;
object Cyc_string2symbol(object str);
string_type Cyc_list2string(object lst);
common_type Cyc_string2number(object str);
void dispatch_string_91append(int argc, object clo, object cont, object str1, ...);
string_type Cyc_string_append(int argc, object str1, ...);
string_type Cyc_string_append_va_list(int argc, object str1, va_list ap);
integer_type Cyc_string_length(object str);
string_type Cyc_substring(object str, object start, object end);
object Cyc_string_ref(object str, object k);
object Cyc_string_set(object str, object k, object chr);
string_type Cyc_installation_dir();
object Cyc_command_line_arguments(object cont);
integer_type Cyc_system(object cmd);
integer_type Cyc_char2integer(object chr);
object Cyc_integer2char(object n);
void my_exit(closure);
object __halt(object obj);
port_type Cyc_stdout(void);
port_type Cyc_stdin(void);
port_type Cyc_stderr(void);
port_type Cyc_io_open_input_file(object str);
port_type Cyc_io_open_output_file(object str);
object Cyc_io_close_port(object port);
object Cyc_io_close_input_port(object port);
object Cyc_io_close_output_port(object port);
object Cyc_io_delete_file(object filename);
object Cyc_io_file_exists(object filename);
object Cyc_io_read_char(object port);
object Cyc_io_peek_char(object port);

object Cyc_is_boolean(object o);
object Cyc_is_cons(object o);
object Cyc_is_null(object o);
object Cyc_is_number(object o);
object Cyc_is_real(object o);
object Cyc_is_integer(object o);
object Cyc_is_vector(object o);
object Cyc_is_port(object o);
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
void GC(closure,object*,int);

void Cyc_st_init();
void Cyc_st_add(char *frame);
void Cyc_st_print(FILE *out);

char *_strdup (const char *s);
object add_symbol(symbol_type *psym);
object add_symbol_by_name(const char *name);
object find_symbol_by_name(const char *name);
object find_or_add_symbol(const char *name);
extern list symbol_table;

extern list global_table;
void add_global(object *glo);

void add_mutation(object var, object value);
void clear_mutations();
extern list mutation_table;

void dispatch(int argc, function_type func, object clo, object cont, object args);
void dispatch_va(int argc, function_type_va func, object clo, object cont, object args);
void do_dispatch(int argc, function_type func, object clo, object *buffer);

// Note: below is OK since alloca memory is not freed until function exits
#define list2vector(v, l) object v = nil; { \
  integer_type len; \
  Cyc_check_cons_or_nil(l); \
  len = Cyc_length(l); \
  v = alloca(sizeof(vector_type)); \
  ((vector)v)->tag = vector_tag; \
  ((vector)v)->num_elt = len.value; \
  ((vector)v)->elts = (((vector)v)->num_elt > 0) ? (object *)alloca(sizeof(object) * ((vector)v)->num_elt) : NULL; \
  object lst = l; \
  int i = 0; \
  while(!nullp(lst)) { \
    ((vector)v)->elts[i++] = car(lst); \
    lst = cdr(lst); \
  } \
}

/* Global variables. */
extern clock_t start;   /* Starting time. */
extern char *stack_begin;   /* Initialized by main. */
extern char *stack_limit1;  /* Initialized by main. */
extern char *stack_limit2;
extern char *bottom;    /* Bottom of tospace. */
extern char *allocp;    /* Cheney allocate pointer. */
extern char *alloc_end;
/* TODO: not sure this is the best strategy for strings, especially if there 
   are a lot of long, later gen strings because that will cause a lot of
   copying to occur during GC */
extern char *dhbottom; /* Bottom of data heap */
extern char *dhallocp; /* Current place in data heap */
extern char *dhalloc_limit; /* GC beyond this limit */ 
extern char *dhalloc_end;
extern long no_gcs; /* Count the number of GC's. */
extern long no_major_gcs; /* Count the number of GC's. */
extern object gc_cont;   /* GC continuation closure. */
extern object gc_ans[NUM_GC_ANS];    /* argument for GC continuation closure. */
extern int gc_num_ans;
extern jmp_buf jmp_main; /* Where to jump to. */

/* Define Lisp constants we need. */
extern const object boolean_t;
extern const object boolean_f;
extern const object quote_void;

/* This section is auto-generated via --autogen */
extern const object primitive_Cyc_91global_91vars;
extern const object primitive_Cyc_91get_91cvar;
extern const object primitive_Cyc_91set_91cvar_67;
extern const object primitive_Cyc_91cvar_127;
extern const object primitive_Cyc_91has_91cycle_127;
extern const object primitive__87;
extern const object primitive__91;
extern const object primitive__85;
extern const object primitive__95;
extern const object primitive__123;
extern const object primitive__125;
extern const object primitive__121;
extern const object primitive__125_123;
extern const object primitive__121_123;
extern const object primitive_apply;
extern const object primitive__75halt;
extern const object primitive_exit;
extern const object primitive_Cyc_91current_91exception_91handler;
extern const object primitive_Cyc_91default_91exception_91handler;
extern const object primitive_cons;
extern const object primitive_cell_91get;
extern const object primitive_set_91global_67;
extern const object primitive_set_91cell_67;
extern const object primitive_cell;
extern const object primitive_eq_127;
extern const object primitive_eqv_127;
extern const object primitive_equal_127;
extern const object primitive_assoc;
extern const object primitive_assq;
extern const object primitive_assv;
extern const object primitive_member;
extern const object primitive_memq;
extern const object primitive_memv;
extern const object primitive_length;
extern const object primitive_vector_91length;
extern const object primitive_set_91car_67;
extern const object primitive_set_91cdr_67;
extern const object primitive_car;
extern const object primitive_cdr;
extern const object primitive_caar;
extern const object primitive_cadr;
extern const object primitive_cdar;
extern const object primitive_cddr;
extern const object primitive_caaar;
extern const object primitive_caadr;
extern const object primitive_cadar;
extern const object primitive_caddr;
extern const object primitive_cdaar;
extern const object primitive_cdadr;
extern const object primitive_cddar;
extern const object primitive_cdddr;
extern const object primitive_caaaar;
extern const object primitive_caaadr;
extern const object primitive_caadar;
extern const object primitive_caaddr;
extern const object primitive_cadaar;
extern const object primitive_cadadr;
extern const object primitive_caddar;
extern const object primitive_cadddr;
extern const object primitive_cdaaar;
extern const object primitive_cdaadr;
extern const object primitive_cdadar;
extern const object primitive_cdaddr;
extern const object primitive_cddaar;
extern const object primitive_cddadr;
extern const object primitive_cdddar;
extern const object primitive_cddddr;
extern const object primitive_char_91_125integer;
extern const object primitive_integer_91_125char;
extern const object primitive_string_91_125number;
extern const object primitive_string_91cmp;
extern const object primitive_string_91append;
extern const object primitive_list_91_125string;
extern const object primitive_string_91_125symbol;
extern const object primitive_symbol_91_125string;
extern const object primitive_number_91_125string;
extern const object primitive_string_91length;
extern const object primitive_substring;
extern const object primitive_make_91vector;
extern const object primitive_list_91_125vector;
extern const object primitive_vector_91ref;
extern const object primitive_vector_91set_67;
extern const object primitive_string_91ref;
extern const object primitive_string_91set_67;
extern const object primitive_Cyc_91installation_91dir;
extern const object primitive_command_91line_91arguments;
extern const object primitive_system;
extern const object primitive_boolean_127;
extern const object primitive_char_127;
extern const object primitive_eof_91object_127;
extern const object primitive_null_127;
extern const object primitive_number_127;
extern const object primitive_real_127;
extern const object primitive_integer_127;
extern const object primitive_pair_127;
extern const object primitive_procedure_127;
extern const object primitive_port_127;
extern const object primitive_vector_127;
extern const object primitive_string_127;
extern const object primitive_symbol_127;
extern const object primitive_open_91input_91file;
extern const object primitive_open_91output_91file;
extern const object primitive_close_91port;
extern const object primitive_close_91input_91port;
extern const object primitive_close_91output_91port;
extern const object primitive_file_91exists_127;
extern const object primitive_delete_91file;
extern const object primitive_read_91char;
extern const object primitive_peek_91char;
extern const object primitive_Cyc_91write_91char;
extern const object primitive_Cyc_91write;
extern const object primitive_Cyc_91display;
extern const object primitive_call_95cc;
/* -------------------------------------------- */

/* Globals that are needed by the runtime */
extern object Cyc_glo_eval;
extern object Cyc_glo_call_cc;

#define __glo_eval Cyc_glo_eval
#define __glo_call_95cc Cyc_glo_call_cc

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
void Cyc_rt_raise2(const char *msg, object err);
void Cyc_rt_raise_msg(const char *err);
/* END exception handler */

#endif /* CYCLONE_RUNTIME_H */

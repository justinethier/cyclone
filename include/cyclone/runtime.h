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
#define Cyc_check_num_args(data, fnc_name, num_args, args) { \
  object l = Cyc_length(data, args); \
  if (num_args > obj_obj2int(l)) { \
    char buf[128]; \
    snprintf(buf, 127, "Expected %d arguments to %s but received %ld.", \
             num_args, fnc_name, obj_obj2int(l));  \
    Cyc_rt_raise_msg(data, buf); \
  } \
}

#define Cyc_check_type(data, fnc_test, tag, obj) { \
  if ((boolean_f == fnc_test(obj))) Cyc_invalid_type_error(data, tag, obj); }

#define Cyc_check_pair_or_null(d,obj) { if (obj != NULL) { Cyc_check_pair(d,obj); }}
#define Cyc_check_pair(d,obj) Cyc_check_type(d,Cyc_is_pair, pair_tag, obj);
#define Cyc_check_num(d,obj) Cyc_check_type(d,Cyc_is_number, integer_tag, obj);
#define Cyc_check_int(d,obj) Cyc_check_type(d,Cyc_is_integer, integer_tag, obj);
#define Cyc_check_str(d,obj) Cyc_check_type(d,Cyc_is_string, string_tag, obj);
#define Cyc_check_sym(d,obj) Cyc_check_type(d,Cyc_is_symbol, symbol_tag, obj);
#define Cyc_check_vec(d,obj) Cyc_check_type(d,Cyc_is_vector, vector_tag, obj);
#define Cyc_check_bvec(d,obj) Cyc_check_type(d,Cyc_is_bytevector, bytevector_tag, obj);
#define Cyc_check_port(d,obj) Cyc_check_type(d,Cyc_is_port, port_tag, obj);
#define Cyc_check_mutex(d,obj) Cyc_check_type(d,Cyc_is_mutex, mutex_tag, obj);
#define Cyc_check_cond_var(d,obj) Cyc_check_type(d,Cyc_is_cond_var, cond_var_tag, obj);
void Cyc_invalid_type_error(void *data, int tag, object found);
void Cyc_check_obj(void *data, int tag, object obj);
void Cyc_check_bounds(void *data, const char *label, int len, int index);
/* END error checking */

extern long global_stack_size;
extern long global_heap_size;
extern const object Cyc_EOF;

object cell_get(object cell);

#define global_set(glo,value) Cyc_global_set(data, (object *)&glo, value)
object Cyc_global_set(void *thd, object * glo, object value);

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
  list var = (count > 0) ? alloca(sizeof(pair_type)*count) : NULL; \
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
        var[i].hdr.mark = gc_color_red; \
        var[i].hdr.grayed = 0; \
        var[i].tag = pair_tag; \
        var[i].pair_car = tmp; \
        var[i].pair_cdr = (i == (count-1)) ? NULL : &var[i + 1]; \
      } \
      va_end(va); \
    } \
  }

#define return_inexact_double_op(data, cont, OP, z) \
  make_double(d, 0.0); \
  Cyc_check_num(data, z); \
  if (obj_is_int(z)) { \
    d.value = OP(obj_obj2int(z)); \
  } else if (type_of(z) == integer_tag) { \
    d.value = OP(((integer_type *)z)->value); \
  } else { \
    d.value = OP(((double_type *)z)->value); \
  } \
  return_closcall1(data, cont, &d)

#define return_exact_double_op(data, cont, OP, z) \
  int i = 0; \
  Cyc_check_num(data, z); \
  if (obj_is_int(z)) { \
    i = obj_obj2int(z); \
  } else if (type_of(z) == integer_tag) { \
    i = (int)OP(((integer_type *)z)->value); \
  } else { \
    i = (int)OP(((double_type *)z)->value); \
  } \
  return_closcall1(data, cont, obj_int2obj(i))

#define unbox_number(n) \
  ((obj_is_int(n) ? obj_obj2int(n) : \
                    ((type_of(n) == integer_tag) ? \
                       ((integer_type *)n)->value : \
                       ((double_type *)n)->value)))

/* Prototypes for primitive functions. */

extern object Cyc_global_variables;
int _cyc_argc;
char **_cyc_argv;
void gc_init_heap(long heap_size);
object Cyc_car(void *data, object lis);
object Cyc_cdr(void *data, object lis);
object Cyc_get_global_variables();
object Cyc_get_cvar(object var);
object Cyc_set_cvar(object var, object value);
object apply(void *data, object cont, object func, object args);
void Cyc_apply(void *data, int argc, closure cont, object prim, ...);
void dispatch_apply_va(void *data, int argc, object clo, object cont, object func, ...);
object apply_va(void *data, object cont, int argc, object func, ...);
object Cyc_string_cmp(void *data, object str1, object str2);
object dispatch_string_91append(void *data, int argc, object clo, object cont,
                                object str1, ...);
list malloc_make_pair(object, object);
cvar_type *mcvar(object * var);
object Cyc_display(object, FILE * port);
void dispatch_display_va(void *data, int argc, object clo, object cont,
                         object x, ...);
object Cyc_display_va(int argc, object x, ...);
object Cyc_display_va_list(int argc, object x, va_list ap);
object Cyc_write_char(void *data, object c, object port);
object Cyc_write(object, FILE * port);
void dispatch_write_va(void *data, int argc, object clo, object cont,
                       object x, ...);
object Cyc_write_va(int argc, object x, ...);
object Cyc_write_va_list(int argc, object x, va_list ap);

object Cyc_has_cycle(object lst);
object Cyc_num_eq(void *, object cont, int argc, object n, ...);
object Cyc_num_gt(void *, object cont, int argc, object n, ...);
object Cyc_num_lt(void *, object cont, int argc, object n, ...);
object Cyc_num_gte(void *, object cont, int argc, object n, ...);
object Cyc_num_lte(void *, object cont, int argc, object n, ...);
int Cyc_num_eq_op(void *, object x, object y);
int Cyc_num_gt_op(void *, object x, object y);
int Cyc_num_lt_op(void *, object x, object y);
int Cyc_num_gte_op(void *, object x, object y);
int Cyc_num_lte_op(void *, object x, object y);
object Cyc_num_cmp_va_list(void *data, int argc,
                           int (fn_op(void *, object, object)), object n,
                           va_list ns);
object Cyc_eq(object x, object y);
object Cyc_set_cell(void *, object l, object val);
object Cyc_set_car(void *, object l, object val);
object Cyc_set_cdr(void *, object l, object val);
object Cyc_length(void *d, object l);
integer_type Cyc_length_as_object(void *d, object l);
object Cyc_vector_length(void *data, object v);
object Cyc_vector_ref(void *d, object v, object k);
object Cyc_vector_set(void *d, object v, object k, object obj);
object Cyc_make_vector(void *data, object cont, int argc, object len, ...);
object Cyc_make_bytevector(void *data, object cont, int argc, object len, ...);
object Cyc_bytevector(void *data, object cont, int argc, object bval, ...);
object Cyc_bytevector_length(void *data, object bv);
object Cyc_bytevector_append(void *data, object cont, int _argc, object bv,
                             ...);
object Cyc_bytevector_copy(void *data, object cont, object bv, object start,
                           object end);
object Cyc_bytevector_u8_ref(void *data, object bv, object k);
object Cyc_bytevector_u8_set(void *data, object bv, object k, object b);
object Cyc_utf82string(void *data, object cont, object bv, object start,
                       object end);
object Cyc_string2utf8(void *data, object cont, object str, object start,
                       object end);
object Cyc_list2vector(void *data, object cont, object l);
object Cyc_number2string2(void *data, object cont, int argc, object n, ...);
object Cyc_symbol2string(void *d, object cont, object sym);
object Cyc_string2symbol(void *d, object str);
object Cyc_list2string(void *d, object cont, object lst);
object Cyc_string2number_(void *d, object cont, object str);
object Cyc_string2number2_(void *data, object cont, int argc, object str, ...);
int binstr2int(const char *str);
int octstr2int(const char *str);
object Cyc_string_append(void *data, object cont, int argc, object str1, ...);
object Cyc_string_length(void *data, object str);
object Cyc_substring(void *data, object cont, object str, object start,
                     object end);
object Cyc_string_ref(void *data, object str, object k);
object Cyc_string_set(void *data, object str, object k, object chr);
object Cyc_installation_dir(void *data, object cont, object type);
object Cyc_command_line_arguments(void *data, object cont);
object Cyc_system(object cmd);
object Cyc_char2integer(object chr);
object Cyc_integer2char(void *data, object n);
void Cyc_halt(object obj);
object __halt(object obj);
port_type Cyc_stdout(void);
port_type Cyc_stdin(void);
port_type Cyc_stderr(void);
port_type Cyc_io_open_input_file(void *data, object str);
port_type Cyc_io_open_output_file(void *data, object str);
object Cyc_io_close_port(void *data, object port);
object Cyc_io_close_input_port(void *data, object port);
object Cyc_io_close_output_port(void *data, object port);
object Cyc_io_flush_output_port(void *data, object port);
object Cyc_io_delete_file(void *data, object filename);
object Cyc_io_file_exists(void *data, object filename);
object Cyc_io_read_char(void *data, object cont, object port);
object Cyc_io_peek_char(void *data, object cont, object port);
object Cyc_io_read_line(void *data, object cont, object port);

object Cyc_is_boolean(object o);
object Cyc_is_pair(object o);
object Cyc_is_null(object o);
object Cyc_is_number(object o);
object Cyc_is_real(object o);
object Cyc_is_integer(object o);
object Cyc_is_vector(object o);
object Cyc_is_bytevector(object o);
object Cyc_is_port(object o);
object Cyc_is_mutex(object o);
object Cyc_is_cond_var(object o);
object Cyc_is_symbol(object o);
object Cyc_is_string(object o);
object Cyc_is_char(object o);
object Cyc_is_procedure(void *data, object o);
object Cyc_is_macro(object o);
object Cyc_is_eof_object(object o);
object Cyc_is_cvar(object o);
object Cyc_is_opaque(object o);
object Cyc_sum_op(void *data, common_type * x, object y);
object Cyc_sub_op(void *data, common_type * x, object y);
object Cyc_mul_op(void *data, common_type * x, object y);
object Cyc_div_op(void *data, common_type * x, object y);
object Cyc_sum(void *data, object cont, int argc, object n, ...);
object Cyc_sub(void *data, object cont, int argc, object n, ...);
object Cyc_mul(void *data, object cont, int argc, object n, ...);
object Cyc_div(void *data, object cont, int argc, object n, ...);
object Cyc_num_op_va_list(void *data, int argc,
                          object(fn_op(void *, common_type *, object)),
                          int default_no_args, int default_one_arg, object n,
                          va_list ns, common_type * buf);
int equal(object, object);
list assq(void *, object, list);
list assoc(void *, object x, list l);
object equalp(object, object);
object memberp(void *, object, list);
object memqp(void *, object, list);

object Cyc_spawn_thread(object thunk);
void Cyc_start_trampoline(gc_thread_data * thd);
void Cyc_end_thread(gc_thread_data * thd);
void Cyc_exit_thread(gc_thread_data * thd);
object Cyc_thread_sleep(void *data, object timeout);
void GC(void *, closure, object *, int);
object Cyc_trigger_minor_gc(void *data, object cont);
object copy2heap(void *data, object obj);

void Cyc_st_add(void *data, char *frame);
void Cyc_st_print(void *data, FILE * out);

char *_strdup(const char *s);
object add_symbol(symbol_type * psym);
object add_symbol_by_name(const char *name);
object find_symbol_by_name(const char *name);
object find_or_add_symbol(const char *name);

extern list global_table;
void add_global(object * glo);

void dispatch(void *data, int argc, function_type func, object clo, object cont,
              object args);
void dispatch_va(void *data, int argc, function_type_va func, object clo,
                 object cont, object args);
void do_dispatch(void *data, int argc, function_type func, object clo,
                 object * buffer);

extern const object boolean_t;
extern const object boolean_f;
extern const object quote_void;

/* This section is auto-generated via --autogen */
extern const object primitive_Cyc_91global_91vars;
extern const object primitive_Cyc_91get_91cvar;
extern const object primitive_Cyc_91set_91cvar_67;
extern const object primitive_Cyc_91cvar_127;
extern const object primitive_Cyc_91opaque_127;
extern const object primitive_Cyc_91has_91cycle_127;
extern const object primitive_Cyc_91spawn_91thread_67;
extern const object primitive_Cyc_91end_91thread_67;
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
extern const object primitive_bytevector_91length;
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
extern const object primitive_make_91bytevector;
extern const object primitive_make_91vector;
extern const object primitive_list_91_125vector;
extern const object primitive_vector_91ref;
extern const object primitive_vector_91set_67;
extern const object primitive_bytevector;
extern const object primitive_bytevector_91append;
extern const object primitive_Cyc_91bytevector_91copy;
extern const object primitive_Cyc_91string_91_125utf8;
extern const object primitive_Cyc_91utf8_91_125string;
extern const object primitive_bytevector_91u8_91ref;
extern const object primitive_bytevector_91u8_91set_67;
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
extern const object primitive_macro_127;
extern const object primitive_Cyc_91macro_127;
extern const object primitive_port_127;
extern const object primitive_vector_127;
extern const object primitive_bytevector_127;
extern const object primitive_string_127;
extern const object primitive_symbol_127;
extern const object primitive_open_91input_91file;
extern const object primitive_open_91output_91file;
extern const object primitive_close_91port;
extern const object primitive_close_91input_91port;
extern const object primitive_close_91output_91port;
extern const object primitive_Cyc_91flush_91output_91port;
extern const object primitive_file_91exists_127;
extern const object primitive_delete_91file;
extern const object primitive_read_91char;
extern const object primitive_peek_91char;
extern const object primitive_Cyc_91read_91line;
extern const object primitive_Cyc_91write_91char;
extern const object primitive_Cyc_91write;
extern const object primitive_Cyc_91display;
extern const object primitive_call_95cc;
/* -------------------------------------------- */

/** Globals that are needed by the runtime 
 *  What's going on here is the globals are defined by a module, but
 *  are also used by the runtime. At least for now, macros below are
 *  used to point everybody to the objects.
 *
 *  The assumption for now is that a program that does not include
 *  the necessary libray would never use the corresponding function.
 */
extern object Cyc_glo_eval_from_c;
extern object Cyc_glo_call_cc;

#define __glo_eval_91from_91c_scheme_eval Cyc_glo_eval_from_c
#define __glo_call_95cc_scheme_base Cyc_glo_call_cc

/* Exception handling */
object Cyc_default_exception_handler(void *data, int argc, closure _,
                                     object err);
object Cyc_current_exception_handler(void *data);
void Cyc_rt_raise(void *data, object err);
void Cyc_rt_raise2(void *data, const char *msg, object err);
void Cyc_rt_raise_msg(void *data, const char *err);

double MRG32k3a (double seed);
#endif                          /* CYCLONE_RUNTIME_H */

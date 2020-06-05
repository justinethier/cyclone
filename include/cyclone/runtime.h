/** 
 * Cyclone Scheme
 * Copyright (c) 2014, Justin Ethier
 * All rights reserved.
 *
 * This file contains the C runtime used by compiled programs.
 */

#ifndef CYCLONE_RUNTIME_H
#define CYCLONE_RUNTIME_H


/**
 * The boolean True value.
 * \ingroup objects
 */
extern const object boolean_t;
/**
 * The boolean False value.
 * \ingroup objects
 */
extern const object boolean_f;
/**
 * The void value.
 * \ingroup objects
 */
extern const object quote_void;
/**
 * The EOF value.
 * \ingroup objects
 */
extern const object Cyc_EOF;

/**
 * The void value.
 * \ingroup objects
 */
extern const object Cyc_VOID;

/**
 * \ingroup gc_minor
 */
void GC(void *, closure, object *, int);

/**
 * \ingroup gc_major
 */
void gc_init_heap(long heap_size);

/**
 * \defgroup prim Primitives
 * @brief Built-in Scheme functions provided by the runtime library
 *
 */
/**@{*/

/**
 * \defgroup prim_err Error checking
 * @brief Runtime error checks including object type validation, bounds, and number of function arguments
 *
 */
/**@{*/
#define Cyc_check_num_args(data, fnc_name, num_args, args) { \
  object l = Cyc_length(data, args); \
  if (num_args > obj_obj2int(l)) { \
    char buf[128]; \
    snprintf(buf, 127, "Expected %d arguments to %s but received %ld", \
             num_args, fnc_name, obj_obj2int(l));  \
    Cyc_rt_raise_msg(data, buf); \
  } \
}

#define Cyc_verify_mutable(data, obj) { \
  if (immutable(obj)) Cyc_immutable_obj_error(data, obj); }
#define Cyc_verify_immutable(data, obj) { \
  if (boolean_f == Cyc_is_immutable(obj)) Cyc_mutable_obj_error(data, obj); }
#define Cyc_check_type(data, fnc_test, tag, obj) { \
  if ((boolean_f == fnc_test(obj))) Cyc_invalid_type_error(data, tag, obj); }
#define Cyc_check_type2(data, fnc_test, tag, obj) { \
  if ((boolean_f == fnc_test(data, obj))) Cyc_invalid_type_error(data, tag, obj); }

#define Cyc_check_pair_or_null(d,obj) { if (obj != NULL) { Cyc_check_pair(d,obj); }}
#define Cyc_check_pair(d,obj) Cyc_check_type(d,Cyc_is_pair, pair_tag, obj)
#define Cyc_check_proc(d,obj) Cyc_check_type2(d,Cyc_is_procedure, closureN_tag, obj)
#define Cyc_check_num(d,obj) Cyc_check_type(d,Cyc_is_number, integer_tag, obj)
#define Cyc_check_fixnum(d,obj) Cyc_check_type(d,Cyc_is_fixnum, integer_tag, obj)
#define Cyc_check_int(d,obj) Cyc_check_type(d,Cyc_is_integer, integer_tag, obj)
#define Cyc_check_str(d,obj) Cyc_check_type(d,Cyc_is_string, string_tag, obj)
#define Cyc_check_sym(d,obj) Cyc_check_type(d,Cyc_is_symbol, symbol_tag, obj)
#define Cyc_check_vec(d,obj) Cyc_check_type(d,Cyc_is_vector, vector_tag, obj)
#define Cyc_check_bvec(d,obj) Cyc_check_type(d,Cyc_is_bytevector, bytevector_tag, obj)
#define Cyc_check_port(d,obj) Cyc_check_type(d,Cyc_is_port, port_tag, obj)
#define Cyc_check_mutex(d,obj) Cyc_check_type(d,Cyc_is_mutex, mutex_tag, obj)
#define Cyc_check_cond_var(d,obj) Cyc_check_type(d,Cyc_is_cond_var, cond_var_tag, obj)
#define Cyc_check_atomic(d,obj) Cyc_check_type(d,Cyc_is_atomic, atomic_tag, obj)
#define Cyc_check_opaque(d,obj) Cyc_check_type(d,Cyc_is_opaque, c_opaque_tag, obj)
void Cyc_invalid_type_error(void *data, int tag, object found);
void Cyc_immutable_obj_error(void *data, object obj);
void Cyc_mutable_obj_error(void *data, object obj);
void Cyc_check_obj(void *data, int tag, object obj);
void Cyc_check_bounds(void *data, const char *label, int len, int index);
/**@}*/
/* END error checking */

extern long global_stack_size;
extern long global_heap_size;

char **get_env_variables();
void pack_env_variables(void *data, object k);
void set_env_variables(char **vars);

object cell_get(object cell);

#define global_set(glo,value) Cyc_global_set(data, NULL, (object *)&glo, value)
#define global_set_id(id,glo,value) Cyc_global_set(data, id, (object *)&glo, value)
object Cyc_global_set(void *thd, object sym, object * glo, object value);

#define global_set_cps(thd,k,glo,value) Cyc_global_set_cps(thd, k, NULL, (object *)&glo, value)
#define global_set_cps_id(thd,k,id,glo,value) Cyc_global_set_cps(thd, k, id, (object *)&glo, value)
object Cyc_global_set_cps(void *thd, object cont, object sym, object * glo, object value);

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
        var[i].hdr.immutable = 0; \
        var[i].tag = pair_tag; \
        var[i].pair_car = tmp; \
        var[i].pair_cdr = (i == (count-1)) ? NULL : &var[i + 1]; \
      } \
      va_end(va); \
    } \
  }
/* Prototypes for primitive functions. */

/**
 * \defgroup prim_ctrl Control flow
 * @brief Primitives that control the flow of program execution
 */

/**@{*/
object apply(void *data, object cont, object func, object args);
void Cyc_apply(void *data, int argc, closure cont, object prim, ...);
void dispatch_apply_va(void *data, int argc, object clo, object cont, object func, ...);
object apply_va(void *data, object cont, int argc, object func, ...);
void dispatch(void *data, int argc, function_type func, object clo, object cont,
              object args);
void dispatch_va(void *data, int argc, function_type_va func, object clo,
                 object cont, object args);
void do_dispatch(void *data, int argc, function_type func, object clo,
                 object * buffer);

/**@}*/

/**
 * \defgroup prim_str Strings
 * @brief String functions
 */
/**@{*/
object Cyc_string_cmp(void *data, object str1, object str2);
object dispatch_string_91append(void *data, int argc, object clo, object cont,
                                object str1, ...);
object Cyc_string2number_(void *d, object cont, object str);
object Cyc_string2number2_(void *data, object cont, int argc, object str, ...);
int binstr2int(const char *str);
int octstr2int(const char *str);
object Cyc_string_append(void *data, object cont, int argc, object str1, ...);
object Cyc_string_length(void *data, object str);
object Cyc_string_byte_length(void *data, object str);
object Cyc_substring(void *data, object cont, object str, object start,
                     object end);
object Cyc_string_ref(void *data, object str, object k);
object Cyc_string_set(void *data, object str, object k, object chr);
/**@}*/

/**
 * \defgroup prim_char Characters
 * @brief Character functions
 */
/**@{*/
object Cyc_char2integer(object chr);
object Cyc_char_eq_op(void *data, object a, object b);
object Cyc_char_gt_op(void *data, object a, object b);
object Cyc_char_lt_op(void *data, object a, object b);
object Cyc_char_gte_op(void *data, object a, object b);
object Cyc_char_lte_op(void *data, object a, object b);
/**@}*/

/**
 * \defgroup prim_sym Symbols
 * @brief Symbol functions
 */
/**@{*/
object Cyc_symbol2string(void *d, object cont, object sym);
object Cyc_string2symbol(void *d, object str);
/**@}*/

/**
 * \defgroup prim_cvar C vars
 * @brief Primitives for the C-variable integration type
 */
/**@{*/
extern object Cyc_global_variables;
cvar_type *mcvar(object * var);
object Cyc_get_global_variables();
object Cyc_get_cvar(object var);
object Cyc_set_cvar(object var, object value);
/**@}*/

/**
 * \defgroup prim_io I/O
 * @brief Input/Output functions
 */
/**@{*/
object Cyc_display(void *data, object, FILE * port);
void dispatch_display_va(void *data, int argc, object clo, object cont,
                         object x, ...);
object Cyc_display_va(void *data, int argc, object x, ...);
object Cyc_display_va_list(void *data, int argc, object x, va_list ap);
object Cyc_write_char(void *data, object c, object port);
object Cyc_write(void *data, object, FILE * port);
void dispatch_write_va(void *data, int argc, object clo, object cont,
                       object x, ...);
object Cyc_write_va(void *data, int argc, object x, ...);
object Cyc_write_va_list(void *data, int argc, object x, va_list ap);
port_type Cyc_stdout(void);
port_type Cyc_stdin(void);
port_type Cyc_stderr(void);
port_type Cyc_io_open_input_file(void *data, object str);
port_type Cyc_io_open_output_file(void *data, object str);
port_type Cyc_io_open_binary_input_file(void *data, object str);
port_type Cyc_io_open_binary_output_file(void *data, object str);
port_type *Cyc_io_open_output_string(void *data);
port_type *Cyc_io_open_input_string(void *data, object str);
port_type *Cyc_io_open_input_bytevector(void *data, object bv);
void Cyc_io_get_output_string(void *data, object cont, object port);
void Cyc_io_get_output_bytevector(void *data, object cont, object port);
object Cyc_io_close_port(void *data, object port);
object Cyc_io_close_input_port(void *data, object port);
object Cyc_io_close_output_port(void *data, object port);
object Cyc_io_flush_output_port(void *data, object port);
object Cyc_io_read_char(void *data, object cont, object port);
object Cyc_io_peek_char(void *data, object cont, object port);
object Cyc_write_u8(void *data, object c, object port);
object Cyc_io_read_u8(void *data, object cont, object port);
object Cyc_io_peek_u8(void *data, object cont, object port);
object Cyc_write_bytevector(void *data, object bvec, object port, object start, object end);
object Cyc_io_read_line(void *data, object cont, object port);
void Cyc_io_read_token(void *data, object cont, object port);
/**@}*/


/**
 * \defgroup prim_num Numbers
 * @brief Number functions
 */
/**@{*/

#define return_inexact_double_op_no_cps(data, ptr, OP, z) \
  double unboxed; \
  Cyc_check_num(data, z); \
  if (obj_is_int(z)) { \
    unboxed = OP(obj_obj2int(z)); \
  } else if (type_of(z) == integer_tag) { \
    unboxed = OP(((integer_type *)z)->value); \
  } else if (type_of(z) == bignum_tag) { \
    unboxed = OP(mp_get_double(&bignum_value(z))); \
  } else { \
    unboxed = OP(((double_type *)z)->value); \
  } \
  assign_double(ptr, unboxed); \
  return ptr;

#define return_inexact_double_op(data, cont, OP, z) \
  make_double(d, 0.0); \
  Cyc_check_num(data, z); \
  if (obj_is_int(z)) { \
    d.value = OP(obj_obj2int(z)); \
  } else if (type_of(z) == integer_tag) { \
    d.value = OP(((integer_type *)z)->value); \
  } else if (type_of(z) == bignum_tag) { \
    d.value = OP(mp_get_double(&bignum_value(z))); \
  } else { \
    d.value = OP(((double_type *)z)->value); \
  } \
  return_closcall1(data, cont, &d)

#define return_inexact_double_or_cplx_op_no_cps(data, ptr, OP, CPLX_OP, z) \
  double unboxed; \
  Cyc_check_num(data, z); \
  if (obj_is_int(z)) { \
    unboxed = OP(obj_obj2int(z)); \
  } else if (type_of(z) == integer_tag) { \
    unboxed = OP(((integer_type *)z)->value); \
  } else if (type_of(z) == bignum_tag) { \
    unboxed = OP(mp_get_double(&bignum_value(z))); \
  } else if (type_of(z) == complex_num_tag) { \
    double complex unboxed = CPLX_OP(complex_num_value(z)); \
    assign_complex_num(ptr, unboxed); \
    return ptr; \
  } else { \
    unboxed = OP(((double_type *)z)->value); \
  } \
  assign_double(ptr, unboxed); \
  return ptr;

#define return_inexact_double_or_cplx_op(data, cont, OP, CPLX_OP, z) \
  make_double(d, 0.0); \
  Cyc_check_num(data, z); \
  if (obj_is_int(z)) { \
    d.value = OP(obj_obj2int(z)); \
  } else if (type_of(z) == integer_tag) { \
    d.value = OP(((integer_type *)z)->value); \
  } else if (type_of(z) == bignum_tag) { \
    d.value = OP(mp_get_double(&bignum_value(z))); \
  } else if (type_of(z) == complex_num_tag) { \
    complex_num_type cn; \
    double complex unboxed = CPLX_OP(complex_num_value(z)); \
    assign_complex_num((&cn), unboxed); \
    return_closcall1(data, cont, &cn); \
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
  } else if (type_of(z) == bignum_tag) { \
    return_closcall1(data, cont, z); \
  } else { \
    i = (int)OP(((double_type *)z)->value); \
  } \
  return_closcall1(data, cont, obj_int2obj(i))

#define return_exact_double_op_no_cps(data, ptr, OP, z) \
  int i = 0; \
  Cyc_check_num(data, z); \
  if (obj_is_int(z)) { \
    i = obj_obj2int(z); \
  } else if (type_of(z) == integer_tag) { \
    i = (int)OP(((integer_type *)z)->value); \
  } else if (type_of(z) == bignum_tag) { \
    return z; \
  } else { \
    i = (int)OP(((double_type *)z)->value); \
  } \
  return obj_int2obj(i);

#define unbox_number(n) \
  ((obj_is_int(n) ? obj_obj2int(n) : \
                    ((type_of(n) == integer_tag) ? \
                       ((integer_type *)n)->value : \
                       ((double_type *)n)->value)))

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
object Cyc_num_fast_eq_op(void *data, object x, object y);
object Cyc_num_fast_gt_op(void *data, object x, object y);
object Cyc_num_fast_lt_op(void *data, object x, object y);
object Cyc_num_fast_gte_op(void *data, object x, object y);
object Cyc_num_fast_lte_op(void *data, object x, object y);
object Cyc_num_cmp_va_list(void *data, int argc,
                           int (fn_op(void *, object, object)), object n,
                           va_list ns);
void Cyc_expt(void *data, object cont, object x, object y);
void Cyc_remainder(void *data, object cont, object num1, object num2);
object Cyc_number2string2(void *data, object cont, int argc, object n, ...);
object Cyc_integer2char(void *data, object n);
object Cyc_sum_op(void *data, common_type * x, object y);
object Cyc_sub_op(void *data, common_type * x, object y);
object Cyc_mul_op(void *data, common_type * x, object y);
object Cyc_div_op(void *data, common_type * x, object y);
object Cyc_sum(void *data, object cont, int argc, object n, ...);
object Cyc_sub(void *data, object cont, int argc, object n, ...);
object Cyc_mul(void *data, object cont, int argc, object n, ...);
object Cyc_div(void *data, object cont, int argc, object n, ...);
// Future idea, there may be uses for this in addition to if statements:
#define Cyc_if(c,t,e) ((boolean_f != c) ? (t) : (e))
object Cyc_fast_sum(void *data, object ptr, object x, object y);
object Cyc_fast_sub(void *data, object ptr, object x, object y);
object Cyc_fast_mul(void *data, object ptr, object x, object y);
object Cyc_fast_div(void *data, object ptr, object x, object y);
object Cyc_fast_list_2(object ptr, object x, object y);
object Cyc_fast_list_3(object ptr, object a1, object a2, object a3);
object Cyc_fast_list_4(object ptr, object a1, object a2, object a3, object a4);
object Cyc_fast_vector_2(object ptr, object a1, object a2);
object Cyc_fast_vector_3(object ptr, object a1, object a2, object a3);
object Cyc_fast_vector_4(object ptr, object a1, object a2, object a3, object a4);
object Cyc_fast_vector_5(object ptr, object a1, object a2, object a3, object a4, object a5);
object Cyc_bit_unset(void *data, object n1, object n2); 
object Cyc_bit_set(void *data, object n1, object n2);
object Cyc_num_op_va_list(void *data, int argc,
                          object(fn_op(void *, common_type *, object)),
                          int default_no_args, int default_one_arg, object n,
                          va_list ns, common_type * buf);
void Cyc_int2bignum(int n, mp_int *bn);
object Cyc_bignum_normalize(void *data, object n);
int Cyc_bignum_cmp(bn_cmp_type type, object x, int tx, object y, int ty);
void Cyc_make_rectangular(void *data, object k, object r, object i);
double MRG32k3a (double seed);
/**@}*/
/**
 * \defgroup prim_eq Equality and type predicates
 */
/**@{*/
//object Cyc_eq(object x, object y);
object Cyc_eqv(object x, object y);
#define Cyc_eq(x, y) (make_boolean(x == y))
int equal(object, object);
object equalp(object, object);
object Cyc_has_cycle(object lst);
object Cyc_is_list(object lst);
//object Cyc_is_boolean(object o);
#define Cyc_is_boolean(o) (make_boolean(o == boolean_f || o == boolean_t))
#define Cyc_is_pair(o) ((is_object_type(o) && ((list) o)->tag == pair_tag) ? boolean_t : boolean_f)
#define Cyc_is_null(o) (make_boolean(o == NULL))
//TODO: convert all of these to macros (if it makes sense, most should), and remove them from runtime.c:
object Cyc_is_number(object o);
object Cyc_is_real(object o);
object Cyc_is_integer(object o);
#define Cyc_is_fixnum(o) (make_boolean(obj_is_int(o)))
//object Cyc_is_fixnum(object o);
#define Cyc_is_bignum(o)     (make_boolean(is_object_type(o) && ((list) o)->tag == bignum_tag))
//object Cyc_is_complex(object o);
#define Cyc_is_complex(o)     (make_boolean(is_object_type(o) && ((list) o)->tag == complex_num_tag))
//object Cyc_is_bignum(object o);
//object Cyc_is_vector(object o);
//object Cyc_is_bytevector(object o);
//object Cyc_is_port(object o);
//object Cyc_is_mutex(object o);
//object Cyc_is_cond_var(object o);
//object Cyc_is_symbol(object o);
//object Cyc_is_string(object o);
#define Cyc_is_vector(o)     (make_boolean(is_object_type(o) && ((list) o)->tag == vector_tag))
#define Cyc_is_bytevector(o) (make_boolean(is_object_type(o) && ((list) o)->tag == bytevector_tag))
#define Cyc_is_port(o)       (make_boolean(is_object_type(o) && ((list) o)->tag == port_tag))
#define Cyc_is_mutex(o)      (make_boolean(is_object_type(o) && ((list) o)->tag == mutex_tag))
#define Cyc_is_atomic(o)     (make_boolean(is_object_type(o) && ((list) o)->tag == atomic_tag))
#define Cyc_is_cond_var(o)   (make_boolean(is_object_type(o) && ((list) o)->tag == cond_var_tag))
#define Cyc_is_symbol(o)     (make_boolean(is_object_type(o) && ((list) o)->tag == symbol_tag))
#define Cyc_is_string(o)     (make_boolean(is_object_type(o) && ((list) o)->tag == string_tag))
//object Cyc_is_char(object o);
#define Cyc_is_char(o) (make_boolean(obj_is_char(o)))
object Cyc_is_procedure(void *data, object o);
//object Cyc_is_macro(object o);
//object Cyc_is_eof_object(object o);
//object Cyc_is_cvar(object o);
//object Cyc_is_opaque(object o);
#define Cyc_is_macro(o)       (make_boolean(is_object_type(o) && ((list) o)->tag == macro_tag))
#define Cyc_is_eof_object(o)  (make_boolean(is_object_type(o) && ((list) o)->tag == eof_tag))
#define Cyc_is_void_object(o) (make_boolean(is_object_type(o) && ((list) o)->tag == void_tag))
#define Cyc_is_cvar(o)        (make_boolean(is_object_type(o) && ((list) o)->tag == cvar_tag))
#define Cyc_is_opaque(o)      (make_boolean(is_object_type(o) && ((list) o)->tag == c_opaque_tag))
object Cyc_is_immutable(object obj);
/**@}*/

/**
 * \defgroup prim_vec Vectors
 * @brief Vector functions
 */
/**@{*/
object Cyc_vector_length(void *data, object v);
object Cyc_vector_ref(void *d, object v, object k);
#define Cyc_vector_ref_unsafe(d, v, k) \
  ((vector) v)->elements[obj_obj2int(k)]
object Cyc_vector_set(void *d, object v, object k, object obj);
object Cyc_vector_set_unsafe(void *d, object v, object k, object obj);
object Cyc_vector_set_cps(void *d, object cont, object v, object k, object obj);
object Cyc_vector_set_unsafe_cps(void *d, object cont, object v, object k, object obj);
object Cyc_make_vector(void *data, object cont, int argc, object len, ...);
/**@}*/

/**
 * \defgroup prim_bv Bytevectors
 * @brief Bytevector functions
 */
/**@{*/
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
/**@}*/

/**
 * \defgroup prim_sys System interface
 * @brief Functions for interacting with the system
 */
/**@{*/
extern int _cyc_argc;
extern char **_cyc_argv;
object Cyc_installation_dir(void *data, object cont, object type);
object Cyc_compilation_environment(void *data, object cont, object var);
object Cyc_command_line_arguments(void *data, object cont);
object Cyc_system(object cmd);
void Cyc_halt(object obj);
object __halt(object obj);
object Cyc_io_delete_file(void *data, object filename);
object Cyc_io_file_exists(void *data, object filename);
time_t Cyc_file_last_modified_time(char *path);
/**@}*/

/**
 * \defgroup prim_thd Threads
 * @brief Thread-oriented functions
 *
 * Most of these are internal and should not be called from 
 * an FFI function.
 */
/**@{*/
object Cyc_spawn_thread(object thunk);
void Cyc_start_trampoline(gc_thread_data * thd);
void Cyc_end_thread(gc_thread_data * thd);
void Cyc_exit_thread(gc_thread_data * thd);
object Cyc_thread_sleep(void *data, object timeout);
/**@}*/

/**
 * \defgroup prim_gc Garbage collection
 * @brief Functions to manually trigger a GC
 */
/**@{*/
object Cyc_trigger_minor_gc(void *data, object cont);
object copy2heap(void *data, object obj);
/**@}*/

/**
 * \defgroup prim_ch Call history
 *
 * @brief Functions for maintaining call history.
 */
/**@{*/

//void Cyc_st_add(void *data, char *frame); migrated from runtime.c
/**
 * @brief Register a frame in the stack trace circular buffer.
 * @param data Thread data object
 * @param frame Name of the frame
 */
#define Cyc_st_add(data, frame) \
{ \
  gc_thread_data *thd = (gc_thread_data *) data; \
  /* Do not allow recursion to remove older frames */ \
  if ((char *)frame != thd->stack_prev_frame) { \
    thd->stack_prev_frame = frame; \
    thd->stack_traces[thd->stack_trace_idx] = frame; \
    thd->stack_trace_idx = (thd->stack_trace_idx + 1) % MAX_STACK_TRACES; \
  } \
}

void Cyc_st_print(void *data, FILE * out);
/**@}*/

/**
 * \defgroup prim_obj Primitive objects
 *
 * @brief Objects added to the global environment at runtime as references to the corresponding primitives.
 *
 * This code was originally auto-generated via `--autogen`
 */
/**@{*/
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
extern const object primitive_assq;
extern const object primitive_assv;
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
extern const object primitive_Cyc_91compilation_91environment;
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
extern const object primitive_open_91binary_91input_91file;
extern const object primitive_open_91binary_91output_91file;
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
/**@}*/

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

/**
 * \defgroup prim_ex Exception handling 
 * @brief Raise and handle Scheme exceptions
 */
/**@{*/
object Cyc_default_exception_handler(void *data, int argc, closure _, object err);

object Cyc_current_exception_handler(void *data);
void Cyc_rt_raise(void *data, object err);
void Cyc_rt_raise2(void *data, const char *msg, object err);
void Cyc_rt_raise_msg(void *data, const char *err);
/**@}*/

/**@}*/

/**
 * \defgroup prim_symtbl Symbol table
 *
 * @brief The symbol table, a thread-safe container for all symbols.
 *
 * This table contains a pointer to each symbol used by the current
 * program.
 */
/**@{*/
object add_symbol(symbol_type * psym);
object find_or_add_symbol(const char *name);
/**@}*/

/**
 * \defgroup prim_glo Library table
 *
 * @brief A table of scheme libraries that are loaded.
 */
/**@{*/
object is_library_loaded(const char *name);
object register_library(const char *name);
/**@}*/

/**
 * \defgroup prim_glo Global table
 *
 * @brief A table of global variables.
 */
/**@{*/
extern list global_table;
void add_global(const char *identifier, object * glo);
void Cyc_set_globals_changed(gc_thread_data *thd);
/**@}*/

/**
 * \defgroup prim_utf8 UTF-8
 *
 * @brief Unicode processing using UTF-8
 */
/**@{*/

/** @brief Successful state */
#define CYC_UTF8_ACCEPT 0

/** @brief Invalid state */
#define CYC_UTF8_REJECT 1

/**
 * Simple macro to make it more convenient to convert a single char
 */
#define Cyc_utf8_encode_char(dest, dest_size, char_value) \
  Cyc_utf8_encode(dest, dest_size, &char_value, 1)

int Cyc_utf8_encode(char *dest, int sz, uint32_t *src, int srcsz);
int Cyc_utf8_count_code_points(uint8_t* s);
uint32_t Cyc_utf8_validate_stream(uint32_t *state, char *str, size_t len); 
uint32_t Cyc_utf8_validate(char *str, size_t len);
/**@}*/

/**
 * \defgroup prim_pairs Pairs and lists
 * @brief Functions for working with pairs and lists
 */
/**@{*/
//object Cyc_car(void *data, object lis);
//object Cyc_cdr(void *data, object lis);
static inline object Cyc_car(void *data, object lis)
{
  Cyc_check_pair(data, lis);
  return car(lis);
}

static inline object Cyc_cdr(void *data, object lis)
{
  Cyc_check_pair(data, lis);
  return cdr(lis);
}
// Unsafe car/cdr
#define Cyc_car_unsafe(d, lis) car(lis)
#define Cyc_cdr_unsafe(d, lis) cdr(lis)

list malloc_make_pair(object, object);
object Cyc_set_cell(void *, object l, object val);
object Cyc_set_car(void *, object l, object val);
object Cyc_set_cdr(void *, object l, object val);
object Cyc_set_car_cps(void *, object cont, object l, object val);
object Cyc_set_cdr_cps(void *, object cont, object l, object val);
object Cyc_length(void *d, object l);
object Cyc_length_unsafe(void *d, object l);
object Cyc_list2vector(void *data, object cont, object l);
object Cyc_list2string(void *d, object cont, object lst);
object Cyc_list(void *data, int argc, object cont, ...);
object memberp(void *data, object x, list l);
object memqp(void *data, object x, list l);
list assq(void *data, object x, list l);
list assoc(void *data, object x, list l);
list assoc_cdr(void *data, object x, list l);
/**@}*/


#endif                          /* CYCLONE_RUNTIME_H */

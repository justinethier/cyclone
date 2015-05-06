#include "cyclone.h"

object Cyc_global_variables = nil;

static symbol_type __EOF = {eof_tag, "", nil}; // symbol_type in lieu of custom type
const object Cyc_EOF = &__EOF;

object cell_get(object cell){
    return car(cell);
}

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
{for (; !nullp(l); l = cdr(l))
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


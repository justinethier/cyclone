(import (scheme base)
        (scheme write)
)

;(define (test n) 
;  ((lambda (x) (+ x x))
;   n))
;(write (test 10))

(define (fac n) (if (= n 0) 1 (* n (fac (- n 1)))))

(write (fac 10))
#| Next-gen runtime:

static void __host_lambda_1(void *data, int pc, int argc, object *args) { // TODO: self? cont?
  object top;
  loop:
  top = alloca(
  // TODO: if exceeded stack limit, initiate minor GC
  //       bundle up args, pc, and pass them along

  switch(pc) {
    3: { // Lambda ID 3
      //static void __lambda_3(void *data, int argc, object self_7312, object r_7310) {
      //  return_closcall2(data,  __glo_write_scheme_write,  primitive__75halt, r_7310);; 
      return_closcall2(data,  __glo_write_scheme_write,  primitive__75halt, args[1]); 
      break;
    }
    1: { // Lambda ID 1
//static void __lambda_1(void *data, int argc, closure _,object k_735, object n_731_732) {
//  Cyc_st_add(data, "fac-test.scm:fac");
//  object c_7316 = Cyc_num_fast_eq_op(data,n_731_732, obj_int2obj(0));
//if( (boolean_f != c_7316) ){ 
//  return_closcall1(data,  k_735,  obj_int2obj(1));
//} else { 
//  
//closureN_type c_7319;
//c_7319.hdr.mark = gc_color_red;
// c_7319.hdr.grayed = 0;
//c_7319.tag = closureN_tag;
// c_7319.fn = (function_type)__lambda_2;
//c_7319.num_args = 1;
//c_7319.num_elements = 2;
//c_7319.elements = (object *)alloca(sizeof(object) * 2);
//c_7319.elements[0] = k_735;
//c_7319.elements[1] = n_731_732;
//
//
//complex_num_type local_7329; object c_7330 = Cyc_fast_sub(data,&local_7329,n_731_732, obj_int2obj(1));
//return_closcall2(data,  __glo_fac,  &c_7319, c_7330);}
//; 
//}
      // TODO
      break;
    }
    2: { // Lambda ID 2
      //static void __lambda_2(void *data, int argc, object self_7311, object r_737) {
      //complex_num_type local_7324; object c_7325 = Cyc_fast_mul(data,&local_7324,((closureN)self_7311)->elements[1], r_737);
      //return_closcall1(data,  ((closureN)self_7311)->elements[0],  c_7325);; 
      object local_7324 = alloca(sizeof(complex_num_type));
      object c_7325 = Cyc_fast_mul(data,local_7324,((closureN)args[0])->elements[1], args[1]);
 // TODO: can we be smart enough to call lambda directly, instead of via closure?
      return_closcall1(data,  ((closureN)args[0])->elements[0],  c_7325); 
      break;
    }
    default: {
      // raise error
    }
  }
}
|#

#| Currently-generated code:
static void __lambda_3(void *data, int argc, object self_7312, object r_7310) ;
static void __lambda_1(void *data, int argc, closure _,object k_735, object n_731_732) ;
static void __lambda_2(void *data, int argc, object self_7311, object r_737) ;

static void __lambda_3(void *data, int argc, object self_7312, object r_7310) {
  return_closcall2(data,  __glo_write_scheme_write,  primitive__75halt, r_7310);; 
}

static void __lambda_1(void *data, int argc, closure _,object k_735, object n_731_732) {
  Cyc_st_add(data, "fac-test.scm:fac");
  object c_7316 = Cyc_num_fast_eq_op(data,n_731_732, obj_int2obj(0));
if( (boolean_f != c_7316) ){ 
  return_closcall1(data,  k_735,  obj_int2obj(1));
} else { 
  
closureN_type c_7319;
c_7319.hdr.mark = gc_color_red;
 c_7319.hdr.grayed = 0;
c_7319.tag = closureN_tag;
 c_7319.fn = (function_type)__lambda_2;
c_7319.num_args = 1;
c_7319.num_elements = 2;
c_7319.elements = (object *)alloca(sizeof(object) * 2);
c_7319.elements[0] = k_735;
c_7319.elements[1] = n_731_732;


complex_num_type local_7329; object c_7330 = Cyc_fast_sub(data,&local_7329,n_731_732, obj_int2obj(1));
return_closcall2(data,  __glo_fac,  &c_7319, c_7330);}
; 
}

static void __lambda_2(void *data, int argc, object self_7311, object r_737) {
  
complex_num_type local_7324; object c_7325 = Cyc_fast_mul(data,&local_7324,((closureN)self_7311)->elements[1], r_737);
return_closcall1(data,  ((closureN)self_7311)->elements[0],  c_7325);; 
}

static void c_entry_pt_first_lambda(void *data, int argc, closure cont, object value);
extern void c_schemecyclonecommon_entry_pt(void *data, int argc, closure cont, object value);
extern void c_schemebase_entry_pt(void *data, int argc, closure cont, object value);
extern void c_schemewrite_entry_pt(void *data, int argc, closure cont, object value);
static void c_entry_pt(data, argc, env,cont) void *data; int argc; closure env,cont; { 
Cyc_set_globals_changed((gc_thread_data *)data);

  add_global((object *) &__glo_fac);
  mclosure0(c_7313, (function_type)__lambda_1);c_7313.num_args = 1; 
  __glo_fac = &c_7313; 

  make_cvar(cvar_7334, (object *)&__glo_fac);make_pair(pair_7335, find_or_add_symbol("fac"), &cvar_7334);
make_pair(c_7336, &pair_7335,Cyc_global_variables);
Cyc_global_variables = &c_7336;
mclosure1(c_done, c_entry_pt_first_lambda, &c_done);
mclosure1(c_7337, c_schemewrite_entry_pt, &c_done);
mclosure1(c_7338, c_schemebase_entry_pt, &c_7337);
mclosure1(c_7339, c_schemecyclonecommon_entry_pt, &c_7338);
(c_7339.fn)(data, 0, &c_7339, &c_7339);
}
static void c_entry_pt_first_lambda(void *data, int argc, closure cont, object value) {
  
    
mclosure0(c_7332, (function_type)__lambda_3);c_7332.num_args = 1;
return_closcall2(data,  __glo_fac,  &c_7332, obj_int2obj(10));
;
}
|#

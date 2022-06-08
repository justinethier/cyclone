; ./sync.sh runtime.c gc.c include/cyclone/*.h test-bn.scm && cd ../cyclone-bootstrap && rm -f cyclone libcyclone.a ; ./install.sh && ./cyclone -L. -I. test-bn.scm && ./test-bn && cd ../cyclone
(import (scheme base) (scheme write) (scheme repl))

 (define-c test-bn
   "(void *data, int argc, closure _, object k, object fx, object radix)"
   " object bn = Cyc_int2bignum2(data, obj_obj2int(fx));
     bignum2string(data, k, bn, obj_obj2int(radix));
     ")

 (define-c test-larger-bn
   "(void *data, int argc, closure _, object k, object fx, object fx2, object radix)"
   " bignum2_type *bn = gc_alloc_bignum2(data, 2);
     C_bignum_digits(bn)[0] = obj_obj2int(fx2);
     C_bignum_digits(bn)[1] = obj_obj2int(fx);
     bn->num_digits = 2;
     bn->sign = 0;
     bignum2string(data, k, bn, obj_obj2int(radix));
     ")

(write
  (list
    (test-bn -10 10)
    (test-bn 163264 10)
    (test-bn 16326 10)
    ;(test-bn -16326000 10)
    (test-bn #x0FFFffff 10)
    (test-bn #x0FFFffff 16)
    (test-bn #x3FFFffff 10)
    (test-bn #x0FFF0001 10)
    (test-larger-bn 0 #x0FFF0001 10)
    (test-larger-bn #x3FFF0000 #x0FFF0001 10)
  ))
(newline)
 ;(repl)

; ./sync.sh runtime.c gc.c include/cyclone/*.h test-bn.scm && cd ../cyclone-bootstrap && rm -f cyclone libcyclone.a ; ./install.sh && ./cyclone -L. -I. test-bn.scm && ./test-bn && cd ../cyclone
(import (scheme base) (scheme write) (scheme repl))

 (define-c test-plus
   "(void *data, int argc, closure _, object k, object fx1, object fx2)"
   " object bn1 = Cyc_int2bignum2(data, obj_obj2int(fx1));
     object bn2 = Cyc_int2bignum2(data, obj_obj2int(fx2));
     object result = bignum2_plus_unsigned(data, bn1, bn2, 0); // TODO: int negp);
     return_closcall1(data, k, result); 
     ")
 (define-c test-bn
   "(void *data, int argc, closure _, object k, object fx)"
   " object bn = Cyc_int2bignum2(data, obj_obj2int(fx));
     return_closcall1(data, k, bn); 
     ")

 (define-c test-larger-bn
   "(void *data, int argc, closure _, object k, object fx, object fx2)"
   " bignum2_type *bn = gc_alloc_bignum2(data, 2);
     C_bignum_digits(bn)[0] = obj_obj2int(fx2);
     C_bignum_digits(bn)[1] = obj_obj2int(fx);
     bn->num_digits = 2;
     bn->sign = 0;
     return_closcall1(data, k, bn); 
     ")

(write (test-bn 123456789))
(newline)

(map
  (lambda (row)
    (write row)
    (newline))
  (list
    (test-plus 1 2)
    (test-plus -1 2)
    (test-plus #x0FFFffff #x0FFFffff)
    (test-bn 123456789 )
    (test-bn 123456789 )
    (test-larger-bn 0 #x0FFF0001 )
    (test-bn #x0FFF0001 )

    (test-bn 0 ) ;; TODO: 0 is broken right now!
    (test-bn -10 )
    (test-bn 163264 )
    (test-bn 16326 )
    (test-bn -16326000 )
    (number->string (test-bn #x0FFFffff) 10)
    (number->string (test-bn #x0FFFffff) 16)
    (number->string (test-bn #x0eadbeef) 16)
    (number->string (test-bn #x0eadbeef) 12)
    (test-bn #x0eadbeef)
    (test-bn #x0eadbeef)
    (test-bn #x3FFFffff)
    (test-larger-bn #x3FFF0000 #x0FFF0001 )
  ))
(newline)
 ;(repl)

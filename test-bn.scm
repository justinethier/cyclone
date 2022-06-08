; ./sync.sh runtime.c gc.c include/cyclone/*.h test-bn.scm && cd ../cyclone-bootstrap && rm -f cyclone libcyclone.a ; ./install.sh && ./cyclone -L. -I. test-bn.scm && ./test-bn && cd ../cyclone
(import (scheme base) (scheme write) (scheme repl))

 (define-c test-bn
   "(void *data, int argc, closure _, object k, object fx, object radix)"
   " object bn = Cyc_int2bignum2(data, obj_obj2int(fx));
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
  ))
(newline)
 ;(repl)

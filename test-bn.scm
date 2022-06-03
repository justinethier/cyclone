; /sync.sh runtime.c gc.c include/cyclone/*.h test-bn.scm && cd ../cyclone-bootstrap && rm -f cyclone libcyclone.a ; make cyclone && ./cyclone -L. -I. test-bn.scm && ./test-bn && cd ../cyclone
(import (scheme base) (scheme write) (scheme repl))

 (define-c test-bn
   "(void *data, int argc, closure _, object k, object fx)"
   " object bn = Cyc_int2bignum2(data, obj_obj2int(fx));
     bignum2string(data, k, bn, 10);
     return_closcall1(data, k, bn); ")

(test-bn 10)
(test-bn 16)
 ;(repl)

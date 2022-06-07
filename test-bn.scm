; ./sync.sh runtime.c gc.c include/cyclone/*.h test-bn.scm && cd ../cyclone-bootstrap && rm -f cyclone libcyclone.a ; ./install.sh && ./cyclone -L. -I. test-bn.scm && ./test-bn && cd ../cyclone
(import (scheme base) (scheme write) (scheme repl))

 (define-c test-bn
   "(void *data, int argc, closure _, object k, object fx)"
   " object bn = Cyc_int2bignum2(data, obj_obj2int(fx));
     bignum2string(data, k, bn, 10);
     ")

(test-bn 10)
(test-bn 163264)
(test-bn 16326)
 ;(repl)

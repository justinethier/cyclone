(import (scheme base) (scheme write) (scheme repl))

 (define-c bn-test
   "(void *data, int argc, closure _, object k, object fx)"
   " object bn = Cyc_int2bignum2(data, fx);
     return_closcall1(data, k, bn); ")

 (repl)

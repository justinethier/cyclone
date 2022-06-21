; ./sync.sh runtime.c gc.c include/cyclone/*.h test-bn.scm && cd ../cyclone-bootstrap && rm -f cyclone libcyclone.a ; ./install.sh && ./cyclone -L. -I. test-bn.scm && ./test-bn && cd ../cyclone
(import (scheme base) (scheme write) (scheme repl))

; TODO:
;object str_to_bignum(void *data, object bignum, char *str, char *str_end, int radix);
; (define-c test-str->bignum
;   "(void *data, int argc, closure _, object k, object str )"
;   " bignum2_type *bn = gc_alloc_bignum2(data, 2);
;     C_bignum_digits(bn)[0] = obj_obj2int(fx2);
;     C_bignum_digits(bn)[1] = obj_obj2int(fx);
;     bn->num_digits = 2;
;     bn->sign = 0;
;     return_closcall1(data, k, bn); 
;     ")

 (define-c test-str2bn
   "(void *data, int argc, closure _, object k, object str, object radix)"
   " int len = string_len(str);
     char *s = string_str(str);
     object bn = _str_to_bignum(data, s, s + len, obj_obj2int(radix));
     return_closcall1(data, k, bn); ")

 (define-c test-plus
   "(void *data, int argc, closure _, object k, object fx1, object fx2)"
   " object bn1 = Cyc_int2bignum2(data, obj_obj2int(fx1));
     object bn2 = Cyc_int2bignum2(data, obj_obj2int(fx2));
     object result = bignum2_plus_unsigned(data, bn1, bn2, 0); // TODO: int negp);
if(is_value_type(result)) {
  printf(\"fixnum result\\n\");
} else if (type_of(result) == bignum2_tag) {
  printf(\"bignum result\\n\");
}
     return_closcall1(data, k, result); 
     ")
 (define-c test-minus
   "(void *data, int argc, closure _, object k, object fx1, object fx2)"
   " object bn1 = Cyc_int2bignum2(data, obj_obj2int(fx1));
     object bn2 = Cyc_int2bignum2(data, obj_obj2int(fx2));
     object result = bignum_minus_unsigned(data, bn1, bn2);
if(is_value_type(result)) {
  printf(\"fixnum result\\n\");
} else if (type_of(result) == bignum2_tag) {
  printf(\"bignum result\\n\");
}
     return_closcall1(data, k, result); 
     ")
 (define-c test-times
   "(void *data, int argc, closure _, object k, object fx1, object fx2)"
   " object x = Cyc_int2bignum2(data, obj_obj2int(fx1));
     object y = Cyc_int2bignum2(data, obj_obj2int(fx2));
     int negp = 0;
     // TODO: set negp
     object result = bignum_times_bignum_unsigned(data, x, y, negp);
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

(write
    (test-plus
      (test-str2bn "1234567890123456789012345678901234567890" 10)
      (test-str2bn "1234567890" 10))
)
(newline)

;(write (test-bn 123456789))
;(newline)
;
;(write (test-str2bn "123454354534523454243999" 10))
;(newline)
;
;(write (test-str2bn "-123454354534523454243999" 10))
;(newline)
;
;(write (test-str2bn "123454354534523454243999" 16))
;(newline)
;
;(write (test-str2bn "1234567890123456789012345678901234567890" 10))
;(newline)
;(write (test-str2bn "1234567890" 10))
;(newline)
;
;(write "subtraction")
;(newline)
;(map
;  (lambda (row)
;    (write row)
;    (newline))
;  (list
;    (test-minus 
;      (test-str2bn "1234567890123456789012345678901234567890" 10)
;      (test-str2bn "1234567890" 10))
;    (test-minus
;      (test-str2bn "1234567890" 10)
;      (test-str2bn "1234567890123456789012345678901234567890" 10))
;    (test-minus 1 1)
;    (test-minus 1 2)
;    (test-minus -1 2)
;    (test-minus (- #x0FFFffff) (- #x0FFFffff))
;    (test-minus (- #x2FFFffff) (- #x2FFFffff))
;))
;(newline)
;
;(write "multiplication")
;(newline)
;(map
;  (lambda (row)
;    (write row)
;    (newline))
;  (list
;    (test-times
;      (test-str2bn "1234567890123456789012345678901234567890" 10)
;      (test-str2bn "1234567890" 10))
;    (test-times 1 1)
;    (test-times 1 2)
;    (test-times -1 2)
;    (test-times #x0FFFffff #x0FFFffff)
;    (test-times #x2FFFffff #x2FFFffff)
;))
;
;(write "general")
;(newline)
;(map
;  (lambda (row)
;    (write row)
;    (newline))
;  (list
;    (test-plus
;      (test-str2bn "1234567890123456789012345678901234567890" 10)
;      (test-str2bn "1234567890" 10))
;    (test-plus #x0FFFffff #x0FFFffff)
;    (test-plus #x2FFFffff #x2FFFffff)
;    (test-plus 1 2)
;    ;; TODO: (fixnum? (test-plus 1 2))
;    (test-plus -1 2)
;    (test-bn 123456789 )
;    (test-bn 123456789 )
;    (test-larger-bn 0 #x0FFF0001 )
;    (test-bn #x0FFF0001 )
;
;    (test-bn 0 ) ;; TODO: 0 is broken right now!
;    (test-bn -10 )
;    (test-bn 163264 )
;    (test-bn 16326 )
;    (test-bn -16326000 )
;    (number->string (test-bn #x0FFFffff) 10)
;    (number->string (test-bn #x0FFFffff) 16)
;    (number->string (test-bn #x0eadbeef) 16)
;    (number->string (test-bn #x0eadbeef) 12)
;    (test-bn #x0eadbeef)
;    (test-bn #x0eadbeef)
;    (test-bn #x3FFFffff)
;    (test-larger-bn #x3FFF0000 #x0FFF0001 )
;  ))
;(newline)
 ;(repl)

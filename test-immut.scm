;; A temporary test file
(import (scheme base) (scheme write))


   ;; TODO: make #t respect object type
   (define-c immutable?
     "(void *data, int argc, closure _, object k, object obj)"
     "object result = boolean_t;
      if (is_object_type(obj) &&
          (type_of(obj) == pair_tag ||
           type_of(obj) == vector_tag ||
           type_of(obj) == bytevector_tag ||
           type_of(obj) == string_tag
          ) &&
          !immutable(obj) ) {
        result = boolean_f;
      }
      return_closcall1(data, k, result); ")

(define-c _Cyc-set-immutable!
   "(void *data, int argc, closure _, object k, object obj, object val)"
   "object result = boolean_f;
    if (is_object_type(obj)) {
      immutable(obj) = (val == boolean_f) ? 0 : 1;
      result = boolean_t;
    }
    return_closcall1(data, k, result); ")

(define (Cyc-set-immutable! obj val)
  (_Cyc-set-immutable! obj val)
  (cond
    ((pair? obj) 
     (_Cyc-set-immutable! (car obj) val)
     (_Cyc-set-immutable! (cdr obj) val))
    ((vector? obj) (vector-for-each (lambda (o) (_Cyc-set-immutable! o val)) obj))))

(define lis '(1 2 3))
(define vec '#((1) 2 3))

(write
  (list
    (immutable? lis)
    (immutable? (car lis))
    (Cyc-set-immutable! lis #f)
    (immutable? lis)
    (immutable? (car lis))
    (set-car! lis 'a)
    lis
))
(newline)
(write
  (list
    (immutable? vec) 
    (immutable? (vector-ref vec 0))
    (Cyc-set-immutable! vec #f)
    (immutable? vec) 
    (immutable? (vector-ref vec 0))
    (vector-set! vec 0 'x)
    vec
  )
)

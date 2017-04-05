;; a temporary file to test define-c helper syntax
(import (scheme base) (scheme write))

(define-syntax
  define-c:inline
  (er-macro-transformer
    (lambda (expr rename compare)
      (let ((fnc (car expr))
            (args "object z") ;; TODO: get from expr
            (inline-body "TODO"))
      `(define-c ,fnc
         ,(string-append 
            "(void *data, int argc, closure _, object k, " 
            args
            ")")
         ,(string-append 
            "(void *data, object ptr, " 
            args
            ")"))))))
;  (define-c truncate
;    "(void *data, int argc, closure _, object k, object z)"
;    " return_exact_double_op(data, k, (int), z); "
;    "(void *data, object ptr, object z)"
;    " return_exact_double_op_no_cps(data, ptr, (int), z);")

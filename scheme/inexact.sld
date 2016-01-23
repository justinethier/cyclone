(define-library (scheme inexact)
  (export 
    sin
  )
  (begin
    (define-c sin
      "(void *data, int argc, closure _, object k, object z)"
      " return_inexact_double_op(data, k, sin, z);")
))

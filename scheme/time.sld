(define-library (scheme time)
  (export 
    current-jiffy ;; TODO: This is just temporary, of course
  )
  (import (scheme base) 
  )
  (begin
    (define-c prim-test 
      "(void *data, int argc, closure _, object k, object arg1, object arg2)"
      " return_closcall1(data,  k, arg1); ")
    ;; End FFI
  ))

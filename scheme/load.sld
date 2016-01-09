(define-library (scheme load)
  (export 
    prim-test ;; TODO: This is just temporary, of course
    load)
  (import (scheme base) 
          (scheme eval) 
          (scheme file)
          (scheme read))
  (begin
    ;; Experimenting with what an FFI could look like
    ;; TODO: also need a way to add #include's, and later on compiler options (may already have that, need to check)
    ;;
      ;; want the signature to be similar to this:
      ;; static void __lambda_0(void *data, int argc, closure _,object k_7322, object arg1_737, object arg2_736) {
      ;; lambda portion is computed, so we can't include that.
      ;; compiler would need to insert the "static void (lambda)" part
;; TODO: maybe break up into two args, one being the args list and the other being the function body??
    (define-c prim-test 
      "(void *data, int argc, closure _, object k, object arg1, object arg2)"
      " return_closcall1(data,  k, arg1); ")
    ;; End FFI
    (define (load filename . env)
      (let ((exprs (call-with-input-file filename
                     (lambda (port)
                       (read-all port)))))
        (for-each
          (lambda (expr)
            (apply eval (cons expr env)))
          exprs)))))

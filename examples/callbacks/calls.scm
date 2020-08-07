(import (scheme base)
        (scheme write)
        (srfi 18))

(include-c-header "sys.h")

(define *done* #f)
(define *dummy signal-done) ;; Hack to prevent optimizing-out signal-done

(define-c start-c-thread
  "(void *data, int argc, closure _, object k)"
  "start_c_thread(); 
   return_closcall1(data, k, boolean_t); ")

(define (signal-done obj)
  (write `(Called from C set *done* to ,obj))
  (newline)
  (set! *done* obj)
  #t)

(define (wait)
  (start-c-thread)
  (thread-sleep! 1)
  (if *done*
      #t
      (wait)))

(wait)

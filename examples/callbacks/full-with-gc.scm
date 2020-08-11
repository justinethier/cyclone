(import (scheme base)
        (scheme write)
        (srfi 1)
        (srfi 18))

(include-c-header "full.h")

(define lock (make-mutex))
(define *done* #f)
(define *dummy signal-done) ;; Hack to prevent optimizing out signal-done

(define-c start-c-thread
  "(void *data, int argc, closure _, object k)"
  "start_c_thread(data); 
   return_closcall1(data, k, boolean_t); ")

;; TODO: this does not work right now, it crashes because we are not setup for GC!

;; Signal (wait) that it is done, this is called from C
(define (signal-done) ; obj)
 (let ((result 0)
       (obj #t))
  (for-each
    (lambda (n)
      (set! result (+ result n))
    )
    (iota 1000))

  (write `(Called from C set *done* to ,obj))
  (newline)
  (mutex-lock! lock)
  (set! *done* obj)
  (mutex-unlock! lock)
  result))

;; More efficient to use a condition var here to signal ready,
;; but this is just an example
(define (wait)
  (thread-sleep! 1)
  (mutex-lock! lock)
  (define done *done*)
  (mutex-unlock! lock)
  (if done
      #t
      (wait)))

(start-c-thread) ;; Spawn a separate C thread since main thread runs Scheme
(wait) ;; Wait for the signal

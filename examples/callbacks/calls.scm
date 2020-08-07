(import (scheme base)
        (scheme write)
        (srfi 18))

(include-c-header "sys.h")

(define *done* #f)
(define *dummy signal-done) ;; Hack to prevent optimizing out signal-done

(define-c start-c-thread
  "(void *data, int argc, closure _, object k)"
  "start_c_thread(); 
   return_closcall1(data, k, boolean_t); ")

;; Signal (wait) that it is done, this is called from C
(define (signal-done obj)
  ;; TODO: why is an error raised here???
  ;; because param objects are thread-specific, I think!!!

  ;; TODO: once that works, how to handle exceptions?
  ;;  for quick calls I think we have to assume they will not happen (??)
  ;;  would be best if simple ones could use default handler though
 
  (write `(Called from C set *done* to ,obj))
  (newline)
  (set! *done* obj)
  #t)

;; More efficient to use a condition var here to signal ready,
;; but this is just an example
(define (wait)
  (thread-sleep! 1)
  (if *done*
      #t
      (wait)))

(start-c-thread) ;; Spawn a separate C thread since main thread runs Scheme
(wait) ;; Wait for the signal

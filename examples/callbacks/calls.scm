(import (scheme base)
        (scheme write)
        (srfi 18))

;;(include-c-header "sys.c")

(define *done* #f)
(define *dummy signal-done) ;; Hack to prevent optimizing-out signal-done

(define (signal-done obj)
  (write `(Called from C set *done* to ,obj))
  (newline)
  (set! *done* obj)
  #t)

(define (wait)
  (thread-sleep! 1)
  (if *done*
      #t
      (wait)))

(wait)

(import (scheme base)
        (scheme file)
        (scheme write))

(let loop ((i 10))
  (if (zero? i)
    (write 'done)
    (loop (- i 1))))


(write (when (lambda () #t) 'true))
(write (when (lambda () #f) 'false))

;; Need to fix this up at some point:
;;
;; Lambda application is broken with pure varargs
;; TODO: once this works, add it to test suite!!!
;((lambda test (write test)) 1 2 3 4)
;
;;; This is OK
;;(define test (lambda args args))
;;(write (test 1 2 3 4))

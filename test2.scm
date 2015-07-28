(import (scheme base)
        (scheme file)
        (scheme write))
(write (make-vector 4 #t))
;
;(map
;  (lambda (_)
;(set! x 1)
;;(write x)
;;(write 'Cyc_procedure)
;(open-input-file "1.scm")
;)
;(list 1))
;;; TODO: C macros for funcall1, etc are not being generated even though entries are set
;;;    in the vector. must be another problem inspecting the vector ???
;(write 'hello)
;(let loop ((i 10))
;  (if (zero? i)
;    (write 'done)
;    (loop (- i 1))))
;
;
;(write (command-line-arguments))
;
;(write (when (lambda () #t) 'true))
;(write (when (lambda () #f) 'false))
;
;; Need to fix this up at some point:
;;
;; Lambda application is broken with pure varargs
;; TODO: once this works, add it to test suite!!!
;((lambda test (write test)) 1 2 3 4)
;
;;; This is OK
;;(define test (lambda args args))
;;(write (test 1 2 3 4))

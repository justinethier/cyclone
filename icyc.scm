;; Cyclone Scheme
;; Copyright (c) 2014, Justin Ethier
;; All rights reserved.
;;
;; This module contains a simple Read-Eval-Print Loop
;;
(cond-expand
  (cyclone
    (display *Cyc-version-banner*))
  (else #f))

(define (repl:next-line)
  (call/cc
    (lambda (k)
      (with-exception-handler
        (lambda (obj)
          (display "Error: ")
          (write obj)
          ; TODO:
          ;(for-each
          ;  (lambda (o)
          ;    (display o)
          ;    (display " "))
          ;  obj)
          (k #t))
        (lambda ()
          (repl)))))
  (repl:next-line))

(define (repl)
  (display "cyclone> ")
  (let ((c (eval (read))))
    (cond
      ((not (eof-object? c))
       (write c)
       (repl:next-line))
      (else 
        (exit 0) ;; TODO: crashes on this branch... WTF?
        ))))

(repl:next-line)

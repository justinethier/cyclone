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

;; TODO: define repl iteration, and wrap in an exception handler

(define (repl:next-line)
  (with-exception-handler
    (lambda (obj)
      (write (list 'an-error-occurred obj)))
    (lambda ()
      (repl))))

(define (repl)
  (display "cyclone> ")
  (let ((c (eval (read))))
    (cond
      ((not (eof-object? c))
       (write c)
       (repl:next-line))
      (else #f))))

(repl:next-line)

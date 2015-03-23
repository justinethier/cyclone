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

; TODO: the below is broken because CPS conversion replaces it with:
;
; ((lambda (call/cc)
;    (define repl:next-line
;
; So repl:next-line is never defined as a global!
; We need a better solution

(define (repl:next-line)
  (call/cc
    (lambda (k)
      (with-exception-handler
        (lambda (obj)
          (write (list 'an-error-occurred obj))
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
      (else #f))))

(repl:next-line)

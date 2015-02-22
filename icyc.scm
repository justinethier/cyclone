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
(define (repl)
  (display "cyclone> ")
  (let ((c (eval (read))))
    (cond
      ((not (eof-object? c))
       (write c)
       (repl))
      (else #f))))
(repl)

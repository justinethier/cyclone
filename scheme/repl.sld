;;;; Cyclone Scheme
;;;; https://github.com/justinethier/cyclone
;;;;
;;;; Copyright (c) 2014-2016, Justin Ethier
;;;; All rights reserved.
;;;;
;;;; This module contains the repl library from r7rs.
;;;;
(define-library (scheme repl)
  (export 
    interaction-environment
    repl)
  (import (scheme base)
          (scheme eval)
          (scheme read)
          (scheme write))
  (begin
    (define (interaction-environment)
      (setup-environment))
    (define (repl)
      (with-handler
        (lambda (obj)
          (cond
            ((error-object? obj)
             (display "Error: ")
             (display (error-object-message obj))
             (if (not (null? (error-object-irritants obj)))
                 (display ": "))
             (for-each
               (lambda (o)
                 (write o)
                 (display " "))
               (error-object-irritants obj)))
            (else
              (display "Error: ")
              (display obj)))
          (newline)
          (repl))
        (display "cyclone> ")
        (flush-output-port)
        (let ((obj (read)))
          (if (eof-object? obj)
              (newline) ;; Quick way to exit REPL
              (let ((c (eval obj)))
                (if (eof-object? c)
                    (display "<EOF>")
                    (write c))
                (newline)
                (repl))))))))

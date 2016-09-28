;;;; Cyclone Scheme
;;;; https://github.com/justinethier/cyclone
;;;;
;;;; Copyright (c) 2014-2016, Justin Ethier
;;;; All rights reserved.
;;;;
;;;; This module contains a simple Read-Eval-Print Loop
;;;;
(import (scheme cyclone common)
        (scheme cyclone libraries)
        (scheme cyclone pretty-print)
        (scheme cyclone util)
        (scheme base)
        (scheme char)
        (scheme lazy)
        (scheme load)
        (scheme read)
        (scheme write)
        (scheme inexact)
        (scheme process-context)
        (scheme time)
        (scheme eval)
        ;(srfi 1)
        ;(srfi 2)
        ;(srfi 133)
        (srfi 69))
(cond-expand
  (cyclone
    (display *Cyc-version-banner*))
  (else #f))

(define *icyc-env* (setup-environment))
(define (repl:next-line)
  (call/cc
    (lambda (k)
      (with-exception-handler
        (lambda (obj)
          (display "Error: ")
          (cond
            ((pair? obj)
             (when (string? (car obj))
               (display (car obj))
               (display ": ")
               (set! obj (cdr obj)))
             (for-each
               (lambda (o)
                 (write o)
                 (display " "))
               obj))
            (else
              (display obj)))
          (newline)
          (k #t))
        (lambda ()
          (repl)))))
  (repl:next-line))

(define (repl)
  (display "cyclone> ")
  (let ((c (eval (read) *icyc-env*)))
    (cond
      ((not (eof-object? c))
       (write c)
       (newline)
       (repl:next-line))
      (else 
        (display "\n")
        (exit 0)))))

;; Use a special version of load to pull defs into the repl's env
(define (load2 f)
  (load f *icyc-env*))
(env:define-variable! 'load load2 *icyc-env*)

(let ((args (command-line-arguments)))
  (if (= (length args) 1)
      (load (car args) *icyc-env*))
  (repl:next-line))

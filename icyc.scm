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
        (scheme repl)
        (scheme write)
        (scheme inexact)
        (scheme process-context)
        (scheme time)
        (scheme eval)
        ;(srfi 1)
        ;(srfi 2)
        ;(srfi 133)
        (srfi 69))

;; Collect values for the given command line arguments and option.
;; Will return a list of values for the option.
;; For example:
;;  ("-a" "1" "2") ==> ("1")
;;  ("-a" "1" -a "2") ==> ("1" "2")
(define (collect-opt-values args opt)
  (cdr
    (foldl
      (lambda (arg accum)
        (cond
          ((equal? arg opt)
           (cons opt (cdr accum)))
          ((car accum) ;; we are at an opt value
           (cons #f (cons arg (cdr accum))))
          (else
           (cons #f (cdr accum)))))
      (list #f)
      args)))

;; Use a special version of load to pull defs into the repl's env
;(define (load2 f)
;  (load f *icyc-env*))
;(env:define-variable! 'load load2 *icyc-env*)

(define (usage)
  (display "
Usage: icyc [OPTIONS] [FILENAME]
Starts the interactive interpreter for Cyclone Scheme.

Options:

 -A directory    Append directory to the list of directories that are searched 
                 in order to locate imported libraries.
 -I directory    Prepend directory to the list of directories that are searched 
                 in order to locate imported libraries.
 -p sexp         Evaluate the given S-expression and exit
 -s              Run as a script, without the normal icyc banner
 -h, --help      Display usage information
 -v              Display version information
 -vn             Display version number
")
  (newline))

(let* ((args (command-line-arguments))
       (append-dirs (collect-opt-values args "-A"))
       (prepend-dirs (collect-opt-values args "-I"))
       (run-as-script #f))
  ;; Process arguments used by main REPL
  (if (member "-s" args)
      (set! run-as-script #t))

  ;; Add additional places to look for imports, if necessary
  (if (or (not (null? append-dirs))
          (not (null? prepend-dirs)))
      (%set-import-dirs! append-dirs prepend-dirs))

  ;; Process remaining arguments that, if present, will cause icyc to
  ;; do something other than start the REPL. If no such arguments are
  ;; found, just run icyc.
  (cond
    ((member "-p" args)
     (let ((sexp-strs (collect-opt-values args "-p")))
       (cond
         ((null? sexp-strs)
          (usage))
         (else
          (let* ((sexp-str (apply string-append sexp-strs))
                 (in-port (open-input-string sexp-str))
                 (sexp (cons 'begin (read-all in-port))))
            (display
              (eval sexp))
            (newline)
            (close-port in-port))))))
    ((or (member "-h" args)
         (member "--help" args))
     (usage))
    ((member "-v" args)
     (display *version-banner*))
    ((member "-vn" args)
     (display (Cyc-version)))
    (else
     (if (not run-as-script) 
         (display *Cyc-version-banner*))
     (if (and (>= (length args) 1)
              (not (member (car (reverse args)) '("-s"))))
         (load (car (reverse args)) #;*icyc-env*))
     (repl))))


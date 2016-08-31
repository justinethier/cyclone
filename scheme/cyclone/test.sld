;;;; Cyclone Scheme
;;;; https://github.com/justinethier/cyclone
;;;;
;;;; This module contains a testing framework ported from (chibi test)
;;;; which in turn was ported from CHICKEN.
;;;;
(define-library (scheme cyclone test)
  (export
   warning test-group-inc! print-exception ;; TODO: should not be needed here
                         ;; must be a problem with referential transparency
                         ;; because a syntax-rules macro expands into code
                         ;; that is using these
   test test-equal test-error test-assert test-not test-values
   test-group current-test-group
   test-begin test-end test-syntax-error test-propagate-info
   test-vars test-run test-exit
   current-test-verbosity
   current-test-applier current-test-handler current-test-skipper
   current-test-group-reporter test-failure-count
   current-test-epsilon current-test-comparator)
  (import (scheme base)
          (scheme write)
          (scheme complex)
          (scheme process-context)
          (scheme time)
          ;(chibi term ansi)
  )
  ;(cond-expand
  ; (chibi
  ;  (import (only (chibi) pair-source print-exception protect)))
  ; (else
    (begin

;; TODO: clean this up, extend, and place in its own library (scheme cyclone term)
;; escape sequences from: http://misc.flogisoft.com/bash/tip_colors_and_formatting
;; may be a good use for macros...
      (define (vt100:normal) (string #\escape #\[ #\0 #\m))
      (define (vt100:bold)   (string #\escape #\[ #\1 #\m))
      (define (vt100:underline)   (string #\escape #\[ #\2 #\m))
      (define (vt100:fg:normal) (string #\escape #\[ #\3 #\9 #\m))
      (define (vt100:fg:red)    (string #\escape #\[ #\3 #\1 #\m))
      (define (vt100:fg:green)  (string #\escape #\[ #\3 #\2 #\m))
      (define (vt100:fg:yellow)  (string #\escape #\[ #\3 #\3 #\m))
      (define (bold x) (string-append (vt100:bold) x (vt100:normal)))
      (define (underline x) (string-append (vt100:underline) x (vt100:normal)))
      (define (red x) (string-append (vt100:fg:red) x (vt100:fg:normal)))
      (define (yellow x) (string-append (vt100:fg:yellow) x (vt100:fg:normal)))
      (define (green x) (string-append (vt100:fg:green) x (vt100:fg:normal)))
      (define (pair-source x) #f)
      (define print-exception write)) ;))
  (include "test.scm"))

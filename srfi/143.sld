;;;; Cyclone Scheme
;;;; https://github.com/justinethier/cyclone
;;;;
;;;; Copyright (c) 2014-2016, Justin Ethier
;;;; All rights reserved.
;;;;
;;;; This module is (currently) a stub of SRFI 143.
;;;; Note the SRFI is still in DRAFT status.
;;;;
(define-library (srfi 143)
  (import (scheme base))
  (export
    fx-width
    fx-greatest
    fx-least
    fixnum?
    ;fxzero? fxpositive? fxnegative? fxodd? fxeven?
    ;fx= fx< fx> fx<= fx>=
    ;fxmax fxmin
    ;fx+ fx- fx*
    ;fxabs fxsquare fxsqrt fxexpt
    ;fx+/carry
    ;fx-/carry
    ;fx*+/carry
    ;fxfloor/ fxfloor-quotient fxfloor-remainder
    ;fxceiling/ fxceiling-quotient fxceiling-remainder
    ;fxtruncate/ fxtruncate-quotient fxtruncate-remainder
    ;fxround/ fxround-quotient fxround-remainder
    ;fxeuclidean/ fxeuclidean-quotient fxeuclidean-remainder
    ;fxbalanced/ fxbalanced-quotient fxbalanced-remainder
    ;fxnot
    ;fxand   fxior   fxxor   fxeqv
    ;fxnand  fxnor 
    ;fxandc1 fxandc2 fxorc1  fxorc2 
    ;farithmetic-shift fxbit-count fxinteger-length

    ;fxif 
    ;fxbit-set? fxcopy-bit fxbit-swap
    ;fxany-bit-set? fxevery-bit-set?
    ;fxfirst-set-bit

    ;fxbit-field fxbit-field-any? fxbit-field-every?
    ;fxbit-field-clear fxbit-field-set
    ;fxbit-field-replace  fbit-field-replace-same
    ;fxbit-field-rotate fxbit-field-reverse
    ;fxbit-field-append

    ;fixnum->list list->fixnum
    ;fixnum->vector vector->fixnum
    ;fxbits
    ;fxfold fxfor-each fxunfold
    ;fxlogical-shift
  )
  (begin
    (define (fx-width) 31)
    (define (fx-greatest) 1073741823)
    (define (fx-least)   -1073741824)
    (define-c fixnum?
      "(void *data, int argc, closure _, object k, object obj)"
      " return_closcall1(data, k, 
          obj_is_int(obj) ? boolean_t : boolean_f); ")
  ))


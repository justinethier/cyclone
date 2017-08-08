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
    fx-width fx-greatest fx-least
    fixnum?
    fx=? fx<? fx>? fx<=? fx>=?
    fxzero? fxpositive? fxnegative? fxodd? fxeven?
    fxmax fxmin
    fx+ fx- fx*
    fxneg
    fxquotient fxremainder
    fxabs 
    fxsquare 
    fxsqrt 
; TODO: 
;  fx+/carry fx-/carry fx*/carry

    fxnot fxand fxior fxxor 
    fxarithmetic-shift 
    fxarithmetic-shift-left fxarithmetic-shift-right
;  fxbit-count fxlength 
    fxif ;fxbit-set? fxcopy-bit
;  fxfirst-set-bit fxbit-field
;  fxbit-field-rotate fxbit-field-reverse
  )
  (inline
    fx-width
    fx-greatest
    fx-least
    fixnum?
    fx=?  fx<? fx>? fx<=? fx>=?
    fxzero? fxpositive? fxnegative? fxodd? fxeven?
    fx+ fx- fx*
    fxneg fxquotient fxremainder
    fxsquare
    fxabs 
    fxnot fxand fxior fxxor 
    fxarithmetic-shift 
    fxarithmetic-shift-left fxarithmetic-shift-right
    fxif
  )
  (begin
    (define (fx-width) 31)
    (define (fx-greatest) 1073741823)
    (define (fx-least)   -1073741824)
    (define-c fixnum?
      "(void *data, int argc, closure _, object k, object obj)"
      " return_closcall1(data, k, 
          obj_is_int(obj) ? boolean_t : boolean_f); ")
    (define-syntax bin-num-op
      (er-macro-transformer
        (lambda (expr rename compare)
          (let* ((fnc (cadr expr))
                 (args
                  "(void* data, int argc, closure _, object k, object i, object j)")
                 (op-str (caddr expr))
                 (body
                   (string-append
                    " Cyc_check_fixnum(data, i);
                      Cyc_check_fixnum(data, j);
                      object result = obj_int2obj(obj_obj2int(i) " op-str " obj_obj2int(j));
                      return_closcall1(data, k, result); ")))
            `(define-c ,fnc ,args ,body)))))
    (define-syntax cmp-op
      (er-macro-transformer
        (lambda (expr rename compare)
          (let* ((fnc (cadr expr))
                 (args
                  "(void* data, int argc, closure _, object k, object i, object j)")
                 (op-str (caddr expr))
                 (body
                   (string-append
                    " Cyc_check_fixnum(data, i);
                      Cyc_check_fixnum(data, j);
                      object result = (obj_obj2int(i) " op-str " obj_obj2int(j)) ? boolean_t : boolean_f;
                      return_closcall1(data, k, result); ")))
            `(define-c ,fnc ,args ,body)))))
    (begin
      (bin-num-op fx+ "+")
      (bin-num-op fx- "-")
      (bin-num-op fx* "*")
      (bin-num-op fxquotient "/")
      (bin-num-op fxremainder "%")
      (cmp-op fx=? "==")
      (cmp-op fx<?  "<")
      (cmp-op fx>? ">")
      (cmp-op fx<=? "<=")
      (cmp-op fx>=? ">=")
    )
    (define-c fxzero?
      "(void* data, int argc, closure _, object k, object i)"
      " Cyc_check_fixnum(data, i);
        return_closcall1(data, k, obj_obj2int(i) == 0 ? boolean_t : boolean_f); ")
    (define (fxpositive? i) (fx>? i 0))
    (define (fxnegative? i) (fx<? i 0))
    (define-c fxodd?
      "(void* data, int argc, closure _, object k, object i)"
      " Cyc_check_fixnum(data, i);
        return_closcall1(data, k, obj_obj2int(i) % 2 ? boolean_t : boolean_f); ")
    (define (fxeven? i) 
      (if (fxodd? i) #f #t))
    (define (fxmax first . rest) (foldl (lambda (old new) (if (fx>? old new) old new)) first rest))
    (define (fxmin first . rest) (foldl (lambda (old new) (if (fx<? old new) old new)) first rest))
    (define fxsqrt exact-integer-sqrt)
    (define (fxsquare i) (fx* i i))
    (define (fxneg i) (fx- 0 i))
    (define (fxabs i)
      (if (fxnegative? i) (fxneg i) i))
    (define-c fxnot
       "(void* data, int argc, closure _, object k, object i)"
       " Cyc_check_fixnum(data, i);
         object result = obj_int2obj(~(int)(obj_obj2int(i)));
         return_closcall1(data, k, result); ")
    (bin-num-op fxand "&")
    (bin-num-op fxior "|")
    (bin-num-op fxxor "^")
    (define (fxarithmetic-shift i count)
      (if (fxpositive? count)
          (fxarithmetic-shift-left i count)
          (fxarithmetic-shift-right i (fxneg count))))
    (bin-num-op fxarithmetic-shift-left "<<")
    (bin-num-op fxarithmetic-shift-right ">>")
    (define (fxif mask i j)
      (fxior (fxand (fxnot mask) i) (fxand mask j)))
  ))


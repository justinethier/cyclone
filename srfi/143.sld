;;;; Cyclone Scheme
;;;; https://github.com/justinethier/cyclone
;;;;
;;;; Copyright (c) 2014-2016, Justin Ethier
;;;; All rights reserved.
;;;;
;;;; SRFI 143: Fixnums
;;;;
(define-library (srfi 143)
  (import (scheme base)
          (scheme inexact))
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

    ; TODO: requires SRFI 141
    ;  fx+/carry fx-/carry fx*/carry

    fxnot fxand fxior fxxor 
    fxarithmetic-shift 
    fxarithmetic-shift-left fxarithmetic-shift-right
    fxbit-count 
    fxlength 
    fxif fxbit-set? fxcopy-bit
    fxfirst-set-bit 
    fxbit-field
    fxbit-field-rotate fxbit-field-reverse
  )
  (inline
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
    fxbit-count 
    fxif fxcopy-bit
    fxfirst-set-bit 
    fxbit-field
    mask
  )
  (begin
    (define fx-width 31)
    (define fx-greatest 1073741823)
    (define fx-least   -1073741824)

    (define-syntax bin-num-op
      (er-macro-transformer
        (lambda (expr rename compare)
          (let* ((fnc (cadr expr))
                 (op-str (caddr expr))
                 (zero-check? (and (> (length expr) 3) (cadddr expr)))
                 (args "(void* data, int argc, closure _, object k, object i, object j)")
                 (body
                   (string-append
                    " Cyc_check_fixnum(data, i);
                      Cyc_check_fixnum(data, j); "
                      (if zero-check?
                          " if (obj_obj2int(j) == 0) { Cyc_rt_raise_msg(data, \"Divide by zero\");}"
                          "")
                    " object result = obj_int2obj(obj_obj2int(i) " op-str " obj_obj2int(j));
                      return_closcall1(data, k, result); ")))
            `(define-c ,fnc ,args ,body)))))

    ;; TODO: should be able to support any number of arguments
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

    (bin-num-op fx+ "+")
    (bin-num-op fx- "-")
    (bin-num-op fx* "*")
    (bin-num-op fxquotient "/" #t)
    (bin-num-op fxremainder "%" #t)
    (bin-num-op fxand "&")
    (bin-num-op fxior "|")
    (bin-num-op fxxor "^")
    (bin-num-op fxarithmetic-shift-left "<<")
    (bin-num-op fxarithmetic-shift-right ">>")
    (cmp-op fx=? "==")
    (cmp-op fx<?  "<")
    (cmp-op fx>? ">")
    (cmp-op fx<=? "<=")
    (cmp-op fx>=? ">=")

    (define-c fixnum?
      "(void *data, int argc, closure _, object k, object obj)"
      " return_closcall1(data, k, 
          obj_is_int(obj) ? boolean_t : boolean_f); ")

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
    (define (fxabs i) (if (fxnegative? i) (fxneg i) i))

    (define-c fxnot
       "(void* data, int argc, closure _, object k, object i)"
       " Cyc_check_fixnum(data, i);
         object result = obj_int2obj(~(int)(obj_obj2int(i)));
         return_closcall1(data, k, result); ")

    (define (fxarithmetic-shift i count)
      (if (fxpositive? count)
          (fxarithmetic-shift-left i count)
          (fxarithmetic-shift-right i (fxneg count))))

    (define-c fxbit-count
       "(void* data, int argc, closure _, object k, object i)"
       " Cyc_check_fixnum(data, i);
         unsigned int count = 0;
         int n = obj_obj2int(i);
         while (n) {
           n &= (n - 1);
           count++;
         }
         return_closcall1(data, k, obj_int2obj(count));")

    (define (fxlength  i)
      (exact 
        (ceiling (/ (log (if (fxnegative? i)
                             (fxneg i)
                             (fx+ 1 i)))
                    (log 2)))))

    (define (fxif mask n0 n1)
      (fxior (fxand mask n0)
             (fxand (fxnot mask) n1)))

    (define (fxbit-set? index i)
      (or (%fxbit-set? index i)
          (and (negative? i)
               (>= index (fxlength i)))))

    (define-c %fxbit-set?
       "(void* data, int argc, closure _, object k, object index, object i)"
       " Cyc_check_fixnum(data, index);
         Cyc_check_fixnum(data, i);
         int result = ((obj_obj2int(i)) & (1 << (obj_obj2int(index))));
         return_closcall1(data, k, result ? boolean_t : boolean_f); ")

    (define (fxcopy-bit index to bool)
      (if bool
          (fxior to (fxarithmetic-shift-left 1 index))
          (fxand to (fxnot (fxarithmetic-shift-left 1 index)))))

    ;; Helper function
    (define (mask start end) (fxnot (fxarithmetic-shift-left -1 (- end start))))

    (define (fxfirst-set-bit i) 
      (if (fxzero? i)
          -1
          (- (fxbit-count (fxxor i (- i 1))) 1)))
    
    (define (fxbit-field n start end)
      (fxand (mask start end) (fxarithmetic-shift n (- start))))

    (define (fxbit-field-rotate n count start end)
      (define width (fx- end start))
      (set! count (modulo count width))
      (let ((mask (fxnot (fxarithmetic-shift -1 width))))
        (define zn (fxand mask (fxarithmetic-shift n (- start))))
        (fxior (fxarithmetic-shift
                 (fxior (fxand mask (fxarithmetic-shift zn count))
                         (fxarithmetic-shift zn (- count width)))
                 start)
                (fxand (fxnot (fxarithmetic-shift mask start)) n))))
    
    (define (fxreverse k n)
      (do ((m (if (negative? n) (fxnot n) n) (fxarithmetic-shift-right m 1))
           (k (fx+ -1 k) (fx+ -1 k))
           (rvs 0 (fxior (fxarithmetic-shift-left rvs 1) (fxand 1 m))))
          ((fxnegative? k) (if (fxnegative? n) (fxnot rvs) rvs))))
    
    (define (fxbit-field-reverse n start end)
      (define width (- end start))
      (let ((mask (fxnot (fxarithmetic-shift-left -1 width))))
        (define zn (fxand mask (fxarithmetic-shift-right n start)))
        (fxior (fxarithmetic-shift-left (fxreverse width zn) start)
                (fxand (fxnot (fxarithmetic-shift-left mask start)) n))))
  ))


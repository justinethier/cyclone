;;;; Cyclone Scheme
;;;; https://github.com/justinethier/cyclone
;;;; 
;;;; Copyright (c) 1991, 1993, 2001, 2003, 2005 Aubrey Jaffer
;;;; Copyright (c) 2017, Koz Ross, Justin Ethier 
;;;; 
;;;; All rights reserved.
;;;; 
;;;; Redistribution and use in source and binary forms, with or without
;;;; modification, are permitted provided that the following conditions are met:
;;;;     * Redistributions of source code must retain the above copyright
;;;;       notice, this list of conditions and the following disclaimer.
;;;;     * Redistributions in binary form must reproduce the above copyright
;;;;       notice, this list of conditions and the following disclaimer in the
;;;;       documentation and/or other materials provided with the distribution.
;;;;     * Neither the name of Cyclone nor the
;;;;       names of its contributors may be used to endorse or promote products
;;;;       derived from this software without specific prior written permission.
;;;; 
;;;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
;;;; ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
;;;; DISCLAIMED. IN NO EVENT SHALL <COPYRIGHT HOLDER> BE LIABLE FOR ANY
;;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
;;;; (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
;;;; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
;;;; ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;;;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;;; 
 
(define-syntax binop
  (er-macro-transformer
    (lambda (expr rename compare)
      (let* ((fnc (cadr expr))
             (args
              "(void* data, int argc, closure _, object k, object x, object y)")
             (int-code (caddr expr))
             (bn-op-code (cadddr expr))
             (body
              (string-append
  "Cyc_check_int(data, x);
   Cyc_check_int(data, y);

   if (obj_is_int(x) && obj_is_int(y)) {"
   int-code
   "} else {
     int result;
     alloc_bignum(data, bn);
     mp_int *xx, *yy;
     mp_int tmpx, tmpy;

     if (obj_is_int(x)) {
       BIGNUM_CALL(mp_init(&tmpx));
       Cyc_int2bignum(obj_obj2int(x), &tmpx);
       xx = &tmpx;
     } else {
       xx = &bignum_value(x);
     }

     if (obj_is_int(y)) {
       BIGNUM_CALL(mp_init(&tmpy));
       Cyc_int2bignum(obj_obj2int(y), &tmpy);
       yy = &tmpy;
     } else {
       yy = &bignum_value(y);
     }

     "
     bn-op-code
     "
     if (MP_OKAY != result) {
      char buffer[128];
      snprintf(buffer, 127, \"Bignum error: %s\", mp_error_to_string(result));
      Cyc_rt_raise_msg(data, buffer);
     }
     return_closcall1(data, k, Cyc_bignum_normalize(data, bn));
   }
  ")))
        `(define-c ,fnc ,args ,body)))))

(begin 
  (binop 
    raw-logand
    " int result = ((int)unbox_number(x)) & ((int)unbox_number(y));
      return_closcall1(data, k, obj_int2obj(result)); "
    " result = mp_and(xx, yy, &(bignum_value(bn))); ")
  (binop 
    raw-logior
    " int result = ((int)unbox_number(x)) | ((int)unbox_number(y));
      return_closcall1(data, k, obj_int2obj(result)); "
    " result = mp_or(xx, yy, &(bignum_value(bn))); ")
  (binop 
    raw-logxor
    " int result = ((int)unbox_number(x)) ^ ((int)unbox_number(y));
      return_closcall1(data, k, obj_int2obj(result)); "
    " result = mp_xor(xx, yy, &(bignum_value(bn))); ")
   )

(define (logand x . rest)
  (if (null? rest)
    x
    (apply logand (raw-logand x (car rest)) (cdr rest))))

(define bitwise-and logand)

(define (logior x . rest)
  (if (null? rest)
    x
    (apply logior (raw-logior x (car rest)) (cdr rest))))

(define bitwise-ior logior)

(define (logxor x . rest)
  (if (null? rest)
    x
    (apply logxor (raw-logxor x (car rest)) (cdr rest))))

(define bitwise-xor logxor)

(define-c lognot
  "(void* data, int argc, closure _, object k, object x)"
  "Cyc_check_int(data, x);
   alloc_bignum(data, bn);
   if (Cyc_is_bignum(x) == boolean_t) {
     BIGNUM_CALL(mp_copy(&bignum_value(x), &bignum_value(bn)));
   } else {
     Cyc_int2bignum((int)unbox_number(x), &bignum_value(bn));
   }

   // From https://github.com/libtom/libtommath/issues/30
   /* A one's complement, aka bitwise NOT, is actually just -a - 1 */
   //CHECK_ERROR(mp_neg(&op->mp, &out->mp));
   //CHECK_ERROR(mp_sub_d(&out->mp, 1, &out->mp));
   BIGNUM_CALL(mp_neg(&bignum_value(bn), &bignum_value(bn)));
   BIGNUM_CALL(mp_sub_d(&bignum_value(bn), 1, &bignum_value(bn)));
   return_closcall1(data, k, Cyc_bignum_normalize(data, bn));
   ")

(define bitwise-not lognot)

;;(define-c bitwise-if
;;  "(void* data, int argc, closure _, object k, 
;;          object mask, object n0, object n1)"
;;  "Cyc_check_fixnum(data, mask); // TODO: bignum support
;;   Cyc_check_fixnum(data, n0);
;;   Cyc_check_fixnum(data, n1);
;;   int m = unbox_number(mask);
;;   int result = (m & ((int)unbox_number(n0))) | ((~m) & ((int)unbox_number(n1)));
;;   return_closcall1(data, k, obj_int2obj(result));")

(define (bitwise-if mask n0 n1)
  (logior (logand mask n0)
          (logand (lognot mask) n1)))

(define bitwise-merge bitwise-if)

(define (logtest n1 n2)
  (not (zero? (logand n1 n2))))

(define any-bits-set? logtest)

(define (logcount n)
  (define lookup #u8(0 1 1 2 1 2 2 3 1 2 2 3 2 3 3 4))
  (define (logcount-rec n tot)
    (if (zero? n)
      tot
      (logcount-rec (quotient n 16) 
                    (+ (bytevector-u8-ref lookup (modulo n 16)) tot))))
  (cond
    ((negative? n) (logcount-rec (lognot n) 0))
    ((positive? n) (logcount-rec n 0))
    (else 0)))

(define bit-count logcount)

(define-c integer-length
  "(void* data, int argc, closure _, object k, object x)"
  "Cyc_check_int(data, x);
   if (Cyc_is_bignum(x) == boolean_t) {
     int res;
     BIGNUM_CALL(mp_radix_size(&bignum_value(x), 2, &res));
     return_closcall1(data, k, obj_int2obj((res - 1)));
   } else {
     int input = (int)unbox_number(x);
     int res = 0;
     while (input) {
           res++;
           input >>= 1;
     };
     return_closcall1(data, k, obj_int2obj(res));
   }")

(define (log2-binary-factors n)
  (- (integer-length (raw-logand n (- n))) 1))

(define first-set-bit log2-binary-factors)

(define (logbit? index n)
  (logtest (ash 1 index) n))

(define bit-set? logbit?)

(define (copy-bit index to bool)
  (if bool
    (logior to (ash 1 index))
    (logand to (lognot (ash 1 index)))))

(define (bit-field n start end)
  (logand (lognot (ash -1 (- end start)))
          (ash n (- start))))

(define (copy-bit-field to from start end)
  (bitwise-if (ash (lognot (ash -1 (- end start))) start)
              (ash from start)
              to))

;(define-c ash
;  "(void* data, int argc, closure _, object k, object x, object y)"
;  "Cyc_check_int(data, x);
;   Cyc_check_int(data,y);
;   int bf = (int)unbox_number(x);
;   int shift = (int)unbox_number(y);
;   //int i;
;   if (shift > 0) {
;     bf <<= shift;
;   } else {
;     bf >>= abs(shift);
;   }
;//   if (shift > 0) {
;//         for (i = 0; i < shift; i++) {
;//                  bf *= 2;
;//         }
;//   } else {
;//         for (i = 0; i < abs(shift); i++) {
;//                 bf /= 2; 
;//         }
;//   }
;   return_closcall1(data, k, obj_int2obj(bf))")

 (define-c ash
   "(void* data, int argc, closure _, object k, object x, object y)"
   "Cyc_check_int(data, x);
    Cyc_check_fixnum(data,y);
    int shift, i;
    //int result;
    alloc_bignum(data, bn);
 
    if (Cyc_is_bignum(x) == boolean_t){
      BIGNUM_CALL(mp_copy(&bignum_value(x), &bignum_value(bn)));
    } else {
      Cyc_int2bignum((int)unbox_number(x), &bignum_value(bn));
    }
 
// Inefficient but always works without overflow
// Should be able to do pure fixnum math in some cases, though
    shift = (int)unbox_number(y);
    if (shift > 0) {
      for (i = 0; i < shift; i++) {
         BIGNUM_CALL(mp_mul_2(&bignum_value(bn), &bignum_value(bn)));
      }
    } else {
      for (i = 0; i < abs(shift); i++) {
         BIGNUM_CALL(mp_div_2(&bignum_value(bn), &bignum_value(bn)));
      }
    }

    return_closcall1(data, k, Cyc_bignum_normalize(data, bn));")

(define arithmetic-shift ash)

(define (rotate-bit-field n count start end)
  (define width (- end start))
  (set! count (modulo count width))
  (let ((mask (lognot (ash -1 width))))
    (define zn (logand mask (ash n (- start))))
    (logior (ash
             (logior (logand mask (ash zn count))
                     (ash zn (- count width)))
             start)
            (logand (lognot (ash mask start)) n))))

(define (bit-reverse k n)
  (do ((m (if (negative? n) (lognot n) n) (ash m -1))
       (k (+ -1 k) (+ -1 k))
       (rvs 0 (logior (ash rvs 1) (logand 1 m))))
      ((negative? k) (if (negative? n) (lognot rvs) rvs))))

(define (reverse-bit-field n start end)
  (define width (- end start))
  (let ((mask (lognot (ash -1 width))))
    (define zn (logand mask (ash n (- start))))
    (logior (ash (bit-reverse width zn) start)
            (logand (lognot (ash mask start)) n))))

(define (integer->list k . len)
  (if (null? len)
      (do ((k k (ash k -1))
           (lst '() (cons (odd? k) lst)))
          ((<= k 0) lst))
      (do ((idx (- (car len) 1) (- idx 1))
           (k k (ash k -1))
           (lst '() (cons (odd? k) lst)))
          ((negative? idx) lst))))

(define (list->integer bools)
  (do ((bs bools (cdr bs))
       (acc 0 (+ acc acc (if (car bs) 1 0))))
      ((null? bs) acc)))

(define (booleans->integer . bools)
  (list->integer bools))

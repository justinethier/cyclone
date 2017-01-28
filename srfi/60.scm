;;; "60.scm", bit access and operations for integers for Scheme
;;; Copyright (C) 1991, 1993, 2001, 2003, 2005 Aubrey Jaffer
;;; Copyright (C) 2017 Koz Ross
;
;Permission to copy this software, to modify it, to redistribute it,
;to distribute modified versions, and to use it for any purpose is
;granted, subject to the following restrictions and understandings.
;
;1.  Any copy made of this software must include this copyright notice
;in full.
;
;2.  I have made no warranty or representation that the operation of
;this software will be error-free, and I am under no obligation to
;provide any services, by way of maintenance, update, or otherwise.
;
;3.  In conjunction with products arising from the use of this
;material, there shall be no use of my name in any advertising,
;promotional, or sales literature without prior written consent in
;each case.

(define-c raw-logand
  "(void* data, int argc, closure _, object k, object x, object y)"
  "Cyc_check_int(data, x);
  Cyc_check_int(data, y);
  int result = ((int)unbox_number(x)) & ((int)unbox_number(y));
  return_closcall1(data, k, obj_int2obj(result));")

(define (logand x . rest)
  (if (null? rest)
    x
    (logand (raw-logand x (car rest)) (cdr rest))))

(define bitwise-and logand)

(define-c raw-logior
  "(void* data, int argc, closure _, object k, object x, object y)"
  "Cyc_check_int(data, x);
  Cyc_check_int(data, y);
  int result = ((int)unbox_number(x)) | ((int)unbox_number(y));
  return_closcall1(data, k, obj_int2obj(result));")

(define (logior x . rest)
  (if (null? rest)
    x
    (logior (raw-logior x (car rest)) (cdr rest))))

(define bitwise-ior logior)

(define-c raw-logxor
  "(void* data, int argc, closure _, object k, object x, object y)"
  "Cyc_check_int(data, x);
  Cyc_check_int(data, y);
  int result = ((int)unbox_number(x)) ^ ((int)unbox_number(y));
  return_closcall1(data, k, obj_int2obj(result));")

(define (logxor x . rest)
  (if (null? rest)
    x
    (logxor (raw-logxor x (car rest)) (cdr rest))))

(define-c lognot
  "(void* data, int argc, closure _, object k, object x)"
  "Cyc_check_int(data, x);
  int result = ~(((int)unbox_number(x)));
  return_closcall1(data, k, obj_int2obj(result));")

(define bitwise-not lognot)

(define-c bitwise-if
  "(void* data, int argc, closure _, object k, 
          object mask, object n0, object n1)"
  "Cyc_check_int(data, mask);
  Cyc_check_int(data, n0);
  Cyc_check_int(data, n1);
  int m = unbox_number(mask);
  int result = (m & ((int)unbox_number(n0))) | ((~m) & ((int)unbox_number(n1)));
  return_closcall1(data, k, obj_int2obj(result));")

(define bitwise-merge bitwise-if)

(define (logtest n1 n2)
  (not (zero? (logand n1 n2))))

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

(define (integer-length x)
  (exact (ceiling (log x 2))))

(define (log2-binary-factors n)
  (- (integer-length (logand n (- n))) 1))

(define first-set-bit log2-binary-factors)

(define (logbit? index n)
  (logtest (expt 2 index) n))

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

(define-c ash
  "(void* data, int argc, closure _, object k, object n, object count)"
  "Cyc_check_int(data, n);
  Cyc_check_int(data, count);
  int x = unbox_number(n);
  int y = unbox_number(count);
  int result = (y < 0) ? (x >> y) : (x << y);
  return_closcall1(data, k, obj_int2obj(result));")

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
       (k (- k 1) (- k 1))
       (rvs 0 (logior (ash rvs 1) (logand 1 m))))
      ((negative? k) (if (negative? n) (lognot rvs) rvs))))

(define (reverse-bit-field n start end)
  (define width (- end start))
  (let ((mask (lognot (ash -1 width))))
    (define zn (logand mask (arithmetic-shift n (- start))))
    (logior (arithmetic-shift (bit-reverse width zn) start)
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

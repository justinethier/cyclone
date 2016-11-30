;;; Below are some default comparators provided by SRFI-114,
;;; but not SRFI-128, which this SRFI has transitioned to
;;; depend on. See the rationale for SRFI-128 as to why it is
;;; preferred in usage compared to SRFI-114.

;; Most if not all of this code is taken from SRFI-114

; (define exact inexact->exact)

(define string-foldcase string-downcase)

(define (make-comparison=/< = <)
  (lambda (a b)
    (cond
      ((= a b) 0)
      ((< a b) -1)
      (else 1))))

;; Comparison procedure for real numbers only
(define (real-comparison a b)
  (cond
    ((< a b) -1)
    ((> a b) 1)
    (else 0)))

;; Comparison procedure for non-real numbers.
(define (complex-comparison a b)
  (let ((real-result (real-comparison (real-part a) (real-part b))))
    (if (= real-result 0)
      (real-comparison (imag-part a) (imag-part b))
      real-result)))

(define (number-hash obj) (exact (abs obj)))

(define number-comparator
  (make-comparator number? = complex-comparison number-hash))

(define char-comparison (make-comparison=/< char=? char<?))

(define (char-hash obj) (abs (char->integer obj)))

(define char-comparator
  (make-comparator char? char=? char-comparison char-hash))

;; Makes a hash function that works vectorwise
(define limit (expt 2 20))

(define (make-vectorwise-hash hash length ref)
  (lambda (obj)
    (let loop ((index (- (length obj) 1)) (result 5381))
      (if (= index 0)
        result
        (let* ((prod (modulo (* result 33) limit))
               (sum (modulo (+ prod (hash (ref obj index))) limit)))
          (loop (- index 1) sum))))))

(define string-hash
  (make-vectorwise-hash char-hash string-length string-ref))

(define string-comparison (make-comparison=/< string=? string<?))

(define string-ci-comparison (make-comparison=/< string-ci=? string-ci<?))

(define string-comparator
  (make-comparator string? string=? string-comparison string-hash))

(define (string-ci-hash obj) (string-hash (string-foldcase obj)))

(define string-ci-comparator
  (make-comparator string? string-ci=? string-ci-comparison string-ci-hash))

(define eq-comparator
  (make-comparator
    #t
    eq?
    #f
    default-hash))

(define eqv-comparator
  (make-comparator
    #t
    eqv?
    #f
    default-hash))

(define equal-comparator
  (make-comparator
    #t
    equal?
    #f
    default-hash))

;;;; Cyclone Scheme
;;;; https://github.com/justinethier/cyclone
;;;;
;;;; Copyright (c) 2014-2021, Justin Ethier
;;;; All rights reserved.
;;;;
;;;; This module contains unit tests for threading / SRFI 18.
;;;;

(import 
  (scheme base)
  (scheme eval)
  (scheme inexact)
  (scheme write)
  (cyclone test))


(define vec #(1 2))

(test-group
  "vector literals"
  (test #(1 2) vec)
  (test vec (vector 1 2))
)

(test-group
  "strings"
  (test "??>" "??>")
)

(test-group
  "make-list"
  (test '() (make-list -2))
)

(test-group
  "apply"
  (test '(5 1 2) (eval '(apply cons '(5 (1 2)))))
  (test '(5 1 2) (apply cons '(5 (1 2))))
)

(cond-expand
  (memory streams
    (test-group
      "I/O"
      (define p (open-input-string "one\ntwo\n"))
      (test #\o (read-char p))
      (test "ne" (read-line p))
      (test "two" (read-line p))
      (test (eof-object) (read-line p))
      (define p (open-input-string "one\ntwo\n"))
      (test "one" (read-line p))
      (test #\t (read-char p))
      (test #\w (read-char p))
      (test "o" (read-line p))
    )
  )
  (else #f)
)

(test-group
  "rationals"
  (test 3.0 (numerator (/ 6 4)))
  (test 2.0 (denominator (/ 6 4)))
  (test 3.0 (expt 81 1/4))
  (test #t
    (< 1.0e+40 
       (/ 33333333333333333333333333333333333333333 3.0)
       1.2e+40))
)

(test-group
  "numeric operations - floor, truncate, "
  (test -1 (truncate -1))
  (test -1.0 (truncate -1.0))
  (test -1.0 (truncate -1.1))
  (test -1.0 (truncate -1.1))
  (test +inf.0 (truncate +inf.0))

  (test (values 2 1) (floor/ 5 2))
  (test (values -3 1) (floor/ -5 2))
  (test (values -3 -1) (floor/ 5 -2))
  (test (values 2 -1) (floor/ -5 -2))
  (test (values 2 1) (truncate/ 5 2))
  (test (values -2 -1) (truncate/ -5 2))
  (test (values -2 1) (truncate/ 5 -2))
  (test (values 2 -1) (truncate/ -5 -2))
  (test (values 2.0 -1.0) (truncate/ -5.0 -2))

  (test 4 (gcd 32 -36))
  (test 0 (gcd))
  (test 288 (lcm 32 -36))
  (test 288.0 (lcm 32.0 -36))
  (test 1 (lcm))

  (test -5.0 (floor -4.3))
  (test -4.0 (ceiling -4.3))
  (test -4.0 (truncate -4.3))
  (test -4.0 (round -4.3))
  (test 3.0  (floor 3.5))
  (test 4.0  (ceiling 3.5))
  (test 3.0  (truncate 3.5))
  (test 4.0  (round 3.5))
  (test 2.0  (round 2.5))
  (test -4.0 (round -3.5))
  (test -2.0 (round -2.5))
  (test 4.0  (round 7/2)) ;; Rationals not supported, so result is inexact
  (test 7    (round 7))

  (test 3.0 (numerator (/ 6 4))) ;; Inexact because we don't support rationals yet
  (test 2.0 (denominator (/ 6 4))) ;; Inexact because we don't support rationals yet
  (test 2.0 (denominator (inexact (/ 6 4))))
)

(test-group
  "sqrt"
  (test 1i (sqrt -1))
  (test 1i (sqrt -1.0))
  (test +i (sqrt -1.0))
  (test 2 (sqrt 4))
  (test 2.0 (sqrt 4.0))
  (test 2i (sqrt -4.0))
  (test #t (complex? (sqrt -1)))
  (test #t (complex? (sqrt -i)))
)

(test-group
  "exact"
  (test -1 (exact -1))
  (test -1 (exact -1.0))
  (test -1 (exact -1.1))
  (test -1 (exact -1.1))
  (test 1.0+1.0i (exact 1.1+1.2i))
  ;(test #t (bignum? (exact 111111111111111111111111111.0)))
  ;(test #t (bignum? (exact -111111111111111111111111111.0)))
  ;(test +inf.0 (exact +inf.0))
)

(test-group
  "records"
  (define-record-type employee
     (make-employee name title)
     employee?
     (name get-name)
     (title get-title)
     (test get-test set-test!)) ;; Uninitialized by constructor
  (define e (make-employee "test-name" "job 1"))

  (test #f (get-test e))
  (set-test! e 'test-field)
  (test 'test-field (get-test e))
)

(test-group
  "assoc"
  (define a 0.0)
  (test '(0.0) (assoc a (list (list a))))
  (test '(0.0) (assoc 0.0 (list (list a))))
  (test '(0.0) (assv a (list (list a))))
  (test '(0.0) (assv 0.0 (list (list a))))
  (test '(0.0) (assq a (list (list a))))
  (test #f (assq 0.0 (list (list a))))
)

(test-group
  "member"
  (define m 0.0)
  (test '(0.0) (member m (list m)))
  (test '(0.0) (member 0.0 (list m)))
  (test '(0.0) (memv m (list m)))
  (test '(0.0) (memv 0.0 (list m)))
  (test '(0.0) (memq m (list m)))
  (test #f (memq 0.0 (list m)))
)

(test-group
  "exception handling"
  (define (capture-output thunk)
    (let ((output-string (open-output-string)))
      (parameterize ((current-output-port output-string))
        (thunk))
      (let ((result (get-output-string output-string)))
       (close-output-port output-string)
       result)))
  (test
    "should be a number65"
    (capture-output
      (lambda ()
        (with-exception-handler
          (lambda (con)
            (cond
              ((string? con)
               (display con))
              (else
               (display "a warning has been issued")))
            42)
          (lambda ()
            (display
              (+ (raise-continuable "should be a number")
                 23)))))))
  (test
    "condition: an-error"
    (capture-output
      (lambda ()
        (call-with-current-continuation
          (lambda (k)
            (with-exception-handler
              (lambda (x)
                (display "condition: ")
                (write x)
                (k "exception"))
              (lambda ()
                (+ 1 (raise 'an-error)))))))))
)

(test-exit)


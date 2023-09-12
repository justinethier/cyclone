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

(test-group
  "rationals"
  (test 3.0 (numerator (/ 6 4)))
  (test 2.0 (denominator (/ 6 4)))
)

(test-group
  "truncate"
  (test -1 (truncate -1))
  (test -1.0 (truncate -1.0))
  (test -1.0 (truncate -1.1))
  (test -1.0 (truncate -1.1))
  (test +inf.0 (truncate +inf.0))
)

(test-group
  "exact"
  (test -1 (exact -1))
  (test -1 (exact -1.0))
  (test -1 (exact -1.1))
  (test -1 (exact -1.1))
  (test #t (bignum? (exact 111111111111111111111111111.0)))
  (test #t (bignum? (exact -111111111111111111111111111.0)))
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

(test-exit)


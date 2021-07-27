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

(test-exit)


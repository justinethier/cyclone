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
  (srfi 18)
  (cyclone test))


(test-group
  "thread-join!"
  (let ((t (thread-start! (make-thread (lambda () (expt 2 100))))))
    (test (expt 2 100) (thread-join! t)))
)

(test-exit)

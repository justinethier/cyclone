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


(test-group
  "make-list"
  (test '() (make-list -2))
)

(test-exit)


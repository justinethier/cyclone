;;;; Cyclone Scheme
;;;; https://github.com/justinethier/cyclone
;;;;
;;;; Copyright (c) 2014-2021, Justin Ethier
;;;; All rights reserved.
;;;;
;;;; This module contains unit tests for (cyclone test)
;;;;

(import 
  (scheme base)
  (cyclone test))

(test-group
  "assert"
  (test-assert #t)
)

(test-group
  "not"
  (test-not #f)
)

(test-exit)



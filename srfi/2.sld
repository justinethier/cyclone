;;;; Cyclone Scheme
;;;; https://github.com/justinethier/cyclone
;;;;
;;;; Copyright (c) 2014-2016, Justin Ethier
;;;; All rights reserved.
;;;;
;;;; This module is an interface SRFI-2 and-let*
;;;;
(define-library (srfi 2)
  (import (scheme base))
  (export and-let*)
  (include "2.scm"))

;;;; Cyclone Scheme
;;;; https://github.com/justinethier/cyclone
;;;;
;;;; Copyright (c) 2017, Koz Ross
;;;;
;;;; This module is an interface to the Basic Format Strings library.
(define-library
  (srfi 28)
  (import 
    (scheme base)
    (scheme write))
  (export format)
  (include "28.scm"))


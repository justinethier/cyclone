;;;; Cyclone Scheme
;;;; https://github.com/justinethier/cyclone
;;;; 
;;;; Copyright (c) 2014-2017, Justin Ethier
;;;; Copyright (c) 2017, Koz Ross
;;;;
;;;; This module is an interface to the Generators library.
(define-library 
  (srfi 121)
  (import 
    (scheme base)
    (scheme case-lambda))
  (export 
    generator make-iota-generator make-range-generator 
    make-coroutine-generator list->generator vector->generator
    reverse-vector->generator string->generator
    bytevector->generator
    make-for-each-generator make-unfold-generator)
  (export 
    gcons* gappend gcombine gfilter gremove 
    gtake gdrop gtake-while gdrop-while
    gdelete gdelete-neighbor-dups gindex gselect)
  (export 
    generator->list generator->reverse-list
    generator->vector generator->vector!  generator->string
    generator-fold generator-for-each generator-find
    generator-count generator-any generator-every generator-unfold)
  (include "121.scm"))

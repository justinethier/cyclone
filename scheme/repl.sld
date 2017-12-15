;;;; Cyclone Scheme
;;;; https://github.com/justinethier/cyclone
;;;;
;;;; Copyright (c) 2014-2016, Justin Ethier
;;;; All rights reserved.
;;;;
;;;; This module contains the repl library from r7rs.
;;;;
(define-library (scheme repl)
  (export 
    interaction-environment
  )
  (import (scheme eval))
  (begin
    (define (interaction-environment)
      (setup-environment))
  ))

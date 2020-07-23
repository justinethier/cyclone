;;;; Cyclone Scheme
;;;; https://github.com/justinethier/cyclone
;;;;
;;;; Copyright (c) 2014-2016, Justin Ethier
;;;; All rights reserved.
;;;;
;;;; This module contains the load library from r7rs.
;;;;
(define-library (scheme load)
  (export 
    load)
  (import (scheme base) 
          (scheme eval) 
          (scheme file)
          (scheme read))
  (begin
    (define (load filename . env)
      (let ((exprs (call-with-input-file filename
                     (lambda (port)
                       (read-all/source port filename)))))
        (for-each
          (lambda (expr)
            (apply eval (cons expr env)))
          exprs)))))

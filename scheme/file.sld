;;;; Cyclone Scheme
;;;; https://github.com/justinethier/cyclone
;;;;
;;;; Copyright (c) 2014-2016, Justin Ethier
;;;; All rights reserved.
;;;;
;;;; This module contains the file library from r7rs.
;;;;
(define-library (scheme file)
  (export
    call-with-input-file 
    call-with-output-file
    delete-file 
    file-exists?
    ;open-binary-input-file 
    ;open-binary-output-file
    ;open-input-file 
    ;open-output-file
    with-input-from-file 
    with-output-to-file
  )
  (import (scheme base))
  (inline
    file-exists?
    delete-file)
  (begin
    (define-c file-exists?
      "(void *data, int argc, closure _, object k, object filename)"
      " return_closcall1(data, k, Cyc_io_file_exists(data, filename));"
      "(void *data, object ptr, object filename)"
      " return Cyc_io_file_exists(data, filename);")
    (define-c delete-file
      "(void *data, int argc, closure _, object k, object filename)"
      " return_closcall1(data, k, Cyc_io_delete_file(data, filename));"
      "(void *data, object ptr, object filename)"
      " return Cyc_io_delete_file(data, filename);")
    (define (call-with-input-file string proc)
      (call-with-port (open-input-file string) proc))
    (define (call-with-output-file string proc)
      (call-with-port (open-output-file string) proc))
    (define (with-input-from-file string thunk)
      ;; Have to do this the long way since parameterize is not available
      (let ((old (current-input-port))
            (new ((current-input-port '<param-convert>) (open-input-file string))))
        (dynamic-wind
          (lambda () (current-input-port '<param-set!> new))
          thunk
          (lambda () 
            (close-port (current-input-port))
            (current-input-port '<param-set!> old)))))
    (define (with-output-to-file string thunk)
      ;; Have to do this the long way since parameterize is not available
      (let ((old (current-output-port))
            (new ((current-output-port '<param-convert>) (open-output-file string))))
        (dynamic-wind
          (lambda () (current-output-port '<param-set!> new))
          thunk
          (lambda () 
            (close-port (current-output-port))
            (current-output-port '<param-set!> old)))))
))

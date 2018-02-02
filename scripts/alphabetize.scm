;;;; Cyclone Scheme
;;;; https://github.com/justinethier/cyclone
;;;;
;;;; Copyright (c) 2014-2016, Justin Ethier
;;;; All rights reserved.
;;;;
;;;; A simple program to format links for the API index page.
;;;;
(import 
  (scheme base) 
  (scheme write)
  (scheme cyclone util))


;(display (convert-line line))
(define last-alpha #f)
(define (loop)
 (let ((line (read-line)))
  (when (not (eof-object? line))
    (when (> (string-length line) 1)
      (when (not (eq? last-alpha (string-ref line 0)))
        (set! last-alpha (string-ref line 0))
        (newline)
        (display "- - -")
        (newline))
      (display (substring line 1 (string-length line)))
      (newline)
    )
    (loop))))
(loop)

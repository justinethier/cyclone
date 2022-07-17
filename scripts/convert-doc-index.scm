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

;; TODO: move this somewhere useful
(define (index-of lst x)
 (define (find lst idx)
   (cond 
     ((null? lst) #f)
     ((eq? (car lst) x) idx)
     (else (find (cdr lst) (+ idx 1)))))
 (find lst 0))

;; goal is:
;; [`load`](api/scheme/load.md#load)
;(define line "docs/api/scheme/base.md:- [`denominator`](#denominator)")
;(define line "docs/api/scheme/process-context.md:- [`get-environment-variables`](#get-environment-variables)")
;(define line "docs/api/scheme/char.md:- [`string-ci>=?`](#string-ci-3)")

(define (convert-line line)
  (let* ((lis (string->list line))
         (s-file 5)
         (e-file (index-of lis #\:))
         (file (substring line s-file e-file))
         (s-fnc (+ 1 (index-of lis #\`)))
         (e-fnc (+ s-fnc (index-of (string->list (substring line (+ 0 s-fnc) (string-length line))) #\`)))
         (fnc (substring line s-fnc e-fnc))
         (s-link (index-of lis #\#))
         (e-link (string-length line))
         (link (substring line s-link e-link))
         )
   (string-append 
     (string (string-ref fnc 0))
     "[`" fnc "`](" file "" link "")))

;(display (convert-line line))
(define (loop)
 (let ((line (read-line)))
  (when (not (eof-object? line))
    (with-handler
      (lambda (obj)
        (display `(Error processing line ,line details ,obj) (current-error-port)))
      (display (convert-line line))
      (newline))
        
    #;(call/cc
      (lambda (k)
        (with-exception-handler
          (lambda (obj)
            (display `(Error processing line ,line details ,obj))
            (k #t))
          (lambda ()
            (display (convert-line line))
            (newline)))))
    (loop))))
(loop)

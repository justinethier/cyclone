;;;; Cyclone Scheme
;;;; https://github.com/justinethier/cyclone
;;;;
;;;; Copyright (c) 2014-2019, Justin Ethier
;;;; All rights reserved.
;;;;
;;;; TBD
;;;;
(define-library (cyclone foreign)
 (import
   (scheme base)
   ;(scheme write) ;; TODO: debugging only!
 )
 ;(include-c-header "<ck_pr.h>")
 (export
  ;; TODO
 )
 (begin
  ;; TODO: internal to compiler? Anything to define in this library??
  ;; internal name could be different (Cyc-foreign-code) to facilitate
  ;; library renaming, etc here
  ;(foreign-code STRING ...)


;; TODO: how to handle this?

;could maybe have a macro (define-c-foreign) that takes below and rewrites it as a define-c
;would be nice if we could have foreign-lambda though, which seems much more flexible.
;maybe we can work up to that

  ;(define strlen
  ;  (foreign-lambda int "strlen" char-vector) )

;    (define-syntax define-curl-const
;      (er-macro-transformer
;        (lambda (expr rename compare)
;          (let* ((sym (cadr expr))
;                 (str (symbol->string sym))
;                 (lib_fnc_str (string-append "_" str))
;                 (lib_fnc (string->symbol lib_fnc_str)) ;; Internal library function
;                 (args "(void *data, int argc, closure _, object k)")
;                 (body
;                   (string-append
;                     "return_closcall1(data, k, obj_int2obj(" str "));"))
;                )
;          `(begin
;            (define-c ,lib_fnc ,args ,body)
;            (define ,sym (,lib_fnc))
;            )))))


 )
)

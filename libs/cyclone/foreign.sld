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
   foreign-code
   foreign-value
 )
 (begin
  ;; TODO: internal to compiler? Anything to define in this library??
  ;; internal name could be different (Cyc-foreign-code) to facilitate
  ;; library renaming, etc here
  ;(foreign-code STRING ...)


;; TODO: foreign-lambda
;;
;; We are going to use the CHICKEN interface: 
;;  (foreign-lambda RETURNTYPE NAME ARGTYPE ...)
;;
;; And modify it a bit for our infrastructure:
;;
;;  (define-foreign-lambda SCM-NAME RETURNTYPE C-NAME ARGTYPE ...)
;;
;; We need to develop a macro to accept this interface and generate a
;; define-c equivalent. Not nearly as flexible as CHICKEN but will work
;; with our existing infrastructure. This is good enough for version 1.


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


  (define-syntax foreign-value
    (er-macro-transformer
      (lambda (expr rename compare)
        (let* ((code-arg (cadr expr))
               (type-arg (caddr expr))
              )
          ;(for-each
          ;  (lambda (arg)
          ;    (if (not (string? arg))
          ;        (error "foreign-value" "Invalid argument: string expected, received " arg)))
          ;  (cdr expr))
          `(Cyc-foreign-value ,code-arg ,type-arg)))))

  (define-syntax foreign-code
    (er-macro-transformer
      (lambda (expr rename compare)
        (for-each
          (lambda (arg)
            (if (not (string? arg))
                (error "foreign-code" "Invalid argument: string expected, received " arg)))
          (cdr expr))
        `(Cyc-foreign-code ,@(cdr expr)))))
 )
)

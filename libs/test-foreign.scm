(import 
  (scheme base) 
  (scheme write)
  (cyclone test)
  (cyclone foreign)
  (scheme cyclone util)
  (scheme cyclone pretty-print))

;(define-syntax foreign-code
;  (er-macro-transformer
;    (lambda (expr rename compare)
;      (for-each
;        (lambda (arg)
;          (if (not (string? arg))
;              (error "foreign-code" "Invalid argument: string expected, received " arg)))
;        (cdr expr))
;      `(Cyc-foreign-code ,@(cdr expr)))))
;
;;; Unbox scheme object
;;;
;;; scm->c :: string -> symbol -> string
;;;
;;; Inputs:
;;;  - code - C variable used to reference the Scheme object
;;;  - type - Data type of the Scheme object
;;; Returns:
;;;  - C code used to unbox the data
;;(define (scm->c code type)
;(define-syntax scm->c
;  (er-macro-transformer
;    (lambda (expr rename compare)
;      (let ((code (cadr expr))
;            (type (caddr expr)))
;       `(case ,type
;          ((int integer)
;           (string-append "obj_obj2int(" ,code ")"))
;          ((bool)
;           (string-append "(" ,code " == boolean_f)"))
;          ((string)
;           (string-append "string_str(" ,code ")"))
;          (else
;            (error "scm->c unable to convert scheme object of type " ,type)))))))
;
;;; Box C object, basically the meat of (foreign-value)
;;;
;;; c->scm :: string -> symbol -> string
;;;
;;; Inputs:
;;;  - C expression
;;;  - Data type used to box the data
;;; Returns:
;;;  - Allocation code?
;;;  - C code
;(define-syntax c->scm
; (er-macro-transformer
;  (lambda (expr rename compare)
;    (let ((code (cadr expr))
;          (type (caddr expr)))
;     `(case ,type
;        ((int integer)
;         (cons
;           ""
;           (string-append "obj_int2obj(" ,code ")")))
;        ((float double)
;         (let ((var (mangle (gensym 'var))))
;         (cons
;           (string-append 
;             "make_double(" var ", " ,code ");")
;           (string-append "&" var)
;         )))
;        ((bool)
;         (cons
;           ""
;           (string-append "(" ,code " == 0 ? boolean_f : boolean_t)")))
;    ;    ((string)
;    ;    TODO: how to handle the allocation here?
;    ;          may need to return a c-code pair???
;    ;     (string-append "
;    ;     ))
;        (else
;          (error "c->scm unable to convert C object of type " ,type)))))))
;
;;(pretty-print (
;(define-syntax define-foreign-lambda
;  (er-macro-transformer
;    (lambda (expr rename compare)
;      (let* ((scm-fnc (cadr expr))
;             (c-fnc (cadddr expr))
;             (rv-type (caddr expr))
;             (arg-types (cddddr expr))
;             (arg-syms/unbox 
;               (map 
;                 (lambda (type)
;                   (let ((var (mangle (gensym 'arg))))
;                     (cons 
;                       var 
;                       (scm->c var type)
;                       ;(string-append "string_str(" var ")")
;                       )))
;                 arg-types))
;             (returns
;               (c->scm 
;                 (string-append 
;                   c-fnc "(" (string-join (map cdr arg-syms/unbox) ",") ")")
;                 rv-type))
;             (return-alloc (car returns))
;             (return-expr (cdr returns))
;             (args (string-append
;                     "(void *data, int argc, closure _, object k " 
;                     (apply string-append 
;                       (map
;                         (lambda (sym/unbox)
;                           (string-append ", object " (car sym/unbox)))
;                       arg-syms/unbox))
;                      ")"))
;             (body
;             ;; TODO: need to unbox all args, pass to C function, then box up the result
;               (string-append
;                 return-alloc
;                 "return_closcall1(data, k, " return-expr ");"))
;            )
;      `(define-c ,scm-fnc ,args ,body)
;        ))
; '(define-foreign-lambda scm-strlen int "strlen" string)
; list
; list
;)
;)
;
;
;;(define-c foreign-value
;; "(void *data, int argc, closure _, object k, object code, object type)"
;; " // TODO: need to dispatch conversion based on type
;;   return_closcall1(data, k, obj_int2obj(code
;; ")
;
;(define-foreign-lambda scm-strlen int "strlen" string)

;(write (Cyc-foreign-value "errno" "3"))
;(newline)
(test-group "foreign-value"
  (test 3 (Cyc-foreign-value "1 + 2" 'integer))
)

(test-group "foreign-code"
(write (foreign-code 
         "printf(\"test %d %d \\n\", 1, 2);"
         "printf(\"test %d %d %d\\n\", 1, 2, 3);")) (newline)
)

;; Must be top-level
(define-foreign-lambda scm-strlen int "strlen" string)
(define-foreign-lambda scm-strlend double "strlen" string)

(test-group "foreign lambda"
  (test 15 (scm-strlen "testing 1, 2, 3"))
  (test 15.0 (scm-strlend "testing 1, 2, 3"))
)
(test-exit)

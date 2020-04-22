(import 
  (scheme base) 
  (scheme write)
  (cyclone test)
  (scheme cyclone cgen)
  (scheme cyclone util)
  (scheme cyclone pretty-print))

(define-syntax foreign-code
  (er-macro-transformer
    (lambda (expr rename compare)
      (for-each
        (lambda (arg)
          (if (not (string? arg))
              (error "foreign-code" "Invalid argument: string expected, received " arg)))
        (cdr expr))
      `(Cyc-foreign-code ,@(cdr expr)))))

;(pretty-print (
(define-syntax define-foreign-lambda
  (er-macro-transformer
    (lambda (expr rename compare)

;; Temporary definition, this does not stay here!
(define (scm->c code type)
  (case type
    ((int integer)
     (string-append "obj_obj2int(" code ")"))
    ((bool)
     (string-append "(" code " == boolean_f)"))
    ((string)
     (string-append "string_str(" code ")"))
    (else
      (error "scm->c unable to convert scheme object of type " type))))

      (let* ((scm-fnc (cadr expr))
             (c-fnc (cadddr expr))
             (rv-type (caddr expr))
             (arg-types (cddddr expr))
             (arg-syms/unbox 
               (map 
                 (lambda (type)
                   (let ((var (mangle (gensym 'arg))))
                     (cons 
                       var 
                       (scm->c var type)
                       ;(string-append "string_str(" var ")")
                       )))
                 arg-types))
             ;(arg-strings 
             ;  (map
             ;    (lambda (sym)
             ;      (string-append " object " sym) 
             ;    )
             ;    arg-syms))

           ; TODO: append mangled args to other args
           ;  cyclone> (string-join '("a" "b" "c") ",")
           ;  "a,b,c"

             (args (string-append
                     "(void *data, int argc, closure _, object k " 
                     (apply string-append 
                       (map
                         (lambda (sym/unbox)
                           (string-append ", object " (car sym/unbox)))
                       arg-syms/unbox))
                      ")"))
             (body
             ;; TODO: need to unbox all args, pass to C function, then box up the result
               (string-append
                 "return_closcall1(data, k, obj_int2obj(" c-fnc "(" (string-join (map cdr arg-syms/unbox) ",") ")));"))
            )
      `(define-c ,scm-fnc ,args ,body)
        ))
 '(define-foreign-lambda scm-strlen int "strlen" string)
 list
 list
)
)

;; Unbox scheme object
(define (scm->c code type)
  (case type
    ((int integer)
     (string-append "obj_obj2int(" code ")"))
    ((bool)
     (string-append "(" code " == boolean_f)"))
    ((string)
     (string-append "string_str(" code ")"))
    (else
      (error "scm->c unable to convert scheme object of type " type))))

;; Box C object, basically the meat of (foreign-value)
(define (c->scm code type)
  (case type
    ((int integer)
     (string-append "obj_int2obj(" code ")"))
    ((bool)
     (string-append "(" code " == 0 ? boolean_f : boolean_t)"))
;    ((string)
;    TODO: how to handle the allocation here?
;          may need to return a c-code pair???
;     (string-append "
;     ))
    (else
      (error "c->scm unable to convert C object of type " type))))


;(define-c foreign-value
; "(void *data, int argc, closure _, object k, object code, object type)"
; " // TODO: need to dispatch conversion based on type
;   return_closcall1(data, k, obj_int2obj(code
; ")

;(define-foreign-lambda scm-strlen int "strlen" string)

;(write (Cyc-foreign-value "errno" "3"))
;(newline)
(test-group "basic"
(write (foreign-code 
         "printf(\"test %d %d \\n\", 1, 2);"
         "printf(\"test %d %d %d\\n\", 1, 2, 3);")) (newline)
)

;; Must be top-level
(define-foreign-lambda scm-strlen int "strlen" string)

(test-group "foreign lambda"
  (test 15 (scm-strlen "testing 1, 2, 3"))
)
(test-exit)

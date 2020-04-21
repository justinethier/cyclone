(import 
  (scheme base) 
  (scheme write)
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

;(pretty-print
;(
(define-syntax define-foreign-code
  (er-macro-transformer
    (lambda (expr rename compare)
      (let* ((scm-fnc (cadr expr))
             (c-fnc (cadddr expr))
             (rv-type (caddr expr))
             (arg-types (cddddr expr))
             (arg-syms/unbox 
               (map 
                 (lambda (type)
                   (let ((var (mangle (gensym 'arg))))
                     (cons var (string-append "string_str(" var ")"))))
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
                           (string-append "," (car sym/unbox)))
                       arg-syms/unbox))
                      ")"))
             (body
             ;; TODO: need to unbox all args, pass to C function, then box up the result
               (string-append
                 "return_closcall1(data, k, obj_int2obj(" c-fnc "(" (string-join (map cdr arg-syms/unbox) ",") ")));"))
            )
      `((define-c ,scm-fnc ,args ,body)
        )))
; '(define-foreign-lambda scm-strlen int "strlen" string)
; list
; list)
)
(define-foreign-lambda scm-strlen int "strlen" string)
(display (scm-strlen "testing 1, 2, 3"))
(newline)

;(define-c foreign-value
; "(void *data, int argc, closure _, object k, object code, object type)"
; " // TODO: need to dispatch conversion based on type
;   return_closcall1(data, k, obj_int2obj(code
; ")

;(define-foreign-lambda scm-strlen int "strlen" string)

;(write (Cyc-foreign-value "errno" "3"))
;(newline)
(write (foreign-code 
         "printf(\"test %d %d \\n\", 1, 2);"
         "printf(\"test %d %d %d\\n\", 1, 2, 3);"))
(newline)

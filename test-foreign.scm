(import (scheme base) (scheme write))

(define-syntax foreign-code
  (er-macro-transformer
    (lambda (expr rename compare)
      (for-each
        (lambda (arg)
          (if (not (string? arg))
              (error "foreign-code" "Invalid argument: string expected, received " arg)))
        (cdr expr))
      `(Cyc-foreign-code ,@(cdr expr)))))

(define-syntax define-foreign-code
  (er-macro-transformer
    (lambda (expr rename compare)
      (let* ((scm-fnc (cadr expr))
             (c-fnc (cadddr expr))
             (rv-type (caddr expr))
             (arg-types (cddddr expr))
;             (str (symbol->string sym))
;             (lib_fnc_str (string-append "_" str))
;             (lib_fnc (string->symbol lib_fnc_str)) ;; Internal library function
;             (args "(void *data, int argc, closure _, object k)")
;             (body
;               (string-append
;                 "return_closcall1(data, k, obj_int2obj(" str "));"))
            )
      `((define-c ,lib_fnc ,args ,body)
        )))))

;(define-c foreign-value
; "(void *data, int argc, closure _, object k, object code, object type)"
; " // TODO: need to dispatch conversion based on type
;   return_closcall1(data, k, obj_int2obj(code
; ")

(define-foreign-lambda scm-strlen int "strlen" string)

;(write (Cyc-foreign-value "errno" "3"))
;(newline)
(write (foreign-code 
         "printf(\"test %d %d \\n\", 1, 2);"
         "printf(\"test %d %d %d\\n\", 1, 2, 3);"))
(newline)

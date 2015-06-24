;(import (scheme base)
;        (scheme file)
;        (scheme write))

; TODO: I think this compiles OK (test), but interpreter does not like it:
;cyclone> ( call-with-output-file "test.txt" (lambda () #f))
;Error: Unable to evaluate:  ((procedure () ...) <port>)

; TODO: need to get this working in compiler, then try interpreter:
;(with-output-to-file 
;  "test.out" 
;  (lambda ()
;    (write 'hello)
;    (display " ")
;    (display 'world)))

; BEGIN test code - trying to get definition of with-output-to-file to work
    (define (my-make-parameter init . o)
      (let* ((converter
               (if (pair? o) (car o) (lambda (x) x)))
             (value (converter init)))
        (lambda args
          (cond
            ((null? args)
             value)
            ((eq? (car args) '<param-set!>)
             (set! value (cadr args)))
            ((eq? (car args) '<param-convert>)
             converter)
           (else
             (error "bad parameter syntax"))))))
    (define my-param
      (my-make-parameter (current-output-port)));(Cyc-stdout)))
(define old (my-param))
(define new (my-param '<param-convert> (open-output-file "test.txt")))
; The next line seems to crash in icyc but not in compiled code (until write, at least). what's going on??
(my-param '<param-set!> new)
(write 'test (my-param))
;(write 'hello-world)
; END test code

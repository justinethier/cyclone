(import (scheme base)
        (scheme file)
        (scheme write))

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
(define old (current-output-port))
(define new (current-output-port '<param-convert> (open-output-file "test.txt")))
(current-output-port '<param-set!> new)
(write 'test (current-output-port))
;(write 'hello-world)
; END test code

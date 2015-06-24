(import (scheme base)
        (scheme file)
        (scheme write))

; TODO: I think this compiles OK (test), but interpreter does not like it:
;cyclone> ( call-with-output-file "test.txt" (lambda () #f))
;Error: Unable to evaluate:  ((procedure () ...) <port>)

; TODO: need to get this working in compiler, then try interpreter:
(with-output-to-file 
  "test.out" 
  (lambda ()
    (write 'hello)
    (display " ")
    (display 'world)))

(import (scheme base)
        (scheme file)
        (scheme write))

; TODO: I think this compiles OK (test), but interpreter does not like it:
;cyclone> 
(call-with-output-file "test.txt" (lambda (port) (write 'ok port)))
;Error: Unable to evaluate:  ((procedure () ...) <port>)



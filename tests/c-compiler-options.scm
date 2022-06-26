(import (scheme base)
        (scheme write)
        (cyclone foreign))

;(c-linker-options "-I/tmp")
(c-compiler-options "-I/tmp")
;(define c-compiler-options list)
;(c-linker-options "-I/tmp")

(display "hello")


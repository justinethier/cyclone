;; Simple test to prevent regressions of top-level c-compiler-options 
(import (scheme base)
        (scheme write)
        (cyclone foreign))

(c-compiler-options "-I/tmp")

(display "hello")


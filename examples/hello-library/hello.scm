        ; TODO: just adding temporarily until import is supported.
        ; idea is to try and see how the C code needs to change to
(import (scheme base)
        (scheme write)
        (libs lib1)
        ;(rename (prefix (libs lib1) test-))
        )

(write "hello")
;(test-lib1-hello)
(write (lib1-hello))
(write "world")

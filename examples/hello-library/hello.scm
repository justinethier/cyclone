        ; TODO: just adding temporarily until import is supported.
        ; idea is to try and see how the C code needs to change to
(import (scheme base)
        (scheme write)
        (libs lib1)
        ;(rename (prefix (libs lib1) test-))
        )

(write "hello")
(newline)

(write lib1-test-renamed)
(newline)

(write (lib1-hello))
(newline)

(write "world")
(newline)

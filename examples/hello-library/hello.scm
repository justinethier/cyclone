        ; TODO: just adding temporarily until import is supported.
        ; idea is to try and see how the C code needs to change to
(import (scheme base)
        (libs lib1)
        ;(rename (prefix (libs lib1) test-))
        )

;(test-lib1-hello)
(write lib1-hello)

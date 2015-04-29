        ; TODO: just adding temporarily until import is supported.
        ; idea is to try and see how the C code needs to change to
        ; support libraries
        (define lib2-hello 
            "Hello from library #2")
;(import ;(scheme base)
;        (libs lib2)
;        ;(rename (prefix (libs lib1) test-))
;        )
;
;(test-lib1-hello)
(write lib2-hello)

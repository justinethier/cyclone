;;;
;;; Justin Ethier
;;; husk scheme
;;;
;;; A sample library
;;;
(define-library (libs lib1)
    (export lib1-hello)
    (import (scheme base)
            (scheme write)
            (libs lib2))
    (begin
        (define (internal-func)
            (write lib2-hello))
        (define (lib1-hello)
            (internal-func))))

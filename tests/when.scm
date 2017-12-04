(import (scheme base) (scheme write))
(define-syntax when
  (syntax-rules ()
    ((when test result1 result2 ...)
     (if test
         (begin result1 result2 ...)))))

(write
  (when #t 1))

(import (scheme base) (scheme write))
(define-syntax my-when
  (syntax-rules ()
    ((my-when test result1 result2 ...)
     (if test
         (begin result1 result2 ...)))))

(write
  (my-when #t 1))

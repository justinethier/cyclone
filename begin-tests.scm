;; A temporary test file
(import (scheme base ) (scheme write))

;(
;(lambda ()
  (begin
    (define tmp1 1)
    (define tmp2 2))
  
  (write `(,tmp1 ,tmp2))
;))

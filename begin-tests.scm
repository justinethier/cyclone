;; A temporary test file
(import (scheme base ) (scheme write))

;; TODO: begin splicing does not work at top-level
((lambda ()
  (begin
    (define tmp1 1)
    (define tmp2 2))
  
  (write `(,tmp1 ,tmp2))
))

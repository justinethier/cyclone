;; An example of using begin to splice in definitions
(import (scheme base ) 
        (scheme write))

(begin
  (define tmp1 1)
  (define tmp2 2))

(write `(,tmp1 ,tmp2))


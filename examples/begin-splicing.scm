;; An example of using begin to splice in definitions. As the 
;; spec states:
;;
;; "It causes the contained expressions and definitions to be
;;  evaluated exactly as if the enclosing begin construct were
;;  not present."
(import (scheme base ) 
        (scheme write))

(begin
  (define tmp1 1)
  (define tmp2 2))

(write `(,tmp1 ,tmp2))


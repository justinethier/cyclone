(import (scheme base)
        (scheme read))
'aa
(define (quoted? exp)
  (tagged-list? exp 'quote))

(write (read))


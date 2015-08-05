(import (scheme base)
        (scheme write))

(define-syntax test
  (er-macro-transformer
    (lambda (expr rename compare)
      (write "testing")
      (write expr))))

(test 1 2 3)
(test 'done)

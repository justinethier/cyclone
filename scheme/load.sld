(define-library (scheme load)
  (export load)
  (import (scheme base) 
          (scheme eval) 
          (scheme file)
          (scheme read))
  (begin
    (define (load filename)
      (let ((exprs (call-with-input-file filename
                     (lambda (port)
                       (read-all port)))))
        (for-each
          (lambda (expr)
            (eval expr))
          exprs)))))

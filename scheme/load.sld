(define-library (scheme load)
  (export 
    load)
  (import (scheme base) 
          (scheme eval) 
          (scheme file)
          (scheme read))
  (begin
    (define (load filename . env)
      (let ((exprs (call-with-input-file filename
                     (lambda (port)
                       (read-all port)))))
        (for-each
          (lambda (expr)
            (apply eval (cons expr env)))
          exprs)))))

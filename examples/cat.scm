(import (scheme base) 
        (scheme file) 
        (scheme write))

(define (cat fname)
  (call-with-input-file
    fname
    (lambda (fp)
      (define (loop)
        (let ((s (read-line fp)))
          (cond
           ((not (eof-object? s))
            (display s)
            ;(newline)
            (loop)))))
      (loop))))

(cat
  "cyclone.scm"
  ;"scheme/base.sld"
)

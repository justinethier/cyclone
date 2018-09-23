;; A temporary test file
(import (scheme base) (scheme write))

(define (analyze . opts)
  (write 'test))

(define (analyze-if exp a-env rename-env local-renamed)
  (let ((pproc (analyze (if-predicate exp) a-env rename-env local-renamed))
        (cproc (analyze (if-consequent exp) a-env rename-env local-renamed))
        (aproc (analyze (if-alternative exp) a-env rename-env local-renamed)))
    (lambda (env)
      (if (pproc env)
          (cproc env)
          (aproc env)))))
(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))
(define (if-alternative exp)
  (if (not (null? (cdddr exp))) ;; TODO: add (not) support
      (cadddr exp)
      #f))

(write (analyze-if 'a 'b 'c 'd))

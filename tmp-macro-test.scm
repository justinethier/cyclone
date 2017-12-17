;; This is a temporary test file
(import (scheme base) (scheme write) (scheme eval) (scheme cyclone util))

(define env (env:extend-environment '() '() env:the-empty-environment))
(define env2 (env:extend-environment '(a b c) '(1 2 3) env))

(define a-env (create-environment '() '()))

(let* (
(vars (foldl (lambda (lis acc) (append acc (car lis))) '() a-env))
(vals (foldl (lambda (lis acc) (append acc (cdr lis))) '() a-env))
(zipped (apply map list vars (list vals)))
(defined-macros
  (filter
    (lambda (v)
      (Cyc-macro? (Cyc-get-cvar (cadr v))))
    zipped))
(macro-env 
  (env:extend-environment
    (map car defined-macros)
    (map (lambda (v)
           (list 'macro (cadr v)))
         defined-macros)
    '())) ;; base-env
)
  (write (list vars vals zipped defined-macros macro-env)))

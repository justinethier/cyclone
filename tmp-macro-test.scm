;; This is a temporary test file
(import (scheme base) (scheme write) (scheme eval) (scheme cyclone util))

(define a-env (create-environment '() '()))

(let* (
(vars (foldl (lambda (lis acc) (append acc (car lis))) '() a-env))
(vals (foldl (lambda (lis acc) (append acc (cdr lis))) '() a-env))
(zipped (apply map list vars (list vals)))
(defined-macros
  (filter
    (lambda (v)
      (Cyc-macro? (Cyc-get-cvar (cdr v))))
    zipped))
(macro-env 
  (env:extend-environment
    (map car defined-macros)
    (map (lambda (v)
           (list 'macro (cdr v)))
         defined-macros)
    '())) ;; base-env
)
  (write macro-env))

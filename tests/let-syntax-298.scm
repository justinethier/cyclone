;; From:
;; https://github.com/ashinn/chibi-scheme/issues/298
(import (scheme base))

(define-syntax bar
  (syntax-rules ()
    ((_)
     (let-syntax ((foo (syntax-rules () ((_) 'ok))))
       (foo)))))

(define-syntax foo (syntax-rules ()))

(bar)
(foo) 

;; From:
;; https://github.com/ashinn/chibi-scheme/issues/298
(import (scheme base) (scheme write))

(define-syntax bar
  (syntax-rules ()
    ((_)
     (let-syntax ((foo (syntax-rules () ((_) 'ok))))
       (foo)))))

(define-syntax foo (syntax-rules () ((_) 'foo)))

(write
(bar)
)
(write
(foo) 
)

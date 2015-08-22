;(import (scheme eval) (scheme write))
;
;(eval '((lambda (expr rename compare) (cond ((null? (cdr expr)) #t) ((null? (cddr expr)) (cadr expr)) (else (list (rename (quote if)) (cadr expr) (cons (rename (quote and)) (cddr expr)) #f)))) '(test 1 2 3) (lambda (x) x) '()))
;;;(eval '((lambda (x) x) 1))
;
;
(import (scheme base)
        (test-lib test)
        (scheme eval)
        (scheme write))

;(define-syntax test
;  (er-macro-transformer
;    (lambda (expr rename compare)
;      `((lambda ()
;        (write "testing")
;        (write (quote ,(cdr expr))))))))
;
; WTF is the macro unable to be evaluated when the same code works as part of *defined-macros*???
;
(define-syntax test 
  (er-macro-transformer
     (lambda (expr rename compare)
       (cond ((null? (cdr expr)) #t)
;       (cond ((null? (cdr expr)))
             ((null? (cddr expr)) (cadr expr))
             (else (list (rename 'if) (cadr expr)
                         (cons (rename 'and) (cddr expr))
                         #f))))))

(define-syntax test2
  (er-macro-transformer
     (lambda (expr rename compare)
       ;; need some way to get these in the eval env
       ;;
       ;; may need to maintain an environment in the compiler and pass it
       ;; along to eval somehow when macro is expanded. would this just
       ;; involve changes to expand? also, does that mean macro:expand
       ;; should call eval directly if a non-compiled macro is found?
       ;; if that is the case, macro:expand would also need to receive
       ;; the env parameter so it could pass that along to.
       ;; tbd how this parameter would be combined with eval's global env,
       ;; because it would need to extend it.
       ;; could eval expose a function to extend the global env (or any env)?
       (test 1 2 3) ; breaks
       ;(my-or 1 2 3) ; breaks
       (and ''test ''test2))))

(write (test2 1 2 3))
(write (test 1 2 3))
(write (my-or 1 2 3 'or))
(write (my-or #f 2 3 'or))
;(test 'done)
'done

(define x 1)
(write x)
(write
  (eval 'my-or))
(write
  (eval '(my-or 1 2 x)))

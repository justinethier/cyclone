;; A temporary test file
(import
  (scheme base)
  (scheme write))

;; TODO: can we prove that the define is being used like "let" and not "letrec"?
;; If so, can simplify things a bit, perhaps

;(define (test)
;  (define (fnc x y z) (write (list x y z)))
;    (fnc 1 2 3)
;    (fnc 1 2 3))
;"---------------- after wrap-mutables:"
; */
;/* 
;((define test
;   (lambda-1-cont
;     (k$12)
;     ((lambda-2
;        (fnc$5)
;        ((lambda-5
;           (fnc$5)
;           (Cyc-seq
;             (set-cell!
;               fnc$5
;               (lambda-3-cont
;                 (k$16 x$1$7 y$2$8 z$3$9)
;                 (write k$16 (Cyc-fast-list-3 x$1$7 y$2$8 z$3$9))))
;             ((cell-get fnc$5)
;              (lambda-4 (r$14) ((cell-get fnc$5) k$12 1 2 3))
;              1
;              2
;              3)))
;         (cell fnc$5)))
;      #f)))
; (test %halt))


;(define (test)
;  (letrec ((fnc (lambda (x y z) (write (list x y z)))))
;    (fnc 1 2 3)
;    (fnc 1 2 3))
;  )



(define (test)
  (let ((fnc (lambda (x y z) (write (list x y z)))))
    (fnc 1 2 3)
    (fnc 1 2 3))
  )
;"---------------- after macro expansion:"
; */
;/* 
;((define test
;   (lambda ()
;     ((lambda (fnc$1) (fnc$1 1 2 3) (fnc$1 1 2 3))
;      (lambda (x$2 y$3 z$4) (write (list x$2 y$3 z$4))))))
; (test))
;"---------------- after wrap-mutables:"
; */
;/* 
;((define test
;   (lambda-1-cont
;     (k$11)
;     ((lambda-2
;        (fnc$1$5)
;        (fnc$1$5
;          (lambda-3 (r$13) (fnc$1$5 k$11 1 2 3))
;          1
;          2
;          3))
;      (lambda-4-cont
;        (k$14 x$2$6 y$3$7 z$4$8)
;        (write k$14 (Cyc-fast-list-3 x$2$6 y$3$7 z$4$8))))))
; (test %halt))
(test)

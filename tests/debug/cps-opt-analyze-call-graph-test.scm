;; Some notes:
;; Need a new pass to identify all variables that contribute to each computed variable. Maybe write this as a separate program again, will need to flesh this out.
;; 
;; Need another pass to flag all vars that cannot be inlined (or just add this to current checks). I think if any contribution var is mutated or passed to a function outside the module being compiled, then the var cannot be inlined. It is probably possible to be a bit smarter?
;; 
;; Will have to assess how badly this hurts performance.
;;
(import 
  (scheme base)
  (scheme write)
)
;(inline
;  my-string<=?
;  my-not)
;
;    (define (my-string<=? str1 str2) (<= (string-cmp str1 str2) 0))
;    (define (my-not x) (if x #f #t))
;
;(define (queue-empty) (cons '() '()))
;(define objects-dumped (queue-empty))
;(define object-queue (queue-empty))
;(define (queue->list queue) (car queue))
;(define (queue-put! queue x)
;  (let ((entry (cons x '())))
;    (if (null? (car queue))
;        (set-car! queue entry)
;        (set-cdr! (cdr queue) entry))
;    (set-cdr! queue entry)
;    x))
;
;(define (pos-in-list x l)
;  (let loop ((l l) (i 0))
;    (cond ((not (pair? l)) #f)
;          ((eq? (car l) x) i)
;          (else (loop (cdr l) (+ i 1))))))
;
;(define (proc-obj-code obj) (vector-ref obj 3))
;
;(define (proc-obj? x)
;  (and (vector? x)
;       (> (vector-length x) 0)
;       (eq? (vector-ref x 0) proc-obj-tag)))
;
;(define proc-obj-tag (list 'proc-obj))
;
; (define (test obj)
;   (if (and (proc-obj? obj) (not (proc-obj-code obj)))
;     #f
;     (let ((n (pos-in-list obj (queue->list objects-dumped))))
;       (if n
;         n
;         (let ((m (length (queue->list objects-dumped))))
;           (queue-put! objects-dumped obj)
;           (queue-put! object-queue obj)
;           m)))))
;
;
;;(define (test obj)
;;  (let ((m (length (queue->list objects-dumped))))
;;    (queue-put! objects-dumped obj)
;;    (queue-put! object-queue obj)
;;    m)
;;)
;
;(queue-put! objects-dumped 'a)
;(queue-put! objects-dumped 'b)
;(write (queue->list objects-dumped))
;(write (test 'c))
;(write (my-not (test 'd)))
;;(write (my-string<=? (symbol->string (car objects-dumped))))

(define current-fs 2)
(define pointer-size 4)
(define (add-n-to-loc68 x y)
  ;; TODO: these should be 4 15, NOT 0
  (write `(add-n-to-loc68 ,x ,y))
  (newline))

(define (adjust-current-fs n) (set! current-fs (+ current-fs n)))
(define (resize-frame n)
  (write `(resize-frame ,n ,current-fs)) (newline)
  (let ((x (- n current-fs)))
    (adjust-current-fs x)
    (add-n-to-loc68 (* (- pointer-size) x) 15)))

(write (resize-frame 1))

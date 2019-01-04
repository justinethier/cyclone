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
(inline
  my-string<=?
  my-not)

    (define (my-string<=? str1 str2) (<= (string-cmp str1 str2) 0))
    (define (my-not x) (if x #f #t))

(define (queue-empty) (cons '() '()))
(define objects-dumped (queue-empty))
(define object-queue (queue-empty))
(define (queue->list queue) (car queue))
(define (queue-put! queue x)
  (let ((entry (cons x '())))
    (if (null? (car queue))
        (set-car! queue entry)
        (set-cdr! (cdr queue) entry))
    (set-cdr! queue entry)
    x))


(define (test obj)
  (let ((m (length (queue->list objects-dumped))))
    (queue-put! objects-dumped obj)
    (queue-put! object-queue obj)
    m)
)

(queue-put! objects-dumped 'a)
(queue-put! objects-dumped 'b)
(write (queue->list objects-dumped))
(write (test 'c))
(write (my-not (test 'd)))
(write (my-string<=? (symbol->string (car objects-dumped))))

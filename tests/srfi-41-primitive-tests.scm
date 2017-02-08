(import
  (scheme base)
  (srfi 41 primitive)
  (scheme cyclone test))

(test-group
  "srfi 41 (primitive)"
  (define strm123 (stream-cons 1 (stream-cons 2 (stream-cons 3 stream-null))))
  (test 1 (stream-car strm123))
  (test 2 (stream-car (stream-cdr strm123)))
  (test-not (stream-pair? (stream-cdr (stream-cons (/ 1 0) stream-null))))
  (test-not (stream? (list 1 2 3)))
  
  (define iter (stream-lambda (f x) 
                              (stream-cons x (iter f (f x)))))
  
  (define nats (iter (lambda (x) (+ x 1)) 0))
  
  (test 1 (stream-car (stream-cdr nats)))
  
  (define stream-add
    (stream-lambda (s1 s2)
                   (stream-cons (+ (stream-car s1) (stream-car s2))
                                (stream-add (stream-cdr s1)
                                            (stream-cdr s2)))))

  (define evens (stream-add nats nats))
  
  (test 0 (stream-car evens))
  (test 2 (stream-car (stream-cdr evens)))
  (test 4 (stream-car (stream-cdr (stream-cdr evens)))))

(test-exit)

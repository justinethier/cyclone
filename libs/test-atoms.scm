(import 
  (scheme base) 
  (scheme write)
  (cyclone concurrency)
  )

(define lis '(1 2))
(define a (make-atom lis))
(write
  (list
    a
    (ref a)
    (compare-and-set! a 1 lis)
    (ref a)
    (compare-and-set! a lis 1)
    (ref a)
))
(newline)
(write
  (list
    (make-shared '(1 (2 (3))))
    (make-shared 1)
    (make-shared 1.0)
    (make-shared "this is a string")
    (make-shared #(a b c d))
    (make-shared #u8(1 2 3 4))
    ))

;; Experimenting with primitives and continuations.
;; There are several primitives that do not require conts. Can we 
;; compile them in such as way that they are not wrapped in a cont?
;; idea is to reduce compiled code, and number of allocated closures.
(import
  (scheme base)
  (scheme write))

(define (test a b c)
  (write
    (cons
      (+ a b c)
      (- a b c))))

(test 1 2 3)

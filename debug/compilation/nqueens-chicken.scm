;(cond-expand
;  (cyclone
;    (import
;      (scheme base)
;      (scheme write))
;   )
;  (else #f))

(define (nqueens n)

  (define (dec-to n)
    (let loop ((i n) (l '()))
      (if (= i 0) l (loop (- i 1) (cons i l)))))

  (define (try x y z)
    (if (null? x)
      (if (null? y)
        1
        0)
      (+ (if (ok? (car x) 1 z)
           (try (append (cdr x) y) '() (cons (car x) z))
           0)
         (try (cdr x) (cons (car x) y) z))))

  (define (ok? row dist placed)
    (if (null? placed)
      #t
      (and (not (= (car placed) (+ row dist)))
           (not (= (car placed) (- row dist)))
           (ok? row (+ dist 1) (cdr placed)))))

  (try (dec-to n) '() '()))

(write
  (nqueens 8))

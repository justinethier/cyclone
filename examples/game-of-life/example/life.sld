(define-library (example life)
  (import (except (scheme base) set!))
  (cond-expand
    (cyclone
     (import (scheme write))
     (import (example grid))
     
     ))
           
  (cond-expand
    (cyclone
  (export life)
  ))

  (cond-expand
    (cyclone
    (begin
    (define (life-count grid i j)
      (define (count i j)
        (if (ref grid i j) 1 0))
      (+ (count (- i 1) (- j 1))
         (count (- i 1) j)
         (count (- i 1) (+ j 1))
         (count i (- j 1))
         (count i (+ j 1))
         (count (+ i 1) (- j 1))
         (count (+ i 1) j)
         (count (+ i 1) (+ j 1))))
    )
    (begin
    (define (life-alive? grid i j)
      (case (life-count grid i j)
            ((3) #t)
            ((2) (ref grid i j))
            (else #f)))
)

    )
  )
  (begin
    (define (clear-vt100)
        (display
            (string 
                (integer->char #x1B) 
                #\[
                #\H
                (integer->char #x1B)
                #\[
                #\J)))
    (define (life-print grid)
      (clear-vt100)
      (each grid
        (lambda (i j v)
          (display (if v "*" " "))
          (if (= j (- (cols grid) 1))
          ;(when (= j (- (cols grid) 1))
            (newline)))))
    (define (life grid iterations)
      (do ((i 0 (+ i 1))
           (grid0 grid grid1)
           (grid1 (make (rows grid) (cols grid))
                  grid0))
          ((= i iterations))
        (each grid0
          (lambda (j k v)
            (let ((a (life-alive? grid0 j k)))
              (put! grid1 j k a))))
              ;(set! grid1 j k a))))
        (life-print grid1)))))

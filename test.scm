(define (eval2 exp . env)
  exp)

(define (test)
  (call/cc
    (lambda (k)
      (write (list 'result
      (with-exception-handler
        (lambda (obj)
          (display "Error: ")
          (cond
            ((pair? obj)
             (for-each
               (lambda (o)
                 (display o)
                 (display " "))
               obj))
            (else
              (display obj)))
          (display "\n")
          (k #t))
        (lambda ()
          (repl))))))))
(define (repl)
  (display "cyclone> ")
  ;(let ((c 'done)) ;(read))))   ;; fine with this line
  (let ((c (eval2 'done))) ;(read)))) ;; Crashes with this line
  ;(let ((c (read)))
    (cond
      ((not (eof-object? c))
       (write c)
       ;(test)
       )
      (else 
        (exit 0) ;; TODO: crashes on this branch... WTF?
        ))))
(test)

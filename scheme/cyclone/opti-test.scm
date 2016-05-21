;; Temporary file to test changes to the optimization library
(import (cps-optimizations)
;        (ast)
        (scheme cyclone pretty-print)
        (scheme write)
        (scheme base))

;(adb:init!)
;(adb:set! 'v (adb:make-var))
;
;(let ((v (adb:get 'v)))
;  (adbv:set-global! v #t)
;  (adbv:set-defined-by! v 1)
;
;  (display v)
;  (newline)
;  (display (adbv:defined-by v))
;  (newline))
;
;
;(define l (ast:make-lambda '() '((write 1) (write 2) (write 3))))
;(write l)
;(newline)
;(set! l (ast:make-lambda '() '((write 1) (write 2) (write 3))))
;(write `(l is now ,l))
;(newline)

(define input-program
'((define test
   #((record-marker)
     #((record-marker) #f (id args body))
     #(9 (k$11 a$4) ((write k$11 3)))))
 (#((record-marker)
    #((record-marker) #f (id args body))
    #(8
      (y$1)
      ((#((record-marker)
          #((record-marker) #f (id args body))
          #(7
            ()
            ((#((record-marker)
                #((record-marker) #f (id args body))
                #(6
                  (r$5)
                  ((#((record-marker)
                      #((record-marker) #f (id args body))
                      #(5
                        (x$2)
                        ((#((record-marker)
                            #((record-marker) #f (id args body))
                            #(4
                              (y$3)
                              ((#((record-marker)
                                  #((record-marker) #f (id args body))
                                  #(3
                                    (r$7)
                                    ((#((record-marker)
                                        #((record-marker) #f (id args body))
                                        #(2
                                          (r$8)
                                          ((write #((record-marker)
                                                    #((record-marker)
                                                      #f
                                                      (id args body))
                                                    #(1 (r$6) ((test %halt 4))))
                                                  r$8))))
                                      (+ x$2 y$3)))))
                                (set! y$3 2)))))
                          #f))))
                    1))))
              0))))))))
  #f))
)

      (set! input-program
        (optimize-cps input-program))

      (pretty-print "---------------- cps analysis db:")
      (pretty-print (adb:get-db))
      (pretty-print "---------------- after cps optimizations:")
      (pretty-print input-program)

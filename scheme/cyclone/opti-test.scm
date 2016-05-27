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
'((define nqueens
   #((record-marker)
     #((record-marker) #f (id args body))
     #(51
       (k$30 n$4)
       ((#((record-marker)
           #((record-marker) #f (id args body))
           #(50
             (dec-to$10 ok?$9 try$8)
             ((#((record-marker)
                 #((record-marker) #f (id args body))
                 #(49
                   (dec-to$13 try$12 ok?$11)
                   ((#((record-marker)
                       #((record-marker) #f (id args body))
                       #(38
                         (r$65)
                         ((#((record-marker)
                             #((record-marker) #f (id args body))
                             #(37
                               (r$31)
                               ((#((record-marker)
                                   #((record-marker) #f (id args body))
                                   #(20
                                     (r$48)
                                     ((#((record-marker)
                                         #((record-marker) #f (id args body))
                                         #(19
                                           (r$32)
                                           ((#((record-marker)
                                               #((record-marker)
                                                 #f
                                                 (id args body))
                                               #(8
                                                 (r$37)
                                                 ((#((record-marker)
                                                     #((record-marker)
                                                       #f
                                                       (id args body))
                                                     #(7
                                                       (r$33)
                                                       ((dec-to$10
                                                          #((record-marker)
                                                            #((record-marker)
                                                              #f
                                                              (id args body))
                                                            #(6
                                                              (r$34)
                                                              ((#((record-marker)
                                                                  #((record-marker)
                                                                    #f
                                                                    (id args
                                                                        body))
                                                                  #(5
                                                                    (r$35)
                                                                    ((#((record-marker)
                                                                        #((record-marker)
                                                                          #f
                                                                          (id args
                                                                              body))
                                                                        #(4
                                                                          (r$36)
                                                                          ((try$8 k$30
                                                                                  r$34
                                                                                  r$35
                                                                                  r$36))))
                                                                      '()))))
                                                                '()))))
                                                          n$4))))
                                                   (set! ok?$9 r$37)))))
                                             #((record-marker)
                                               #((record-marker)
                                                 #f
                                                 (id args body))
                                               #(18
                                                 (k$38 row$16 dist$15 placed$14)
                                                 ((#((record-marker)
                                                     #((record-marker)
                                                       #f
                                                       (id args body))
                                                     #(17
                                                       (r$39)
                                                       ((if r$39
                                                          (k$38 #t)
                                                          (#((record-marker)
                                                             #((record-marker)
                                                               #f
                                                               (id args body))
                                                             #(16
                                                               (r$46)
                                                               ((#((record-marker)
                                                                   #((record-marker)
                                                                     #f
                                                                     (id args
                                                                         body))
                                                                   #(15
                                                                     (r$47)
                                                                     ((#((record-marker)
                                                                         #((record-marker)
                                                                           #f
                                                                           (id args
                                                                               body))
                                                                         #(14
                                                                           (r$40)
                                                                           ((if r$40
                                                                              (k$38 #f)
                                                                              (#((record-marker)
                                                                                 #((record-marker)
                                                                                   #f
                                                                                   (id args
                                                                                       body))
                                                                                 #(13
                                                                                   (r$44)
                                                                                   ((#((record-marker)
                                                                                       #((record-marker)
                                                                                         #f
                                                                                         (id args
                                                                                             body))
                                                                                       #(12
                                                                                         (r$45)
                                                                                         ((#((record-marker)
                                                                                             #((record-marker)
                                                                                               #f
                                                                                               (id args
                                                                                                   body))
                                                                                             #(11
                                                                                               (r$41)
                                                                                               ((if r$41
                                                                                                  (k$38 #f)
                                                                                                  (#((record-marker)
                                                                                                     #((record-marker)
                                                                                                       #f
                                                                                                       (id args
                                                                                                           body))
                                                                                                     #(10
                                                                                                       (r$42)
                                                                                                       ((#((record-marker)
                                                                                                           #((record-marker)
                                                                                                             #f
                                                                                                             (id args
                                                                                                                 body))
                                                                                                           #(9
                                                                                                             (r$43)
                                                                                                             ((ok?$9 k$38
                                                                                                                     row$16
                                                                                                                     r$42
                                                                                                                     r$43))))
                                                                                                         (cdr placed$14)))))
                                                                                                   (+ dist$15
                                                                                                      1))))))
                                                                                           (= r$44
                                                                                              r$45)))))
                                                                                     (- row$16
                                                                                        dist$15)))))
                                                                               (car placed$14))))))
                                                                       (= r$46
                                                                          r$47)))))
                                                                 (+ row$16
                                                                    dist$15)))))
                                                           (car placed$14))))))
                                                   (null? placed$14)))))))))
                                       (set! try$8 r$48)))))
                                 #((record-marker)
                                   #((record-marker) #f (id args body))
                                   #(36
                                     (k$49 x$19 y$18 z$17)
                                     ((#((record-marker)
                                         #((record-marker) #f (id args body))
                                         #(35
                                           (r$50)
                                           ((if r$50
                                              (#((record-marker)
                                                 #((record-marker)
                                                   #f
                                                   (id args body))
                                                 #(21
                                                   (r$51)
                                                   ((if r$51
                                                      (k$49 1)
                                                      (k$49 0)))))
                                               (null? y$18))
                                              (#((record-marker)
                                                 #((record-marker)
                                                   #f
                                                   (id args body))
                                                 #(34
                                                   (k$57)
                                                   ((#((record-marker)
                                                       #((record-marker)
                                                         #f
                                                         (id args body))
                                                       #(33
                                                         (r$64)
                                                         ((ok?$9 #((record-marker)
                                                                   #((record-marker)
                                                                     #f
                                                                     (id args
                                                                         body))
                                                                   #(32
                                                                     (r$58)
                                                                     ((if r$58
                                                                        (#((record-marker)
                                                                           #((record-marker)
                                                                             #f
                                                                             (id args
                                                                                 body))
                                                                           #(31
                                                                             (r$63)
                                                                             ((append
                                                                                #((record-marker)
                                                                                  #((record-marker)
                                                                                    #f
                                                                                    (id args
                                                                                        body))
                                                                                  #(30
                                                                                    (r$59)
                                                                                    ((#((record-marker)
                                                                                        #((record-marker)
                                                                                          #f
                                                                                          (id args
                                                                                              body))
                                                                                        #(29
                                                                                          (r$60)
                                                                                          ((#((record-marker)
                                                                                              #((record-marker)
                                                                                                #f
                                                                                                (id args
                                                                                                    body))
                                                                                              #(28
                                                                                                (r$62)
                                                                                                ((#((record-marker)
                                                                                                    #((record-marker)
                                                                                                      #f
                                                                                                      (id args
                                                                                                          body))
                                                                                                    #(27
                                                                                                      (r$61)
                                                                                                      ((try$8 k$57
                                                                                                              r$59
                                                                                                              r$60
                                                                                                              r$61))))
                                                                                                  (cons r$62
                                                                                                        z$17)))))
                                                                                            (car x$19)))))
                                                                                      '()))))
                                                                                r$63
                                                                                y$18))))
                                                                         (cdr x$19))
                                                                        (k$57 0)))))
                                                                 r$64
                                                                 1
                                                                 z$17))))
                                                     (car x$19)))))
                                               #((record-marker)
                                                 #((record-marker)
                                                   #f
                                                   (id args body))
                                                 #(26
                                                   (r$52)
                                                   ((#((record-marker)
                                                       #((record-marker)
                                                         #f
                                                         (id args body))
                                                       #(25
                                                         (r$54)
                                                         ((#((record-marker)
                                                             #((record-marker)
                                                               #f
                                                               (id args body))
                                                             #(24
                                                               (r$56)
                                                               ((#((record-marker)
                                                                   #((record-marker)
                                                                     #f
                                                                     (id args
                                                                         body))
                                                                   #(23
                                                                     (r$55)
                                                                     ((try$8 #((record-marker)
                                                                               #((record-marker)
                                                                                 #f
                                                                                 (id args
                                                                                     body))
                                                                               #(22
                                                                                 (r$53)
                                                                                 ((k$49 (+ r$52
                                                                                           r$53)))))
                                                                             r$54
                                                                             r$55
                                                                             z$17))))
                                                                 (cons r$56
                                                                       y$18)))))
                                                           (car x$19)))))
                                                     (cdr x$19))))))))))
                                       (null? x$19)))))))))
                           (set! dec-to$10 r$65)))))
                     #((record-marker)
                       #((record-marker) #f (id args body))
                       #(48
                         (k$66 n$20)
                         ((#((record-marker)
                             #((record-marker) #f (id args body))
                             #(47
                               (r$67)
                               ((#((record-marker)
                                   #((record-marker) #f (id args body))
                                   #(46
                                     (i$22 l$21)
                                     ((#((record-marker)
                                         #((record-marker) #f (id args body))
                                         #(45
                                           (loop$23)
                                           ((#((record-marker)
                                               #((record-marker)
                                                 #f
                                                 (id args body))
                                               #(40
                                                 (r$69)
                                                 ((#((record-marker)
                                                     #((record-marker)
                                                       #f
                                                       (id args body))
                                                     #(39
                                                       (r$68)
                                                       ((loop$23
                                                          k$66
                                                          i$22
                                                          l$21))))
                                                   (set! loop$23 r$69)))))
                                             #((record-marker)
                                               #((record-marker)
                                                 #f
                                                 (id args body))
                                               #(44
                                                 (k$70 i$25 l$24)
                                                 ((#((record-marker)
                                                     #((record-marker)
                                                       #f
                                                       (id args body))
                                                     #(43
                                                       (r$71)
                                                       ((if r$71
                                                          (k$70 l$24)
                                                          (#((record-marker)
                                                             #((record-marker)
                                                               #f
                                                               (id args body))
                                                             #(42
                                                               (r$72)
                                                               ((#((record-marker)
                                                                   #((record-marker)
                                                                     #f
                                                                     (id args
                                                                         body))
                                                                   #(41
                                                                     (r$73)
                                                                     ((loop$23
                                                                        k$70
                                                                        r$72
                                                                        r$73))))
                                                                 (cons i$25
                                                                       l$24)))))
                                                           (- i$25 1))))))
                                                   (= i$25 0)))))))))
                                       #f))))
                                 n$20
                                 r$67))))
                           '()))))))))
               #f
               #f
               #f))))
         #f
         #f
         #f)))))
 (#((record-marker)
    #((record-marker) #f (id args body))
    #(3
      ()
      ((#((record-marker)
          #((record-marker) #f (id args body))
          #(2
            (r$26)
            ((nqueens
               #((record-marker)
                 #((record-marker) #f (id args body))
                 #(1 (r$27) ((write %halt r$27))))
               8))))
        0))))))
)

      (set! input-program
        (optimize-cps input-program))

      (pretty-print "---------------- cps analysis db:")
      (pretty-print (adb:get-db))
      (pretty-print "---------------- after cps optimizations:")
      (pretty-print input-program)

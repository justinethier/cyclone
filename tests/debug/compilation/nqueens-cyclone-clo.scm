
CHICKEN
(c)2008-2011 The Chicken Team
(c)2000-2007 Felix L. Winkelmann
Version 4.7.0 
linux-unix-gnu-x86 [ manyargs dload ptables ]
compiled 2011-10-17 on roseapple (Linux)

; loading nqueens-cyclone-clo.scm ...
((define nqueens
   (lambda (k$30 n$4)
     ((%closure
        (lambda (self$77 dec-to$10 ok?$9 try$8)
          ((%closure
             (lambda (self$78 dec-to$10)
               ((%closure
                  (lambda (self$79 ok?$9)
                    ((%closure
                       (lambda (self$80 try$8)
                         ((%closure
                            (lambda (self$81 dec-to$13 try$12 ok?$11)
                              ((%closure
                                 (lambda (self$93 r$67)
                                   ((%closure
                                      (lambda (self$94 r$31)
                                        ((%closure
                                           (lambda (self$111 r$50)
                                             ((%closure
                                                (lambda (self$112 r$32)
                                                  ((%closure
                                                     (lambda (self$125 r$37)
                                                       ((%closure
                                                          (lambda (self$126 r$33)
                                                            ((%closure-ref
                                                               (cell-get (%closure-ref self$126 1))
                                                               0)
                                                             (cell-get (%closure-ref self$126 1))
                                                             (%closure
                                                               (lambda (self$127 r$34)
                                                                 ((%closure
                                                                    (lambda (self$128 r$35)
                                                                      ((%closure
                                                                         (lambda (self$129 r$36)
                                                                           ((%closure-ref
                                                                              (cell-get (%closure-ref self$129 4))
                                                                              0)
                                                                            (cell-get (%closure-ref self$129 4))
                                                                            (%closure-ref self$129 1)
                                                                            (%closure-ref self$129 2)
                                                                            (%closure-ref self$129 3)
                                                                            r$36))
                                                                         (%closure-ref self$128 1)
                                                                         (%closure-ref self$128 2)
                                                                         r$35
                                                                         (%closure-ref self$128 3))
                                                                       '()))
                                                                    (%closure-ref self$127 1)
                                                                    r$34
                                                                    (%closure-ref self$127 2))
                                                                  '()))
                                                               (%closure-ref self$126 2)
                                                               (%closure-ref self$126 4))
                                                             (%closure-ref self$126 3)))
                                                          (%closure-ref self$125 1)
                                                          (%closure-ref self$125 2)
                                                          (%closure-ref self$125 3)
                                                          (%closure-ref self$125 5))
                                                        (set-cell! (%closure-ref self$125 4) r$37)))
                                                     (%closure-ref self$112 1)
                                                     (%closure-ref self$112 2)
                                                     (%closure-ref self$112 3)
                                                     (%closure-ref self$112 4)
                                                     (%closure-ref self$112 5))
                                                   (%closure
                                                     (lambda (self$113 k$38 row$16 dist$15 placed$14)
                                                       ((%closure
                                                          (lambda (self$114 r$39)
                                                            (if r$39
                                                              ((%closure-ref (%closure-ref self$114 2) 0)
                                                               (%closure-ref self$114 2)
                                                               #t)
                                                              ((%closure
                                                                 (lambda (self$115 r$48)
                                                                   ((%closure
                                                                      (lambda (self$116 r$49)
                                                                        ((%closure
                                                                           (lambda (self$117 r$47)
                                                                             ((%closure-ref not 0)
                                                                              not
                                                                              (%closure
                                                                                (lambda (self$118 r$40)
                                                                                  (if r$40
                                                                                    ((%closure
                                                                                       (lambda (self$119 r$45)
                                                                                         ((%closure
                                                                                            (lambda (self$120 r$46)
                                                                                              ((%closure
                                                                                                 (lambda (self$121 r$44)
                                                                                                   ((%closure-ref not 0)
                                                                                                    not
                                                                                                    (%closure
                                                                                                      (lambda (self$122 r$41)
                                                                                                        (if r$41
                                                                                                          ((%closure
                                                                                                             (lambda (self$123 r$42)
                                                                                                               ((%closure
                                                                                                                  (lambda (self$124 r$43)
                                                                                                                    ((%closure-ref
                                                                                                                       (cell-get (%closure-ref self$124 2))
                                                                                                                       0)
                                                                                                                     (cell-get (%closure-ref self$124 2))
                                                                                                                     (%closure-ref self$124 1)
                                                                                                                     (%closure-ref self$124 4)
                                                                                                                     (%closure-ref self$124 3)
                                                                                                                     r$43))
                                                                                                                  (%closure-ref self$123 1)
                                                                                                                  (%closure-ref self$123 2)
                                                                                                                  r$42
                                                                                                                  (%closure-ref self$123 4))
                                                                                                                (cdr (%closure-ref self$123 3))))
                                                                                                             (%closure-ref self$122 2)
                                                                                                             (%closure-ref self$122 3)
                                                                                                             (%closure-ref self$122 4)
                                                                                                             (%closure-ref self$122 5))
                                                                                                           (+ (%closure-ref self$122 1) 1))
                                                                                                          ((%closure-ref (%closure-ref self$122 2) 0)
                                                                                                           (%closure-ref self$122 2)
                                                                                                           #f)))
                                                                                                      (%closure-ref self$121 1)
                                                                                                      (%closure-ref self$121 2)
                                                                                                      (%closure-ref self$121 3)
                                                                                                      (%closure-ref self$121 4)
                                                                                                      (%closure-ref self$121 5))
                                                                                                    r$44))
                                                                                                 (%closure-ref self$120 1)
                                                                                                 (%closure-ref self$120 2)
                                                                                                 (%closure-ref self$120 3)
                                                                                                 (%closure-ref self$120 4)
                                                                                                 (%closure-ref self$120 6))
                                                                                               (= (%closure-ref self$120 5) r$46)))
                                                                                            (%closure-ref self$119 1)
                                                                                            (%closure-ref self$119 2)
                                                                                            (%closure-ref self$119 3)
                                                                                            (%closure-ref self$119 4)
                                                                                            r$45
                                                                                            (%closure-ref self$119 5))
                                                                                          (- (%closure-ref self$119 5)
                                                                                             (%closure-ref self$119 1))))
                                                                                       (%closure-ref self$118 1)
                                                                                       (%closure-ref self$118 2)
                                                                                       (%closure-ref self$118 3)
                                                                                       (%closure-ref self$118 4)
                                                                                       (%closure-ref self$118 5))
                                                                                     (car (%closure-ref self$118 4)))
                                                                                    ((%closure-ref (%closure-ref self$118 2) 0)
                                                                                     (%closure-ref self$118 2)
                                                                                     #f)))
                                                                                (%closure-ref self$117 1)
                                                                                (%closure-ref self$117 2)
                                                                                (%closure-ref self$117 3)
                                                                                (%closure-ref self$117 4)
                                                                                (%closure-ref self$117 5))
                                                                              r$47))
                                                                           (%closure-ref self$116 1)
                                                                           (%closure-ref self$116 2)
                                                                           (%closure-ref self$116 3)
                                                                           (%closure-ref self$116 4)
                                                                           (%closure-ref self$116 6))
                                                                         (= (%closure-ref self$116 5) r$49)))
                                                                      (%closure-ref self$115 1)
                                                                      (%closure-ref self$115 2)
                                                                      (%closure-ref self$115 3)
                                                                      (%closure-ref self$115 4)
                                                                      r$48
                                                                      (%closure-ref self$115 5))
                                                                    (+ (%closure-ref self$115 5)
                                                                       (%closure-ref self$115 1))))
                                                                 (%closure-ref self$114 1)
                                                                 (%closure-ref self$114 2)
                                                                 (%closure-ref self$114 3)
                                                                 (%closure-ref self$114 4)
                                                                 (%closure-ref self$114 5))
                                                               (car (%closure-ref self$114 4)))))
                                                          dist$15
                                                          k$38
                                                          (%closure-ref self$113 1)
                                                          placed$14
                                                          row$16)
                                                        (null? placed$14)))
                                                     (%closure-ref self$112 4))))
                                                (%closure-ref self$111 1)
                                                (%closure-ref self$111 2)
                                                (%closure-ref self$111 3)
                                                (%closure-ref self$111 4)
                                                (%closure-ref self$111 5))
                                              (set-cell! (%closure-ref self$111 5) r$50)))
                                           (%closure-ref self$94 1)
                                           (%closure-ref self$94 2)
                                           (%closure-ref self$94 3)
                                           (%closure-ref self$94 4)
                                           (%closure-ref self$94 5))
                                         (%closure
                                           (lambda (self$95 k$51 x$19 y$18 z$17)
                                             ((%closure
                                                (lambda (self$96 r$52)
                                                  (if r$52
                                                    ((%closure
                                                       (lambda (self$110 r$53)
                                                         (if r$53
                                                           ((%closure-ref (%closure-ref self$110 1) 0)
                                                            (%closure-ref self$110 1)
                                                            1)
                                                           ((%closure-ref (%closure-ref self$110 1) 0)
                                                            (%closure-ref self$110 1)
                                                            0)))
                                                       (%closure-ref self$96 1))
                                                     (null? (%closure-ref self$96 5)))
                                                    ((%closure
                                                       (lambda (self$102 k$59)
                                                         ((%closure
                                                            (lambda (self$103 r$66)
                                                              ((%closure-ref
                                                                 (cell-get (%closure-ref self$103 2))
                                                                 0)
                                                               (cell-get (%closure-ref self$103 2))
                                                               (%closure
                                                                 (lambda (self$104 r$60)
                                                                   (if r$60
                                                                     ((%closure
                                                                        (lambda (self$105 r$65)
                                                                          ((%closure-ref append 0)
                                                                           append
                                                                           (%closure
                                                                             (lambda (self$106 r$61)
                                                                               ((%closure
                                                                                  (lambda (self$107 r$62)
                                                                                    ((%closure
                                                                                       (lambda (self$108 r$64)
                                                                                         ((%closure
                                                                                            (lambda (self$109 r$63)
                                                                                              ((%closure-ref
                                                                                                 (cell-get (%closure-ref self$109 4))
                                                                                                 0)
                                                                                               (cell-get (%closure-ref self$109 4))
                                                                                               (%closure-ref self$109 1)
                                                                                               (%closure-ref self$109 2)
                                                                                               (%closure-ref self$109 3)
                                                                                               r$63))
                                                                                            (%closure-ref self$108 1)
                                                                                            (%closure-ref self$108 2)
                                                                                            (%closure-ref self$108 3)
                                                                                            (%closure-ref self$108 4))
                                                                                          (cons r$64 (%closure-ref self$108 5))))
                                                                                       (%closure-ref self$107 1)
                                                                                       (%closure-ref self$107 2)
                                                                                       r$62
                                                                                       (%closure-ref self$107 3)
                                                                                       (%closure-ref self$107 5))
                                                                                     (car (%closure-ref self$107 4))))
                                                                                  (%closure-ref self$106 1)
                                                                                  r$61
                                                                                  (%closure-ref self$106 2)
                                                                                  (%closure-ref self$106 3)
                                                                                  (%closure-ref self$106 4))
                                                                                '()))
                                                                             (%closure-ref self$105 1)
                                                                             (%closure-ref self$105 2)
                                                                             (%closure-ref self$105 3)
                                                                             (%closure-ref self$105 5))
                                                                           r$65
                                                                           (%closure-ref self$105 4)))
                                                                        (%closure-ref self$104 1)
                                                                        (%closure-ref self$104 2)
                                                                        (%closure-ref self$104 3)
                                                                        (%closure-ref self$104 4)
                                                                        (%closure-ref self$104 5))
                                                                      (cdr (%closure-ref self$104 3)))
                                                                     ((%closure-ref (%closure-ref self$104 1) 0)
                                                                      (%closure-ref self$104 1)
                                                                      0)))
                                                                 (%closure-ref self$103 1)
                                                                 (%closure-ref self$103 3)
                                                                 (%closure-ref self$103 4)
                                                                 (%closure-ref self$103 5)
                                                                 (%closure-ref self$103 6))
                                                               r$66
                                                               1
                                                               (%closure-ref self$103 6)))
                                                            k$59
                                                            (%closure-ref self$102 1)
                                                            (%closure-ref self$102 2)
                                                            (%closure-ref self$102 3)
                                                            (%closure-ref self$102 4)
                                                            (%closure-ref self$102 5))
                                                          (car (%closure-ref self$102 3))))
                                                       (%closure-ref self$96 2)
                                                       (%closure-ref self$96 3)
                                                       (%closure-ref self$96 4)
                                                       (%closure-ref self$96 5)
                                                       (%closure-ref self$96 6))
                                                     (%closure
                                                       (lambda (self$97 r$54)
                                                         ((%closure
                                                            (lambda (self$98 r$56)
                                                              ((%closure
                                                                 (lambda (self$99 r$58)
                                                                   ((%closure
                                                                      (lambda (self$100 r$57)
                                                                        ((%closure-ref
                                                                           (cell-get (%closure-ref self$100 4))
                                                                           0)
                                                                         (cell-get (%closure-ref self$100 4))
                                                                         (%closure
                                                                           (lambda (self$101 r$55)
                                                                             ((%closure-ref (%closure-ref self$101 1) 0)
                                                                              (%closure-ref self$101 1)
                                                                              (+ (%closure-ref self$101 2) r$55)))
                                                                           (%closure-ref self$100 1)
                                                                           (%closure-ref self$100 2))
                                                                         (%closure-ref self$100 3)
                                                                         r$57
                                                                         (%closure-ref self$100 5)))
                                                                      (%closure-ref self$99 1)
                                                                      (%closure-ref self$99 2)
                                                                      (%closure-ref self$99 3)
                                                                      (%closure-ref self$99 4)
                                                                      (%closure-ref self$99 6))
                                                                    (cons r$58 (%closure-ref self$99 5))))
                                                                 (%closure-ref self$98 1)
                                                                 (%closure-ref self$98 2)
                                                                 r$56
                                                                 (%closure-ref self$98 3)
                                                                 (%closure-ref self$98 5)
                                                                 (%closure-ref self$98 6))
                                                               (car (%closure-ref self$98 4))))
                                                            (%closure-ref self$97 1)
                                                            r$54
                                                            (%closure-ref self$97 2)
                                                            (%closure-ref self$97 3)
                                                            (%closure-ref self$97 4)
                                                            (%closure-ref self$97 5))
                                                          (cdr (%closure-ref self$97 3))))
                                                       (%closure-ref self$96 1)
                                                       (%closure-ref self$96 3)
                                                       (%closure-ref self$96 4)
                                                       (%closure-ref self$96 5)
                                                       (%closure-ref self$96 6)))))
                                                k$51
                                                (%closure-ref self$95 1)
                                                (%closure-ref self$95 2)
                                                x$19
                                                y$18
                                                z$17)
                                              (null? x$19)))
                                           (%closure-ref self$94 4)
                                           (%closure-ref self$94 5))))
                                      (%closure-ref self$93 1)
                                      (%closure-ref self$93 2)
                                      (%closure-ref self$93 3)
                                      (%closure-ref self$93 4)
                                      (%closure-ref self$93 5))
                                    (set-cell! (%closure-ref self$93 1) r$67)))
                                 (%closure-ref self$81 1)
                                 (%closure-ref self$81 2)
                                 (%closure-ref self$81 3)
                                 (%closure-ref self$81 4)
                                 (%closure-ref self$81 5))
                               (%closure
                                 (lambda (self$82 k$68 n$20)
                                   ((%closure
                                      (lambda (self$83 r$69)
                                        ((%closure
                                           (lambda (self$84 i$22 l$21)
                                             ((%closure
                                                (lambda (self$85 loop$23)
                                                  ((%closure
                                                     (lambda (self$86 loop$23)
                                                       ((%closure
                                                          (lambda (self$91 r$71)
                                                            ((%closure
                                                               (lambda (self$92 r$70)
                                                                 ((%closure-ref
                                                                    (cell-get (%closure-ref self$92 4))
                                                                    0)
                                                                  (cell-get (%closure-ref self$92 4))
                                                                  (%closure-ref self$92 2)
                                                                  (%closure-ref self$92 1)
                                                                  (%closure-ref self$92 3)))
                                                               (%closure-ref self$91 1)
                                                               (%closure-ref self$91 2)
                                                               (%closure-ref self$91 3)
                                                               (%closure-ref self$91 4))
                                                             (set-cell! (%closure-ref self$91 4) r$71)))
                                                          (%closure-ref self$86 1)
                                                          (%closure-ref self$86 2)
                                                          (%closure-ref self$86 3)
                                                          loop$23)
                                                        (%closure
                                                          (lambda (self$87 k$72 i$25 l$24)
                                                            ((%closure
                                                               (lambda (self$88 r$73)
                                                                 (if r$73
                                                                   ((%closure-ref (%closure-ref self$88 2) 0)
                                                                    (%closure-ref self$88 2)
                                                                    (%closure-ref self$88 3))
                                                                   ((%closure
                                                                      (lambda (self$89 r$74)
                                                                        ((%closure
                                                                           (lambda (self$90 r$75)
                                                                             ((%closure-ref
                                                                                (cell-get (%closure-ref self$90 2))
                                                                                0)
                                                                              (cell-get (%closure-ref self$90 2))
                                                                              (%closure-ref self$90 1)
                                                                              (%closure-ref self$90 3)
                                                                              r$75))
                                                                           (%closure-ref self$89 2)
                                                                           (%closure-ref self$89 4)
                                                                           r$74)
                                                                         (cons (%closure-ref self$89 1)
                                                                               (%closure-ref self$89 3))))
                                                                      (%closure-ref self$88 1)
                                                                      (%closure-ref self$88 2)
                                                                      (%closure-ref self$88 3)
                                                                      (%closure-ref self$88 4))
                                                                    (- (%closure-ref self$88 1) 1))))
                                                               i$25
                                                               k$72
                                                               l$24
                                                               (%closure-ref self$87 1))
                                                             (= i$25 0)))
                                                          loop$23)))
                                                     (%closure-ref self$85 1)
                                                     (%closure-ref self$85 2)
                                                     (%closure-ref self$85 3))
                                                   (cell loop$23)))
                                                i$22
                                                (%closure-ref self$84 1)
                                                l$21)
                                              #f))
                                           (%closure-ref self$83 1))
                                         (%closure-ref self$83 2)
                                         r$69))
                                      k$68
                                      n$20)
                                    '())))))
                            (%closure-ref self$80 1)
                            (%closure-ref self$80 2)
                            (%closure-ref self$80 3)
                            (%closure-ref self$80 4)
                            try$8)
                          #f
                          #f
                          #f))
                       (%closure-ref self$79 1)
                       (%closure-ref self$79 2)
                       (%closure-ref self$79 3)
                       ok?$9)
                     (cell (%closure-ref self$79 4))))
                  dec-to$10
                  (%closure-ref self$78 1)
                  (%closure-ref self$78 2)
                  (%closure-ref self$78 4))
                (cell (%closure-ref self$78 3))))
             (%closure-ref self$77 1)
             (%closure-ref self$77 2)
             ok?$9
             try$8)
           (cell dec-to$10)))
        k$30
        n$4)
      #f
      #f
      #f)))
 ((lambda ()
    ((lambda (r$26)
       ((%closure-ref nqueens 0)
        nqueens
        (%closure
          (lambda (self$76 r$27) ((%closure-ref write 0) write %halt r$27)))
        8))
     0))))
#;1> 

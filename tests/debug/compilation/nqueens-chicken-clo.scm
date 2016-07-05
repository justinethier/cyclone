[closure-converted]
(##core#closure
  (2)
  (lambda (c224 k28)
    (let ((k29 (##core#closure
                 (2)
                 (##core#lambda
                   (c226 r30)
                   (let ((k32 (##core#closure
                                (2)
                                (##core#lambda
                                  (c228 r33)
                                  (let ((t35 (set! nqueens
                                               (##core#closure
                                                 (2)
                                                 (lambda (c230 k37 n1)
                                                   (let ((try3231 (##core#undefined)))
                                                     (let ((try3 (##core#box try3231)))
                                                       (let ((ok?4232 (##core#undefined)))
                                                         (let ((ok?4 (##core#box ok?4232)))
                                                           (let ((t64 (##core#updatebox
                                                                        try3
                                                                        (##core#closure
                                                                          (4)
                                                                          (lambda (c234 k66 x10 y11 z12)
                                                                            (if (##core#inline "C_i_nullp" x10)
                                                                              (let ((r77 (##core#inline "C_i_nullp" y11)))
                                                                                (k66 (##core#cond r77 '1 '0)))
                                                                              (let ((k83 (##core#closure
                                                                                           (6)
                                                                                           (##core#lambda
                                                                                             (c237 r84)
                                                                                             (let ((k87 (##core#closure
                                                                                                          (3)
                                                                                                          (##core#lambda
                                                                                                            (c239 r88)
                                                                                                            ((##core#ref c239 (2))
                                                                                                             (##core#inline_allocate
                                                                                                               ("C_a_i_plus" 4)
                                                                                                               (##core#ref c239 (1))
                                                                                                               r88)))
                                                                                                          r84
                                                                                                          (##core#ref c237 (5)))))
                                                                                               (let ((r92 (##core#inline "C_i_cdr" (##core#ref c237 (4)))))
                                                                                                 (let ((r100 (##core#inline "C_i_car" (##core#ref c237 (4)))))
                                                                                                   (let ((r96 (##core#inline_allocate
                                                                                                                ("C_a_i_cons" 3)
                                                                                                                r100
                                                                                                                (##core#ref c237 (3)))))
                                                                                                     ((##core#unbox (##core#ref c237 (2)) ())
                                                                                                      k87
                                                                                                      r92
                                                                                                      r96
                                                                                                      (##core#ref c237 (1))))))))
                                                                                           z12
                                                                                           (##core#ref c234 (2))
                                                                                           y11
                                                                                           x10
                                                                                           k66)))
                                                                                (let ((k102 (##core#closure
                                                                                              (6)
                                                                                              (##core#lambda
                                                                                                (c244 r103)
                                                                                                (if r103
                                                                                                  (let ((k109 (##core#closure
                                                                                                                (5)
                                                                                                                (##core#lambda
                                                                                                                  (c246 r110)
                                                                                                                  (let ((r118 (##core#inline "C_i_car" (##core#ref c246 (4)))))
                                                                                                                    (let ((r114 (##core#inline_allocate
                                                                                                                                  ("C_a_i_cons" 3)
                                                                                                                                  r118
                                                                                                                                  (##core#ref c246 (3)))))
                                                                                                                      ((##core#unbox (##core#ref c246 (2)) ())
                                                                                                                       (##core#ref c246 (1))
                                                                                                                       r110
                                                                                                                       '()
                                                                                                                       r114))))
                                                                                                                (##core#ref c244 (2))
                                                                                                                (##core#ref c244 (3))
                                                                                                                (##core#ref c244 (4))
                                                                                                                (##core#ref c244 (5)))))
                                                                                                    (let ((r122 (##core#inline "C_i_cdr" (##core#ref c244 (5)))))
                                                                                                      (append k109 r122 (##core#ref c244 (1)))))
                                                                                                  ((##core#ref c244 (2)) '0)))
                                                                                              y11
                                                                                              k83
                                                                                              (##core#ref c234 (2))
                                                                                              z12
                                                                                              x10)))
                                                                                  (let ((r126 (##core#inline "C_i_car" x10)))
                                                                                    ((##core#unbox (##core#ref c234 (1)) ())
                                                                                     k102
                                                                                     r126
                                                                                     '1
                                                                                     z12))))))
                                                                          ok?4
                                                                          try3
                                                                          '#<lambda info (try x10 y11 z12)#>))))
                                                             (let ((t128 (##core#updatebox
                                                                           ok?4
                                                                           (##core#closure
                                                                             (3)
                                                                             (lambda (c252 k130 row13 dist14 placed15)
                                                                               (if (##core#inline "C_i_nullp" placed15)
                                                                                 (k130 '#t)
                                                                                 (let ((r178 (##core#inline "C_i_car" placed15)))
                                                                                   (let ((r182 (##core#inline_allocate
                                                                                                 ("C_a_i_plus" 4)
                                                                                                 row13
                                                                                                 dist14)))
                                                                                     (if (##core#inline "C_i_nequalp" r178 r182)
                                                                                       (k130 '#f)
                                                                                       (let ((r166 (##core#inline "C_i_car" placed15)))
                                                                                         (let ((r170 (##core#inline_allocate
                                                                                                       ("C_a_i_minus" 4)
                                                                                                       row13
                                                                                                       dist14)))
                                                                                           (if (##core#inline "C_i_nequalp" r166 r170)
                                                                                             (k130 '#f)
                                                                                             (let ((r154 (##core#inline_allocate
                                                                                                           ("C_a_i_plus" 4)
                                                                                                           dist14
                                                                                                           '1)))
                                                                                               (let ((r158 (##core#inline "C_i_cdr" placed15)))
                                                                                                 ((##core#unbox (##core#ref c252 (1)) ())
                                                                                                  k130
                                                                                                  row13
                                                                                                  r154
                                                                                                  r158)))))))))))
                                                                             ok?4
                                                                             '#<lambda info (ok? row13 dist14 placed15)#>))))
                                                               (let ((k188 (##core#closure
                                                                             (3)
                                                                             (##core#lambda
                                                                               (c260 r189)
                                                                               ((##core#unbox (##core#ref c260 (2)) ())
                                                                                (##core#ref c260 (1))
                                                                                r189
                                                                                '()
                                                                                '()))
                                                                             k37
                                                                             try3)))
                                                                 (let ((loop6261 (##core#undefined)))
                                                                   (let ((loop6 (##core#box loop6261)))
                                                                     (let ((t44 (##core#updatebox
                                                                                  loop6
                                                                                  (##core#closure
                                                                                    (3)
                                                                                    (lambda (c263 k46 i7 l8)
                                                                                      (if (##core#inline "C_i_nequalp" i7 '0)
                                                                                        (k46 l8)
                                                                                        (let ((r58 (##core#inline_allocate ("C_a_i_minus" 4) i7 '1)))
                                                                                          (let ((r62 (##core#inline_allocate ("C_a_i_cons" 3) i7 l8)))
                                                                                            ((##core#unbox (##core#ref c263 (1)) ())
                                                                                             k46
                                                                                             r58
                                                                                             r62)))))
                                                                                    loop6
                                                                                    '#<lambda info (loop i7 l8)#>))))
                                                                       ((##core#unbox loop6 ()) k188 n1 '()))))))))))))
                                                 '#<lambda info (nqueens n1)#>))))
                                    (let ((k191 (##core#closure
                                                  (2)
                                                  (##core#lambda
                                                    (c267 r192)
                                                    (let ((k194 (##core#closure
                                                                  (2)
                                                                  (##core#lambda
                                                                    (c269 r195)
                                                                    ((##core#ref c269 (1)) (##core#undefined)))
                                                                  (##core#ref c267 (1)))))
                                                      (let ((k197 (##core#closure
                                                                    (2)
                                                                    (##core#lambda
                                                                      (c271 r198)
                                                                      (r198 (##core#ref c271 (1))))
                                                                    k194)))
                                                        (##sys#implicit-exit-handler k197))))
                                                  (##core#ref c228 (1)))))
                                      (let ((k201 (##core#closure
                                                    (2)
                                                    (##core#lambda
                                                      (c273 r202)
                                                      (write (##core#ref c273 (1)) r202))
                                                    k191)))
                                        (nqueens k201 '8)))))
                                (##core#ref c226 (1)))))
                     (##core#callunit "eval" k32)))
                 k28)))
      (##core#callunit "library" k29)))
  '#<lambda info (toplevel)#>)

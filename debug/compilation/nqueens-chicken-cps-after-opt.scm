[optimized]
(lambda (k28)
  (let ((k29 (##core#lambda
               (r30)
               (let ((k32 (##core#lambda
                            (r33)
                            (let ((t35 (set! nqueens
                                         (lambda (k37 n1)
                                           (let ((try3 (##core#undefined)))
                                             (let ((ok?4 (##core#undefined)))
                                               (let ((t64 (set! try3
                                                            (lambda (k66 x10 y11 z12)
                                                              (if (##core#inline "C_i_nullp" x10)
                                                                (let ((r77 (##core#inline "C_i_nullp" y11)))
                                                                  (k66 (##core#cond r77 '1 '0)))
                                                                (let ((k83 (##core#lambda
                                                                             (r84)
                                                                             (let ((k87 (##core#lambda
                                                                                          (r88)
                                                                                          (k66 (##core#inline_allocate ("C_a_i_plus" 4) r84 r88)))))
                                                                               (let ((r92 (##core#inline "C_i_cdr" x10)))
                                                                                 (let ((r100 (##core#inline "C_i_car" x10)))
                                                                                   (let ((r96 (##core#inline_allocate
                                                                                                ("C_a_i_cons" 3)
                                                                                                r100
                                                                                                y11)))
                                                                                     (try3 k87 r92 r96 z12))))))))
                                                                  (let ((k102 (##core#lambda
                                                                                (r103)
                                                                                (if r103
                                                                                  (let ((k109 (##core#lambda
                                                                                                (r110)
                                                                                                (let ((r118 (##core#inline "C_i_car" x10)))
                                                                                                  (let ((r114 (##core#inline_allocate
                                                                                                                ("C_a_i_cons" 3)
                                                                                                                r118
                                                                                                                z12)))
                                                                                                    (try3 k83 r110 '() r114))))))
                                                                                    (let ((r122 (##core#inline "C_i_cdr" x10)))
                                                                                      (append k109 r122 y11)))
                                                                                  (k83 '0)))))
                                                                    (let ((r126 (##core#inline "C_i_car" x10)))
                                                                      (ok?4 k102 r126 '1 z12)))))))))
                                                 (let ((t128 (set! ok?4
                                                               (lambda (k130 row13 dist14 placed15)
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
                                                                                   (ok?4 k130 row13 r154 r158))))))))))))))
                                                   (let ((k188 (##core#lambda (r189) (try3 k37 r189 '() '()))))
                                                     (let ((loop6 (##core#undefined)))
                                                       (let ((t44 (set! loop6
                                                                    (lambda (k46 i7 l8)
                                                                      (if (##core#inline "C_i_nequalp" i7 '0)
                                                                        (k46 l8)
                                                                        (let ((r58 (##core#inline_allocate ("C_a_i_minus" 4) i7 '1)))
                                                                          (let ((r62 (##core#inline_allocate ("C_a_i_cons" 3) i7 l8)))
                                                                            (loop6 k46 r58 r62))))))))
                                                         (loop6 k188 n1 '()))))))))))))
                              (let ((k191 (##core#lambda
                                            (r192)
                                            (let ((k194 (##core#lambda (r195) (k28 (##core#undefined)))))
                                              (let ((k197 (##core#lambda (r198) (r198 k194))))
                                                (##sys#implicit-exit-handler k197))))))
                                (let ((k201 (##core#lambda (r202) (write k191 r202))))
                                  (nqueens k201 '8)))))))
                 (##core#callunit "eval" k32)))))
    (##core#callunit "library" k29)))

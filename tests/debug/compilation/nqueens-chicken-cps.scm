[cps]
(lambda (k28)
  (let ((k29 (##core#lambda
               (r30)
               (let ((t22 r30))
                 (let ((k32 (##core#lambda
                              (r33)
                              (let ((t23 r33))
                                (let ((t35 (set! nqueens
                                             (lambda (k37 n1)
                                               (let ((dec-to2 (##core#undefined)))
                                                 (let ((try3 (##core#undefined)))
                                                   (let ((ok?4 (##core#undefined)))
                                                     (let ((t38 (set! dec-to2
                                                                  (lambda (k40 n5)
                                                                    (let ((k41 (##core#lambda (r42) (k40 r42))))
                                                                      (let ((loop6 (##core#undefined)))
                                                                        (let ((t44 (set! loop6
                                                                                     (lambda (k46 i7 l8)
                                                                                       (let ((k47 (##core#lambda (r48) (k46 r48))))
                                                                                         (let ((k50 (##core#lambda
                                                                                                      (r51)
                                                                                                      (if r51
                                                                                                        (k47 l8)
                                                                                                        (let ((k53 (##core#lambda (r54) (k47 r54))))
                                                                                                          (let ((k57 (##core#lambda
                                                                                                                       (r58)
                                                                                                                       (let ((a56 r58))
                                                                                                                         (let ((k61 (##core#lambda
                                                                                                                                      (r62)
                                                                                                                                      (let ((a60 r62)) (loop6 k53 a56 a60)))))
                                                                                                                           (cons k61 i7 l8))))))
                                                                                                            (- k57 i7 '1)))))))
                                                                                           (= k50 i7 '0)))))))
                                                                          (let ((t9 t44)) (loop6 k41 n5 '())))))))))
                                                       (let ((t18 t38))
                                                         (let ((t64 (set! try3
                                                                      (lambda (k66 x10 y11 z12)
                                                                        (let ((k67 (##core#lambda (r68) (k66 r68))))
                                                                          (let ((k70 (##core#lambda
                                                                                       (r71)
                                                                                       (if r71
                                                                                         (let ((k73 (##core#lambda (r74) (k67 r74))))
                                                                                           (let ((k76 (##core#lambda (r77) (if r77 (k73 '1) (k73 '0)))))
                                                                                             (null? k76 y11)))
                                                                                         (let ((k79 (##core#lambda (r80) (k67 r80))))
                                                                                           (let ((k83 (##core#lambda
                                                                                                        (r84)
                                                                                                        (let ((a82 r84))
                                                                                                          (let ((k87 (##core#lambda
                                                                                                                       (r88)
                                                                                                                       (let ((a86 r88)) (+ k79 a82 a86)))))
                                                                                                            (let ((k91 (##core#lambda
                                                                                                                         (r92)
                                                                                                                         (let ((a90 r92))
                                                                                                                           (let ((k95 (##core#lambda
                                                                                                                                        (r96)
                                                                                                                                        (let ((a94 r96)) (try3 k87 a90 a94 z12)))))
                                                                                                                             (let ((k99 (##core#lambda
                                                                                                                                          (r100)
                                                                                                                                          (let ((a98 r100)) (cons k95 a98 y11)))))
                                                                                                                               (car k99 x10)))))))
                                                                                                              (cdr k91 x10)))))))
                                                                                             (let ((k102 (##core#lambda
                                                                                                           (r103)
                                                                                                           (if r103
                                                                                                             (let ((k105 (##core#lambda (r106) (k83 r106))))
                                                                                                               (let ((k109 (##core#lambda
                                                                                                                             (r110)
                                                                                                                             (let ((a108 r110))
                                                                                                                               (let ((k113 (##core#lambda
                                                                                                                                             (r114)
                                                                                                                                             (let ((a112 r114)) (try3 k105 a108 '() a112)))))
                                                                                                                                 (let ((k117 (##core#lambda
                                                                                                                                               (r118)
                                                                                                                                               (let ((a116 r118)) (cons k113 a116 z12)))))
                                                                                                                                   (car k117 x10)))))))
                                                                                                                 (let ((k121 (##core#lambda
                                                                                                                               (r122)
                                                                                                                               (let ((a120 r122)) (append k109 a120 y11)))))
                                                                                                                   (cdr k121 x10))))
                                                                                                             (k83 '0)))))
                                                                                               (let ((k125 (##core#lambda
                                                                                                             (r126)
                                                                                                             (let ((a124 r126)) (ok?4 k102 a124 '1 z12)))))
                                                                                                 (car k125 x10)))))))))
                                                                            (null? k70 x10)))))))
                                                           (let ((t19 t64))
                                                             (let ((t128 (set! ok?4
                                                                           (lambda (k130 row13 dist14 placed15)
                                                                             (let ((k131 (##core#lambda (r132) (k130 r132))))
                                                                               (let ((k134 (##core#lambda
                                                                                             (r135)
                                                                                             (if r135
                                                                                               (k131 '#t)
                                                                                               (let ((k137 (##core#lambda (r138) (k131 r138))))
                                                                                                 (let ((k140 (##core#lambda
                                                                                                               (r141)
                                                                                                               (if r141
                                                                                                                 (let ((k143 (##core#lambda (r144) (k137 r144))))
                                                                                                                   (let ((k146 (##core#lambda
                                                                                                                                 (r147)
                                                                                                                                 (if r147
                                                                                                                                   (let ((k149 (##core#lambda (r150) (k143 r150))))
                                                                                                                                     (let ((k153 (##core#lambda
                                                                                                                                                   (r154)
                                                                                                                                                   (let ((a152 r154))
                                                                                                                                                     (let ((k157 (##core#lambda
                                                                                                                                                                   (r158)
                                                                                                                                                                   (let ((a156 r158)) (ok?4 k149 row13 a152 a156)))))
                                                                                                                                                       (cdr k157 placed15))))))
                                                                                                                                       (+ k153 dist14 '1)))
                                                                                                                                   (k143 '#f)))))
                                                                                                                     (let ((k161 (##core#lambda
                                                                                                                                   (r162)
                                                                                                                                   (let ((a160 r162)) (not k146 a160)))))
                                                                                                                       (let ((k165 (##core#lambda
                                                                                                                                     (r166)
                                                                                                                                     (let ((a164 r166))
                                                                                                                                       (let ((k169 (##core#lambda
                                                                                                                                                     (r170)
                                                                                                                                                     (let ((a168 r170)) (= k161 a164 a168)))))
                                                                                                                                         (- k169 row13 dist14))))))
                                                                                                                         (car k165 placed15)))))
                                                                                                                 (k137 '#f)))))
                                                                                                   (let ((k173 (##core#lambda
                                                                                                                 (r174)
                                                                                                                 (let ((a172 r174)) (not k140 a172)))))
                                                                                                     (let ((k177 (##core#lambda
                                                                                                                   (r178)
                                                                                                                   (let ((a176 r178))
                                                                                                                     (let ((k181 (##core#lambda
                                                                                                                                   (r182)
                                                                                                                                   (let ((a180 r182)) (= k173 a176 a180)))))
                                                                                                                       (+ k181 row13 dist14))))))
                                                                                                       (car k177 placed15)))))))))
                                                                                 (null? k134 placed15)))))))
                                                               (let ((t20 t128))
                                                                 (let ((k184 (##core#lambda (r185) (k37 r185))))
                                                                   (let ((k188 (##core#lambda
                                                                                 (r189)
                                                                                 (let ((a187 r189)) (try3 k184 a187 '() '())))))
                                                                     (dec-to2 k188 n1))))))))))))))))
                                  (let ((t24 t35))
                                    (let ((k191 (##core#lambda
                                                  (r192)
                                                  (let ((t25 r192))
                                                    (let ((k194 (##core#lambda
                                                                  (r195)
                                                                  (let ((t26 r195)) (k28 (##core#undefined))))))
                                                      (let ((k197 (##core#lambda (r198) (r198 k194))))
                                                        (##sys#implicit-exit-handler k197)))))))
                                      (let ((k201 (##core#lambda
                                                    (r202)
                                                    (let ((a200 r202)) (write k191 a200)))))
                                        (nqueens k201 '8)))))))))
                   (##core#callunit "eval" k32))))))
    (##core#callunit "library" k29)))

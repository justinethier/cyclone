
                      ((%closure
                         (lambda-5
                           (self$42 loop$8$19)
                           ((%closure
                              (lambda-7
                                (self$43 loop$8$19)
                                (Cyc-seq
                                  (set-cell!
                                    loop$8$19
                                    (%closure
                                      (lambda-6-cont
                                        (self$44 k$28 x$9$20)
                                        (if (zero?__inline__ x$9$20)
                                          ((%closure-ref write 0)
                                           write
                                           k$28
                                           'done)
                                          ((%closure-ref
                                             (cell-get (%closure-ref self$44 1))
                                             0)
                                           (cell-get (%closure-ref self$44 1))
                                           k$28
                                           (Cyc-fast-sub x$9$20 1))))
                                      loop$8$19))
                                  ((%closure-ref (cell-get loop$8$19) 0)
                                   (cell-get loop$8$19)
                                   (%closure-ref self$43 1)
                                   10)))
                              (%closure-ref self$42 1))
                            (cell loop$8$19)))
                         (%closure-ref self$41 1))
                       #f)

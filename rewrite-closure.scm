;; TODO: temporary test file
;;
;; goal is for closure conversion to (after CC converting the code) detect cases like the below, with a loop.
;; we then need to rewrite the code to extract the set-cell operation out into an assignment operation via
;; (cell). We will need to replace when the relocated closure assigns the loop variable, and instead will 
;; want to use a special symbol such as "%closure-self-ref" to indicate to cgen that we want to assign
;; the closure to itself (so it can call itself later).
;;
;; See disabled code in (analyze-mutable-variables) for how we detect loops now in wrap-mutables.
;; This will need to be ported to closure-convert.

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

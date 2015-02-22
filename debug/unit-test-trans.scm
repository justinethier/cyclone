;;
;; A test framework to attempt to make it easier to debug generated programs.
;; The idea is to allow execution of Scheme code that has been transformed
;; using cyclone's source-to-source transformations. If the code executes
;; OK here, then it should execute fine after being transformed into C code.
;; Unless of course there is a bug here (hopefully not) or in the Scheme->C
;; compiler.
;;

;; Return a function that can be called directly to 
;; invoke the closure, or indirectly to access closure
;; elements.
;;
;; When called directly, the first arg is the closure
;; itself (self), followed by args passed when the 
;; closure was defined.
(define (%closure . clo-args)
  (define clo-data (list->vector clo-args))
  (define clo
    (lambda args
      (cond 
        ((and (> (length args) 1)
              (equal? 'ref (car args)))
         (vector-ref clo-data (cadr args)))
        (else
         (apply 
           (car clo-args) 
           (cons clo args))))))
   clo)

(define (%closure-ref clo idx)
  (clo 'ref idx))
(define (%halt x)
  (exit))

;; Test code from matt might, may need to tweak per corresponding
;; functionality in the MTA C runtime
;; Suitable definitions for the cell functions:
(define (cell value) (lambda (get? new-value) 
                       (if get? value (set! value new-value))))
(define (set-cell! c v) (c #f v))
(define (cell-get c) (c #t #t))
;; END matt might

(define (test-fac)
((lambda (fac)
   ((lambda (fac)
      ((%closure
         (lambda (self$698 r$689)
           ((%closure
              (lambda (self$699 r$687)
                ((%closure
                   (lambda (self$700 $_$684)
                     ((%closure-ref (cell-get (%closure-ref self$700 1)) 0)
                      (cell-get (%closure-ref self$700 1))
                      (%closure
                        (lambda (self$701 r$688)
                          ((lambda (r$686) (%halt r$686)) (display r$688))))
                      10))
                   (%closure-ref self$699 1))
                 r$687))
              (%closure-ref self$698 1))
            (set-cell! (%closure-ref self$698 1) r$689)))
         fac)
       (%closure
         (lambda (self$694 k$690 n$685)
           ((%closure
              (lambda (self$695 r$691)
                (if r$691
                  ((%closure-ref (%closure-ref self$695 2) 0)
                   (%closure-ref self$695 2)
                   1)
                  ((%closure
                     (lambda (self$696 r$693)
                       ((%closure-ref (cell-get (%closure-ref self$696 1)) 0)
                        (cell-get (%closure-ref self$696 1))
                        (%closure
                          (lambda (self$697 r$692)
                            ((%closure-ref (%closure-ref self$697 1) 0)
                             (%closure-ref self$697 1)
                             (* (%closure-ref self$697 2) r$692)))
                          (%closure-ref self$696 2)
                          (%closure-ref self$696 3))
                        r$693))
                     (%closure-ref self$695 1)
                     (%closure-ref self$695 2)
                     (%closure-ref self$695 3))
                   (- (%closure-ref self$695 3) 1))))
              (%closure-ref self$694 1)
              k$690
              n$685)
            (= 0 n$685)))
         fac)))
    (cell fac)))
 #f))
;(test-fac)

(define (test-set)
  ((lambda (x$684)
     ((lambda (x$684)
        ((%closure
           (lambda (self$687 r$686)
             ((lambda (r$685) (%halt r$685))
              (display (cell-get (%closure-ref self$687 1)))))
           x$684)
         (set-cell! x$684 #t)))
      (cell x$684)))
   #f))
;(test-set)

(define (test-adder)
    ((lambda (increment make-adder)
       ((%closure
          (lambda (self$696 increment)
            ((%closure
               (lambda (self$697 make-adder)
                 ((%closure
                    (lambda (self$700 r$693)
                      ((%closure
                         (lambda (self$701 r$689)
                           ((%closure
                              (lambda (self$702 $_$684)
                                ((%closure-ref
                                   (cell-get (%closure-ref self$702 2))
                                   0)
                                 (cell-get (%closure-ref self$702 2))
                                 (%closure
                                   (lambda (self$703 r$692)
                                     ((%closure
                                        (lambda (self$704 r$690)
                                          ((%closure
                                             (lambda (self$705 $_$685)
                                               ((%closure-ref
                                                  (cell-get (%closure-ref self$705 1))
                                                  0)
                                                (cell-get (%closure-ref self$705 1))
                                                (%closure
                                                  (lambda (self$706 r$691)
                                                    ((lambda (r$688) (%halt r$688)) (display r$691))))
                                                41))
                                             (%closure-ref self$704 1))
                                           r$690))
                                        (%closure-ref self$703 1))
                                      (set-cell! (%closure-ref self$703 1) r$692)))
                                   (%closure-ref self$702 1))
                                 1))
                              (%closure-ref self$701 1)
                              (%closure-ref self$701 2))
                            r$689))
                         (%closure-ref self$700 1)
                         (%closure-ref self$700 2))
                       (set-cell! (%closure-ref self$700 2) r$693)))
                    (%closure-ref self$697 1)
                    make-adder)
                  (%closure
                    (lambda (self$698 k$694 x$686)
                      ((%closure-ref k$694 0)
                       k$694
                       (%closure
                         (lambda (self$699 k$695 y$687)
                           ((%closure-ref k$695 0)
                            k$695
                            (+ (%closure-ref self$699 1) y$687)))
                         x$686))))))
               increment)
             (cell (%closure-ref self$696 1))))
          make-adder)
        (cell increment)))
     #f
     #f))
;(test-adder)

;; Transformed scheme code from if.scm
(define (test-if)
    ((lambda (k$699) (if #t (k$699 1) (k$699 2)))
     (lambda (r$698)
       ((lambda (r$689)
          ((lambda ($_$684)
             ((lambda (k$697) (if #f (k$697 1) (k$697 2)))
              (lambda (r$696)
                ((lambda (r$690)
                   ((lambda ($_$685)
                      ((lambda (k$694)
                         ((lambda (r$695)
                            (if r$695 (k$694 (+ 3 4)) (k$694 (* 3 4))))
                          (+ 1 2)))
                       (lambda (r$691)
                         ((lambda ($_$686)
                            ((lambda (k$692)
                               ((lambda (x$687)
                                  ((lambda (r$693)
                                     (if r$693 (k$692 (+ 1 1)) (k$692 (* 0 0))))
                                   (+ x$687 1)))
                                0))
                             (lambda (r$688) (%halt r$688))))
                          r$691))))
                    r$690))
                 (display r$696)))))
           r$689))
        (display r$698)))))

(define (test-eval)
((lambda (analyze$737
          analyze-quoted$738
          analyze-self-evaluating$739
          env$740
          eval$741
          exp$742
          quoted?$743
          self-evaluating?$744
          tag$745
          tagged-list?$746)
   ((%closure
      (lambda (self$803 analyze$737)
        ((%closure
           (lambda (self$804 analyze-quoted$738)
             ((%closure
                (lambda (self$805 analyze-self-evaluating$739)
                  ((%closure
                     (lambda (self$806 eval$741)
                       ((%closure
                          (lambda (self$807 quoted?$743)
                            ((%closure
                               (lambda (self$808 self-evaluating?$744)
                                 ((%closure
                                    (lambda (self$809 tagged-list?$746)
                                      ((%closure
                                         (lambda (self$812 r$800)
                                           ((%closure
                                              (lambda (self$813 r$768)
                                                ((%closure
                                                   (lambda (self$814 $_$747)
                                                     ((%closure
                                                        (lambda (self$818 r$796)
                                                          ((%closure
                                                             (lambda (self$819 r$769)
                                                               ((%closure
                                                                  (lambda (self$820 $_$748)
                                                                    ((%closure
                                                                       (lambda (self$823 r$793)
                                                                         ((%closure
                                                                            (lambda (self$824 r$770)
                                                                              ((%closure
                                                                                 (lambda (self$825 $_$749)
                                                                                   ((%closure
                                                                                      (lambda (self$828 r$790)
                                                                                        ((%closure
                                                                                           (lambda (self$829 r$771)
                                                                                             ((%closure
                                                                                                (lambda (self$830 $_$750)
                                                                                                  ((%closure
                                                                                                     (lambda (self$834 r$786)
                                                                                                       ((%closure
                                                                                                          (lambda (self$835 r$772)
                                                                                                            ((%closure
                                                                                                               (lambda (self$836 $_$751)
                                                                                                                 ((%closure
                                                                                                                    (lambda (self$839 r$783)
                                                                                                                      ((%closure
                                                                                                                         (lambda (self$840 r$773)
                                                                                                                           ((%closure
                                                                                                                              (lambda (self$841 $_$752)
                                                                                                                                ((%closure
                                                                                                                                   (lambda (self$846 r$779)
                                                                                                                                     ((%closure
                                                                                                                                        (lambda (self$847 r$774)
                                                                                                                                          ((%closure
                                                                                                                                             (lambda (self$848 $_$753)
                                                                                                                                               ((%closure-ref
                                                                                                                                                  (cell-get (%closure-ref self$848 1))
                                                                                                                                                  0)
                                                                                                                                                (cell-get (%closure-ref self$848 1))
                                                                                                                                                (%closure
                                                                                                                                                  (lambda (self$849 r$778)
                                                                                                                                                    ((%closure
                                                                                                                                                       (lambda (self$850 r$775)
                                                                                                                                                         ((%closure
                                                                                                                                                            (lambda (self$851 $_$754)
                                                                                                                                                              ((%closure
                                                                                                                                                                 (lambda (self$852 r$777)
                                                                                                                                                                   ((%closure-ref
                                                                                                                                                                      (cell-get (%closure-ref self$852 1))
                                                                                                                                                                      0)
                                                                                                                                                                    (cell-get (%closure-ref self$852 1))
                                                                                                                                                                    (%closure
                                                                                                                                                                      (lambda (self$853 r$776)
                                                                                                                                                                        ((lambda (r$767) (%halt r$767)) (write r$776))))
                                                                                                                                                                    r$777
                                                                                                                                                                    #f))
                                                                                                                                                                 (%closure-ref self$851 1))
                                                                                                                                                               '(1 . 2)))
                                                                                                                                                            (%closure-ref self$850 1))
                                                                                                                                                          r$775))
                                                                                                                                                       (%closure-ref self$849 1))
                                                                                                                                                     (write r$778)))
                                                                                                                                                  (%closure-ref self$848 1))
                                                                                                                                                2
                                                                                                                                                #f))
                                                                                                                                             (%closure-ref self$847 1))
                                                                                                                                           r$774))
                                                                                                                                        (%closure-ref self$846 2))
                                                                                                                                      (set-cell! (%closure-ref self$846 1) r$779)))
                                                                                                                                   (%closure-ref self$841 1)
                                                                                                                                   (%closure-ref self$841 2))
                                                                                                                                 (%closure
                                                                                                                                   (lambda (self$842 k$780 exp$755)
                                                                                                                                     ((%closure
                                                                                                                                        (lambda (self$843 r$781)
                                                                                                                                          ((%closure
                                                                                                                                             (lambda (self$844 qval$756)
                                                                                                                                               ((%closure-ref (%closure-ref self$844 1) 0)
                                                                                                                                                (%closure-ref self$844 1)
                                                                                                                                                (%closure
                                                                                                                                                  (lambda (self$845 k$782 env$757)
                                                                                                                                                    ((%closure-ref k$782 0)
                                                                                                                                                     k$782
                                                                                                                                                     (%closure-ref self$845 1)))
                                                                                                                                                  qval$756)))
                                                                                                                                             (%closure-ref self$843 1))
                                                                                                                                           r$781))
                                                                                                                                        k$780)
                                                                                                                                      (cadr exp$755))))))
                                                                                                                              (%closure-ref self$840 1)
                                                                                                                              (%closure-ref self$840 2))
                                                                                                                            r$773))
                                                                                                                         (%closure-ref self$839 1)
                                                                                                                         (%closure-ref self$839 3))
                                                                                                                       (set-cell! (%closure-ref self$839 2) r$783)))
                                                                                                                    (%closure-ref self$836 1)
                                                                                                                    (%closure-ref self$836 2)
                                                                                                                    (%closure-ref self$836 3))
                                                                                                                  (%closure
                                                                                                                    (lambda (self$837 k$784 exp$758)
                                                                                                                      ((%closure-ref k$784 0)
                                                                                                                       k$784
                                                                                                                       (%closure
                                                                                                                         (lambda (self$838 k$785 env$759)
                                                                                                                           ((%closure-ref k$785 0)
                                                                                                                            k$785
                                                                                                                            (%closure-ref self$838 1)))
                                                                                                                         exp$758))))))
                                                                                                               (%closure-ref self$835 1)
                                                                                                               (%closure-ref self$835 2)
                                                                                                               (%closure-ref self$835 3))
                                                                                                             r$772))
                                                                                                          (%closure-ref self$834 2)
                                                                                                          (%closure-ref self$834 3)
                                                                                                          (%closure-ref self$834 4))
                                                                                                        (set-cell! (%closure-ref self$834 1) r$786)))
                                                                                                     (%closure-ref self$830 1)
                                                                                                     (%closure-ref self$830 2)
                                                                                                     (%closure-ref self$830 3)
                                                                                                     (%closure-ref self$830 4))
                                                                                                   (%closure
                                                                                                     (lambda (self$831 k$787 exp$760)
                                                                                                       ((%closure-ref
                                                                                                          (cell-get (%closure-ref self$831 4))
                                                                                                          0)
                                                                                                        (cell-get (%closure-ref self$831 4))
                                                                                                        (%closure
                                                                                                          (lambda (self$832 r$788)
                                                                                                            (if r$788
                                                                                                              ((%closure-ref
                                                                                                                 (cell-get (%closure-ref self$832 2))
                                                                                                                 0)
                                                                                                               (cell-get (%closure-ref self$832 2))
                                                                                                               (%closure-ref self$832 4)
                                                                                                               (%closure-ref self$832 3))
                                                                                                              ((%closure-ref
                                                                                                                 (cell-get (%closure-ref self$832 5))
                                                                                                                 0)
                                                                                                               (cell-get (%closure-ref self$832 5))
                                                                                                               (%closure
                                                                                                                 (lambda (self$833 r$789)
                                                                                                                   (if r$789
                                                                                                                     ((%closure-ref
                                                                                                                        (cell-get (%closure-ref self$833 1))
                                                                                                                        0)
                                                                                                                      (cell-get (%closure-ref self$833 1))
                                                                                                                      (%closure-ref self$833 3)
                                                                                                                      (%closure-ref self$833 2))
                                                                                                                     ((%closure-ref (%closure-ref self$833 3) 0)
                                                                                                                      (%closure-ref self$833 3)
                                                                                                                      #f)))
                                                                                                                 (%closure-ref self$832 1)
                                                                                                                 (%closure-ref self$832 3)
                                                                                                                 (%closure-ref self$832 4))
                                                                                                               (%closure-ref self$832 3))))
                                                                                                          (%closure-ref self$831 1)
                                                                                                          (%closure-ref self$831 2)
                                                                                                          exp$760
                                                                                                          k$787
                                                                                                          (%closure-ref self$831 3))
                                                                                                        exp$760))
                                                                                                     (%closure-ref self$830 2)
                                                                                                     (%closure-ref self$830 3)
                                                                                                     (%closure-ref self$830 5)
                                                                                                     (%closure-ref self$830 6))))
                                                                                                (%closure-ref self$829 1)
                                                                                                (%closure-ref self$829 2)
                                                                                                (%closure-ref self$829 3)
                                                                                                (%closure-ref self$829 4)
                                                                                                (%closure-ref self$829 5)
                                                                                                (%closure-ref self$829 6))
                                                                                              r$771))
                                                                                           (%closure-ref self$828 1)
                                                                                           (%closure-ref self$828 2)
                                                                                           (%closure-ref self$828 3)
                                                                                           (%closure-ref self$828 4)
                                                                                           (%closure-ref self$828 5)
                                                                                           (%closure-ref self$828 6))
                                                                                         (set-cell! (%closure-ref self$828 5) r$790)))
                                                                                      (%closure-ref self$825 1)
                                                                                      (%closure-ref self$825 2)
                                                                                      (%closure-ref self$825 3)
                                                                                      (%closure-ref self$825 4)
                                                                                      (%closure-ref self$825 5)
                                                                                      (%closure-ref self$825 6))
                                                                                    (%closure
                                                                                      (lambda (self$826 k$791 exp$761)
                                                                                        ((%closure
                                                                                           (lambda (self$827 r$792)
                                                                                             ((%closure-ref
                                                                                                (cell-get (%closure-ref self$827 3))
                                                                                                0)
                                                                                              (cell-get (%closure-ref self$827 3))
                                                                                              (%closure-ref self$827 2)
                                                                                              (%closure-ref self$827 1)
                                                                                              r$792))
                                                                                           exp$761
                                                                                           k$791
                                                                                           (%closure-ref self$826 1))
                                                                                         'quote))
                                                                                      (%closure-ref self$825 7))))
                                                                                 (%closure-ref self$824 1)
                                                                                 (%closure-ref self$824 2)
                                                                                 (%closure-ref self$824 3)
                                                                                 (%closure-ref self$824 4)
                                                                                 (%closure-ref self$824 5)
                                                                                 (%closure-ref self$824 6)
                                                                                 (%closure-ref self$824 7))
                                                                               r$770))
                                                                            (%closure-ref self$823 1)
                                                                            (%closure-ref self$823 2)
                                                                            (%closure-ref self$823 3)
                                                                            (%closure-ref self$823 4)
                                                                            (%closure-ref self$823 5)
                                                                            (%closure-ref self$823 6)
                                                                            (%closure-ref self$823 7))
                                                                          (set-cell! (%closure-ref self$823 6) r$793)))
                                                                       (%closure-ref self$820 1)
                                                                       (%closure-ref self$820 2)
                                                                       (%closure-ref self$820 3)
                                                                       (%closure-ref self$820 4)
                                                                       (%closure-ref self$820 5)
                                                                       (%closure-ref self$820 6)
                                                                       (%closure-ref self$820 7))
                                                                     (%closure
                                                                       (lambda (self$821 k$794 exp$762)
                                                                         ((%closure
                                                                            (lambda (self$822 r$795)
                                                                              (if r$795
                                                                                ((%closure-ref (%closure-ref self$822 1) 0)
                                                                                 (%closure-ref self$822 1)
                                                                                 #t)
                                                                                ((%closure-ref (%closure-ref self$822 1) 0)
                                                                                 (%closure-ref self$822 1)
                                                                                 #f)))
                                                                            k$794)
                                                                          (number? exp$762))))))
                                                                  (%closure-ref self$819 1)
                                                                  (%closure-ref self$819 2)
                                                                  (%closure-ref self$819 3)
                                                                  (%closure-ref self$819 4)
                                                                  (%closure-ref self$819 5)
                                                                  (%closure-ref self$819 6)
                                                                  (%closure-ref self$819 7))
                                                                r$769))
                                                             (%closure-ref self$818 1)
                                                             (%closure-ref self$818 2)
                                                             (%closure-ref self$818 3)
                                                             (%closure-ref self$818 4)
                                                             (%closure-ref self$818 5)
                                                             (%closure-ref self$818 6)
                                                             (%closure-ref self$818 7))
                                                           (set-cell! (%closure-ref self$818 7) r$796)))
                                                        (%closure-ref self$814 1)
                                                        (%closure-ref self$814 2)
                                                        (%closure-ref self$814 3)
                                                        (%closure-ref self$814 4)
                                                        (%closure-ref self$814 5)
                                                        (%closure-ref self$814 6)
                                                        (%closure-ref self$814 7))
                                                      (%closure
                                                        (lambda (self$815 k$797 exp$763 tag$764)
                                                          ((%closure
                                                             (lambda (self$816 r$798)
                                                               (if r$798
                                                                 ((%closure
                                                                    (lambda (self$817 r$799)
                                                                      ((%closure-ref (%closure-ref self$817 1) 0)
                                                                       (%closure-ref self$817 1)
                                                                       (equal? r$799 (%closure-ref self$817 2))))
                                                                    (%closure-ref self$816 2)
                                                                    (%closure-ref self$816 3))
                                                                  (car (%closure-ref self$816 1)))
                                                                 ((%closure-ref (%closure-ref self$816 2) 0)
                                                                  (%closure-ref self$816 2)
                                                                  #f)))
                                                             exp$763
                                                             k$797
                                                             tag$764)
                                                           (pair? exp$763))))))
                                                   (%closure-ref self$813 1)
                                                   (%closure-ref self$813 2)
                                                   (%closure-ref self$813 3)
                                                   (%closure-ref self$813 4)
                                                   (%closure-ref self$813 5)
                                                   (%closure-ref self$813 6)
                                                   (%closure-ref self$813 7))
                                                 r$768))
                                              (%closure-ref self$812 1)
                                              (%closure-ref self$812 2)
                                              (%closure-ref self$812 3)
                                              (%closure-ref self$812 4)
                                              (%closure-ref self$812 5)
                                              (%closure-ref self$812 6)
                                              (%closure-ref self$812 7))
                                            (set-cell! (%closure-ref self$812 4) r$800)))
                                         (%closure-ref self$809 1)
                                         (%closure-ref self$809 2)
                                         (%closure-ref self$809 3)
                                         (%closure-ref self$809 4)
                                         (%closure-ref self$809 5)
                                         (%closure-ref self$809 6)
                                         tagged-list?$746)
                                       (%closure
                                         (lambda (self$810 k$801 exp$765 env$766)
                                           ((%closure-ref
                                              (cell-get (%closure-ref self$810 1))
                                              0)
                                            (cell-get (%closure-ref self$810 1))
                                            (%closure
                                              (lambda (self$811 r$802)
                                                ((%closure-ref r$802 0)
                                                 r$802
                                                 (%closure-ref self$811 2)
                                                 (%closure-ref self$811 1)))
                                              env$766
                                              k$801)
                                            exp$765))
                                         (%closure-ref self$809 1))))
                                    (%closure-ref self$808 1)
                                    (%closure-ref self$808 2)
                                    (%closure-ref self$808 3)
                                    (%closure-ref self$808 4)
                                    (%closure-ref self$808 5)
                                    self-evaluating?$744)
                                  (cell (%closure-ref self$808 6))))
                               (%closure-ref self$807 1)
                               (%closure-ref self$807 2)
                               (%closure-ref self$807 3)
                               (%closure-ref self$807 4)
                               quoted?$743
                               (%closure-ref self$807 6))
                             (cell (%closure-ref self$807 5))))
                          (%closure-ref self$806 1)
                          (%closure-ref self$806 2)
                          (%closure-ref self$806 3)
                          eval$741
                          (%closure-ref self$806 5)
                          (%closure-ref self$806 6))
                        (cell (%closure-ref self$806 4))))
                     (%closure-ref self$805 1)
                     (%closure-ref self$805 2)
                     analyze-self-evaluating$739
                     (%closure-ref self$805 4)
                     (%closure-ref self$805 5)
                     (%closure-ref self$805 6))
                   (cell (%closure-ref self$805 3))))
                (%closure-ref self$804 1)
                analyze-quoted$738
                (%closure-ref self$804 3)
                (%closure-ref self$804 4)
                (%closure-ref self$804 5)
                (%closure-ref self$804 6))
              (cell (%closure-ref self$804 2))))
           analyze$737
           (%closure-ref self$803 2)
           (%closure-ref self$803 3)
           (%closure-ref self$803 4)
           (%closure-ref self$803 5)
           (%closure-ref self$803 6))
         (cell (%closure-ref self$803 1))))
      analyze-quoted$738
      analyze-self-evaluating$739
      eval$741
      quoted?$743
      self-evaluating?$744
      tagged-list?$746)
    (cell analyze$737)))
 #f
 #f
 #f
 #f
 #f
 #f
 #f
 #f
 #f
 #f))
(test-eval)

; l21 line 1565 - passes #f to l43, which then tries to execute it:
; according to scaffolding though, this is a problem in the transformed Scheme code
;
;static void __lambda_21(object self_73833, object r_73789) {
;  if( !eq(quote_f,   r_73789) ){ 
;  return_funcall2(  cell_get(((closureN)self_73833)->elts[0]), ((closureN)self_73833)->elts[2], ((closureN)self_73833)->elts[1]);
;} else { 
;  return_funcall1(  ((closureN)self_73833)->elts[2], quote_f);}
;; 
;}
;static void __lambda_43(object self_73811, object r_73802) {
;  return_funcall2(  r_73802, ((closureN)self_73811)->elts[1], ((closureN)self_73811)->elts[0]);; 
;}



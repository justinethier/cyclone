;; sboyer cps conversion
((define main
   (lambda (k$645)
     (read (lambda (r$646)
             ((lambda (count$258)
                (read (lambda (r$647)
                        ((lambda (input$259)
                           (read (lambda (r$648)
                                   ((lambda (output$260)
                                      ((lambda (r$649)
                                         ((lambda (s2$261)
                                            ((lambda (r$650)
                                               ((lambda (s1$262)
                                                  ((lambda (name$263)
                                                     ((lambda ()
                                                        ((lambda (r$651)
                                                           ((lambda (r$652)
                                                              ((lambda (r$653)
                                                                 (run-r7rs-benchmark
                                                                   k$645
                                                                   r$651
                                                                   count$258
                                                                   r$652
                                                                   r$653))
                                                               (lambda (k$654 rewrites$264)
                                                                 ((lambda (r$655)
                                                                    (if r$655
                                                                      (k$654 (= rewrites$264 output$260))
                                                                      (k$654 #f)))
                                                                  (number? rewrites$264)))))
                                                            (lambda (k$656)
                                                              (setup-boyer
                                                                (lambda (r$657)
                                                                  (hide (lambda (r$658)
                                                                          (test-boyer k$656 alist term r$658))
                                                                        count$258
                                                                        input$259))))))
                                                         (string-append name$263 ":" s1$262 ":" s2$261)))))
                                                   "sboyer"))
                                                r$650))
                                             (number->string input$259)))
                                          r$649))
                                       (number->string count$258)))
                                    r$648))))
                         r$647))))
              r$646)))))
 (define alist #f)
 (define term #f)
 (define setup-boyer (lambda (k$638) (k$638 #t)))
 (define test-boyer (lambda (k$635) (k$635 #t)))
 (define hide
   (lambda (k$621 r$254 x$253)
     ((lambda (r$622)
        ((lambda (r$623) (call-with-values k$621 r$622 r$623))
         (lambda (k$624 v$256 i$255)
           ((lambda (r$625) (r$625 k$624 x$253)) (vector-ref v$256 i$255)))))
      (lambda (k$626)
        ((lambda (r$631)
           (vector
             (lambda (r$627)
               ((lambda (k$629)
                  ((lambda (r$630) (if r$630 (k$629 0) (k$629 1)))
                   (< r$254 100)))
                (lambda (r$628) (values k$626 r$627 r$628))))
             values
             r$631))
         (lambda (k$632 x$257) (k$632 x$257)))))))
 (define run-r7rs-benchmark
   (lambda (k$580 name$235 count$234 thunk$233 ok?$232)
     ((lambda (rounded$237)
        ((lambda (rounded$238)
           ((lambda (r$615)
              ((lambda (r$581)
                 (display
                   (lambda (r$582)
                     (display
                       (lambda (r$583)
                         (newline
                           (lambda (r$584)
                             (flush-output-port
                               (lambda (r$585)
                                 (jiffies-per-second
                                   (lambda (r$586)
                                     ((lambda (j/s$239)
                                        (current-second
                                          (lambda (r$587)
                                            ((lambda (t0$240)
                                               (current-jiffy
                                                 (lambda (r$588)
                                                   ((lambda (j0$241)
                                                      ((lambda ()
                                                         ((lambda (k$614) (if #f (k$614 #f) (k$614 #f)))
                                                          (lambda (r$589)
                                                            ((lambda (i$243 result$242)
                                                               ((lambda (loop$244)
                                                                  ((lambda (r$591)
                                                                     ((lambda (r$590)
                                                                        (loop$244 k$580 i$243 result$242))
                                                                      (set! loop$244 r$591)))
                                                                   (lambda (k$592 i$246 result$245)
                                                                     ((lambda (r$593)
                                                                        (if r$593
                                                                          ((lambda ()
                                                                             ((lambda (r$594)
                                                                                (thunk$233
                                                                                  (lambda (r$595) (loop$244 k$592 r$594 r$595))))
                                                                              (+ i$246 1))))
                                                                          (ok?$232
                                                                            (lambda (r$596)
                                                                              (if r$596
                                                                                ((lambda ()
                                                                                   (current-jiffy
                                                                                     (lambda (r$598)
                                                                                       ((lambda (j1$247)
                                                                                          (current-second
                                                                                            (lambda (r$599)
                                                                                              ((lambda (t1$248)
                                                                                                 ((lambda (r$600)
                                                                                                    ((lambda (jifs$249)
                                                                                                       ((lambda (r$610)
                                                                                                          (inexact
                                                                                                            (lambda (r$601)
                                                                                                              ((lambda (secs$250)
                                                                                                                 ((lambda (r$609)
                                                                                                                    (rounded$237
                                                                                                                      (lambda (r$602)
                                                                                                                        ((lambda (secs2$251)
                                                                                                                           ((lambda ()
                                                                                                                              (display
                                                                                                                                (lambda (r$603)
                                                                                                                                  (write (lambda (r$604)
                                                                                                                                           (display
                                                                                                                                             (lambda (r$605)
                                                                                                                                               (write (lambda (r$606)
                                                                                                                                                        (display
                                                                                                                                                          (lambda (r$607)
                                                                                                                                                            (display
                                                                                                                                                              (lambda (r$608)
                                                                                                                                                                (newline (lambda (r$597) (k$592 result$245))))
                                                                                                                                                              name$235))
                                                                                                                                                          ") for "))
                                                                                                                                                      secs2$251))
                                                                                                                                             " seconds ("))
                                                                                                                                         secs$250))
                                                                                                                                "Elapsed time: "))))
                                                                                                                         r$602))
                                                                                                                      r$609))
                                                                                                                  (- t1$248 t0$240)))
                                                                                                               r$601))
                                                                                                            r$610))
                                                                                                        (/ jifs$249 j/s$239)))
                                                                                                     r$600))
                                                                                                  (- j1$247 j0$241)))
                                                                                               r$599))))
                                                                                        r$598)))))
                                                                                ((lambda ()
                                                                                   (display
                                                                                     (lambda (r$611)
                                                                                       (write (lambda (r$612)
                                                                                                (newline (lambda (r$613) (k$592 result$245))))
                                                                                              result$245))
                                                                                     "ERROR: returned incorrect result: ")))))
                                                                            result$245)))
                                                                      (< i$246 count$234)))))
                                                                #f))
                                                             0
                                                             r$589))))))
                                                    r$588))))
                                             r$587))))
                                      r$586))))))))
                       name$235))
                   "Running "))
               (set! rounded$237 r$615)))
            (lambda (k$616 x$252)
              ((lambda (r$618)
                 (round (lambda (r$617) (k$616 (/ r$617 1000))) r$618))
               (* 1000 x$252)))))
         #f))
      #f)))
 ((lambda (*symbol-records-alist*$120
           add-lemma$119
           add-lemma-lst$118
           apply-subst$117
           apply-subst-lst$116
           false-term$115
           falsep$114
           get$113
           get-lemmas$112
           get-name$111
           if-constructor$110
           make-symbol-record$109
           one-way-unify$108
           one-way-unify1$107
           one-way-unify1-lst$106
           put$105
           put-lemmas!$104
           rewrite$103
           rewrite-args$102
           rewrite-count$101
           rewrite-with-lemmas$100
           scons$99
           setup$98
           symbol->symbol-record$97
           symbol-record-equal?$96
           tautologyp$95
           tautp$94
           term-args-equal?$93
           term-equal?$92
           term-member?$91
           test$90
           trans-of-implies$89
           trans-of-implies1$88
           translate-alist$87
           translate-args$86
           translate-term$85
           true-term$84
           truep$83
           unify-subst$82
           untranslate-term$81)
    ((lambda ()
       ((lambda (r$265)
          ((lambda (r$577)
             ((lambda (r$266)
                ((lambda (r$576)
                   ((lambda (r$267)
                      ((lambda ()
                         ((lambda (setup$160
                                   add-lemma-lst$159
                                   add-lemma$158
                                   translate-term$157
                                   translate-args$156
                                   untranslate-term$155
                                   put$154
                                   get$153
                                   symbol->symbol-record$152
                                   *symbol-records-alist*$151
                                   make-symbol-record$150
                                   put-lemmas!$149
                                   get-lemmas$148
                                   get-name$147
                                   symbol-record-equal?$146
                                   test$145
                                   translate-alist$144
                                   apply-subst$143
                                   apply-subst-lst$142
                                   tautp$141
                                   tautologyp$140
                                   if-constructor$139
                                   rewrite-count$138
                                   scons$137
                                   rewrite$136
                                   rewrite-args$135
                                   rewrite-with-lemmas$134
                                   unify-subst$133
                                   one-way-unify$132
                                   one-way-unify1$131
                                   one-way-unify1-lst$130
                                   falsep$129
                                   truep$128
                                   false-term$127
                                   true-term$126
                                   trans-of-implies$125
                                   trans-of-implies1$124
                                   term-equal?$123
                                   term-args-equal?$122
                                   term-member?$121)
                            ((lambda (r$573)
                               ((lambda (r$269)
                                  ((lambda (r$567)
                                     ((lambda (r$270)
                                        ((lambda (r$549)
                                           ((lambda (r$271)
                                              ((lambda (r$542)
                                                 ((lambda (r$272)
                                                    ((lambda (r$535)
                                                       ((lambda (r$273)
                                                          ((lambda (r$528)
                                                             ((lambda (r$274)
                                                                ((lambda (r$525)
                                                                   ((lambda (r$275)
                                                                      ((lambda (r$522)
                                                                         ((lambda (r$276)
                                                                            ((lambda (r$515)
                                                                               ((lambda (r$277)
                                                                                  ((lambda (r$514)
                                                                                     ((lambda (r$278)
                                                                                        ((lambda (r$511)
                                                                                           ((lambda (r$279)
                                                                                              ((lambda (r$509)
                                                                                                 ((lambda (r$280)
                                                                                                    ((lambda (r$507)
                                                                                                       ((lambda (r$281)
                                                                                                          ((lambda (r$505)
                                                                                                             ((lambda (r$282)
                                                                                                                ((lambda (r$503)
                                                                                                                   ((lambda (r$283)
                                                                                                                      ((lambda (r$489)
                                                                                                                         ((lambda (r$284)
                                                                                                                            ((lambda (r$480)
                                                                                                                               ((lambda (r$285)
                                                                                                                                  ((lambda (r$473)
                                                                                                                                     ((lambda (r$286)
                                                                                                                                        ((lambda (r$466)
                                                                                                                                           ((lambda (r$287)
                                                                                                                                              ((lambda (r$461)
                                                                                                                                                 ((lambda (r$288)
                                                                                                                                                    ((lambda (r$441)
                                                                                                                                                       ((lambda (r$289)
                                                                                                                                                          ((lambda (r$440)
                                                                                                                                                             ((lambda (r$290)
                                                                                                                                                                ((lambda (r$291)
                                                                                                                                                                   ((lambda (r$433)
                                                                                                                                                                      ((lambda (r$292)
                                                                                                                                                                         ((lambda (r$422)
                                                                                                                                                                            ((lambda (r$293)
                                                                                                                                                                               ((lambda (r$415)
                                                                                                                                                                                  ((lambda (r$294)
                                                                                                                                                                                     ((lambda (r$405)
                                                                                                                                                                                        ((lambda (r$295)
                                                                                                                                                                                           ((lambda (r$404)
                                                                                                                                                                                              ((lambda (r$296)
                                                                                                                                                                                                 ((lambda (r$400)
                                                                                                                                                                                                    ((lambda (r$297)
                                                                                                                                                                                                       ((lambda (r$385)
                                                                                                                                                                                                          ((lambda (r$298)
                                                                                                                                                                                                             ((lambda (r$376)
                                                                                                                                                                                                                ((lambda (r$299)
                                                                                                                                                                                                                   ((lambda (r$373)
                                                                                                                                                                                                                      ((lambda (r$300)
                                                                                                                                                                                                                         ((lambda (r$370)
                                                                                                                                                                                                                            ((lambda (r$301)
                                                                                                                                                                                                                               ((lambda (r$369)
                                                                                                                                                                                                                                  ((lambda (r$302)
                                                                                                                                                                                                                                     ((lambda (r$368)
                                                                                                                                                                                                                                        ((lambda (r$303)
                                                                                                                                                                                                                                           ((lambda (r$361)
                                                                                                                                                                                                                                              ((lambda (r$304)
                                                                                                                                                                                                                                                 ((lambda (r$351)
                                                                                                                                                                                                                                                    ((lambda (r$305)
                                                                                                                                                                                                                                                       ((lambda (r$342)
                                                                                                                                                                                                                                                          ((lambda (r$306)
                                                                                                                                                                                                                                                             ((lambda (r$333)
                                                                                                                                                                                                                                                                ((lambda (r$307)
                                                                                                                                                                                                                                                                   ((lambda (r$327)
                                                                                                                                                                                                                                                                      ((lambda (r$308)
                                                                                                                                                                                                                                                                         ((lambda (r$314)
                                                                                                                                                                                                                                                                            ((lambda (r$309)
                                                                                                                                                                                                                                                                               ((lambda (r$310)
                                                                                                                                                                                                                                                                                  ((lambda (r$268) (main %halt))
                                                                                                                                                                                                                                                                                   (set! test-boyer r$310)))
                                                                                                                                                                                                                                                                                (lambda (k$311 alist$163 term$162 n$161)
                                                                                                                                                                                                                                                                                  ((lambda (r$312)
                                                                                                                                                                                                                                                                                     (test$145
                                                                                                                                                                                                                                                                                       (lambda (r$313)
                                                                                                                                                                                                                                                                                         ((lambda (answer$164)
                                                                                                                                                                                                                                                                                            (if answer$164
                                                                                                                                                                                                                                                                                              (k$311 rewrite-count$138)
                                                                                                                                                                                                                                                                                              (k$311 #f)))
                                                                                                                                                                                                                                                                                          r$313))
                                                                                                                                                                                                                                                                                       alist$163
                                                                                                                                                                                                                                                                                       term$162
                                                                                                                                                                                                                                                                                       n$161))
                                                                                                                                                                                                                                                                                   (set! rewrite-count$138 0)))))
                                                                                                                                                                                                                                                                             (set! setup-boyer r$314)))
                                                                                                                                                                                                                                                                          (lambda (k$315)
                                                                                                                                                                                                                                                                            ((lambda (r$326)
                                                                                                                                                                                                                                                                               ((lambda (r$316)
                                                                                                                                                                                                                                                                                  ((lambda (r$325)
                                                                                                                                                                                                                                                                                     (symbol->symbol-record$152
                                                                                                                                                                                                                                                                                       (lambda (r$324)
                                                                                                                                                                                                                                                                                         ((lambda (r$317)
                                                                                                                                                                                                                                                                                            ((lambda (r$323)
                                                                                                                                                                                                                                                                                               (translate-term$157
                                                                                                                                                                                                                                                                                                 (lambda (r$322)
                                                                                                                                                                                                                                                                                                   ((lambda (r$318)
                                                                                                                                                                                                                                                                                                      ((lambda (r$321)
                                                                                                                                                                                                                                                                                                         (translate-term$157
                                                                                                                                                                                                                                                                                                           (lambda (r$320)
                                                                                                                                                                                                                                                                                                             ((lambda (r$319) (setup$160 k$315))
                                                                                                                                                                                                                                                                                                              (set! true-term$126 r$320)))
                                                                                                                                                                                                                                                                                                           r$321))
                                                                                                                                                                                                                                                                                                       '(t)))
                                                                                                                                                                                                                                                                                                    (set! false-term$127 r$322)))
                                                                                                                                                                                                                                                                                                 r$323))
                                                                                                                                                                                                                                                                                             '(f)))
                                                                                                                                                                                                                                                                                          (set! if-constructor$139 r$324)))
                                                                                                                                                                                                                                                                                       r$325))
                                                                                                                                                                                                                                                                                   'if))
                                                                                                                                                                                                                                                                                (set! *symbol-records-alist*$151 r$326)))
                                                                                                                                                                                                                                                                             '()))))
                                                                                                                                                                                                                                                                       (set! term-member?$121 r$327)))
                                                                                                                                                                                                                                                                    (lambda (k$328 x$166 lst$165)
                                                                                                                                                                                                                                                                      ((lambda (r$329)
                                                                                                                                                                                                                                                                         (if r$329
                                                                                                                                                                                                                                                                           ((lambda () (k$328 #f)))
                                                                                                                                                                                                                                                                           ((lambda (r$332)
                                                                                                                                                                                                                                                                              (term-equal?$123
                                                                                                                                                                                                                                                                                (lambda (r$330)
                                                                                                                                                                                                                                                                                  (if r$330
                                                                                                                                                                                                                                                                                    ((lambda () (k$328 #t)))
                                                                                                                                                                                                                                                                                    ((lambda ()
                                                                                                                                                                                                                                                                                       ((lambda (r$331)
                                                                                                                                                                                                                                                                                          (term-member?$121 k$328 x$166 r$331))
                                                                                                                                                                                                                                                                                        (cdr lst$165))))))
                                                                                                                                                                                                                                                                                x$166
                                                                                                                                                                                                                                                                                r$332))
                                                                                                                                                                                                                                                                            (car lst$165))))
                                                                                                                                                                                                                                                                       (null? lst$165)))))
                                                                                                                                                                                                                                                                 (set! term-args-equal?$122 r$333)))
                                                                                                                                                                                                                                                              (lambda (k$334 lst1$168 lst2$167)
                                                                                                                                                                                                                                                                ((lambda (r$335)
                                                                                                                                                                                                                                                                   (if r$335
                                                                                                                                                                                                                                                                     ((lambda () (k$334 (null? lst2$167))))
                                                                                                                                                                                                                                                                     ((lambda (r$336)
                                                                                                                                                                                                                                                                        (if r$336
                                                                                                                                                                                                                                                                          ((lambda () (k$334 #f)))
                                                                                                                                                                                                                                                                          ((lambda (r$340)
                                                                                                                                                                                                                                                                             ((lambda (r$341)
                                                                                                                                                                                                                                                                                (term-equal?$123
                                                                                                                                                                                                                                                                                  (lambda (r$337)
                                                                                                                                                                                                                                                                                    (if r$337
                                                                                                                                                                                                                                                                                      ((lambda ()
                                                                                                                                                                                                                                                                                         ((lambda (r$338)
                                                                                                                                                                                                                                                                                            ((lambda (r$339)
                                                                                                                                                                                                                                                                                               (term-args-equal?$122 k$334 r$338 r$339))
                                                                                                                                                                                                                                                                                             (cdr lst2$167)))
                                                                                                                                                                                                                                                                                          (cdr lst1$168))))
                                                                                                                                                                                                                                                                                      ((lambda () (k$334 #f)))))
                                                                                                                                                                                                                                                                                  r$340
                                                                                                                                                                                                                                                                                  r$341))
                                                                                                                                                                                                                                                                              (car lst2$167)))
                                                                                                                                                                                                                                                                           (car lst1$168))))
                                                                                                                                                                                                                                                                      (null? lst2$167))))
                                                                                                                                                                                                                                                                 (null? lst1$168)))))
                                                                                                                                                                                                                                                           (set! term-equal?$123 r$342)))
                                                                                                                                                                                                                                                        (lambda (k$343 x$170 y$169)
                                                                                                                                                                                                                                                          ((lambda (r$344)
                                                                                                                                                                                                                                                             (if r$344
                                                                                                                                                                                                                                                               ((lambda ()
                                                                                                                                                                                                                                                                  ((lambda (r$345)
                                                                                                                                                                                                                                                                     (if r$345
                                                                                                                                                                                                                                                                       ((lambda (r$349)
                                                                                                                                                                                                                                                                          ((lambda (r$350)
                                                                                                                                                                                                                                                                             (symbol-record-equal?$146
                                                                                                                                                                                                                                                                               (lambda (r$346)
                                                                                                                                                                                                                                                                                 (if r$346
                                                                                                                                                                                                                                                                                   ((lambda (r$347)
                                                                                                                                                                                                                                                                                      ((lambda (r$348)
                                                                                                                                                                                                                                                                                         (term-args-equal?$122 k$343 r$347 r$348))
                                                                                                                                                                                                                                                                                       (cdr y$169)))
                                                                                                                                                                                                                                                                                    (cdr x$170))
                                                                                                                                                                                                                                                                                   (k$343 #f)))
                                                                                                                                                                                                                                                                               r$349
                                                                                                                                                                                                                                                                               r$350))
                                                                                                                                                                                                                                                                           (car y$169)))
                                                                                                                                                                                                                                                                        (car x$170))
                                                                                                                                                                                                                                                                       (k$343 #f)))
                                                                                                                                                                                                                                                                   (pair? y$169))))
                                                                                                                                                                                                                                                               ((lambda () (k$343 (equal? x$170 y$169))))))
                                                                                                                                                                                                                                                           (pair? x$170)))))
                                                                                                                                                                                                                                                     (set! trans-of-implies1$124 r$351)))
                                                                                                                                                                                                                                                  (lambda (k$352 n$171)
                                                                                                                                                                                                                                                    ((lambda (r$353)
                                                                                                                                                                                                                                                       (if r$353
                                                                                                                                                                                                                                                         ((lambda ()
                                                                                                                                                                                                                                                            ((lambda (r$354) (list k$352 r$354 0 1))
                                                                                                                                                                                                                                                             'implies)))
                                                                                                                                                                                                                                                         ((lambda ()
                                                                                                                                                                                                                                                            ((lambda (r$355)
                                                                                                                                                                                                                                                               ((lambda (r$359)
                                                                                                                                                                                                                                                                  ((lambda (r$360)
                                                                                                                                                                                                                                                                     (list (lambda (r$356)
                                                                                                                                                                                                                                                                             ((lambda (r$358)
                                                                                                                                                                                                                                                                                (trans-of-implies1$124
                                                                                                                                                                                                                                                                                  (lambda (r$357) (list k$352 r$355 r$356 r$357))
                                                                                                                                                                                                                                                                                  r$358))
                                                                                                                                                                                                                                                                              (- n$171 1)))
                                                                                                                                                                                                                                                                           r$359
                                                                                                                                                                                                                                                                           r$360
                                                                                                                                                                                                                                                                           n$171))
                                                                                                                                                                                                                                                                   (- n$171 1)))
                                                                                                                                                                                                                                                                'implies))
                                                                                                                                                                                                                                                             'and)))))
                                                                                                                                                                                                                                                     (equal? n$171 1)))))
                                                                                                                                                                                                                                               (set! trans-of-implies$125 r$361)))
                                                                                                                                                                                                                                            (lambda (k$362 n$172)
                                                                                                                                                                                                                                              ((lambda (r$364)
                                                                                                                                                                                                                                                 (trans-of-implies1$124
                                                                                                                                                                                                                                                   (lambda (r$365)
                                                                                                                                                                                                                                                     ((lambda (r$367)
                                                                                                                                                                                                                                                        (list (lambda (r$366)
                                                                                                                                                                                                                                                                (list (lambda (r$363) (translate-term$157 k$362 r$363))
                                                                                                                                                                                                                                                                      r$364
                                                                                                                                                                                                                                                                      r$365
                                                                                                                                                                                                                                                                      r$366))
                                                                                                                                                                                                                                                              r$367
                                                                                                                                                                                                                                                              0
                                                                                                                                                                                                                                                              n$172))
                                                                                                                                                                                                                                                      'implies))
                                                                                                                                                                                                                                                   n$172))
                                                                                                                                                                                                                                               'implies))))
                                                                                                                                                                                                                                         (set! true-term$126 r$368)))
                                                                                                                                                                                                                                      '*))
                                                                                                                                                                                                                                   (set! false-term$127 r$369)))
                                                                                                                                                                                                                                '*))
                                                                                                                                                                                                                             (set! truep$128 r$370)))
                                                                                                                                                                                                                          (lambda (k$371 x$174 lst$173)
                                                                                                                                                                                                                            (term-equal?$123
                                                                                                                                                                                                                              (lambda (r$372)
                                                                                                                                                                                                                                ((lambda (tmp$175)
                                                                                                                                                                                                                                   (if tmp$175
                                                                                                                                                                                                                                     (k$371 tmp$175)
                                                                                                                                                                                                                                     (term-member?$121 k$371 x$174 lst$173)))
                                                                                                                                                                                                                                 r$372))
                                                                                                                                                                                                                              x$174
                                                                                                                                                                                                                              true-term$126))))
                                                                                                                                                                                                                       (set! falsep$129 r$373)))
                                                                                                                                                                                                                    (lambda (k$374 x$177 lst$176)
                                                                                                                                                                                                                      (term-equal?$123
                                                                                                                                                                                                                        (lambda (r$375)
                                                                                                                                                                                                                          ((lambda (tmp$178)
                                                                                                                                                                                                                             (if tmp$178
                                                                                                                                                                                                                               (k$374 tmp$178)
                                                                                                                                                                                                                               (term-member?$121 k$374 x$177 lst$176)))
                                                                                                                                                                                                                           r$375))
                                                                                                                                                                                                                        x$177
                                                                                                                                                                                                                        false-term$127))))
                                                                                                                                                                                                                 (set! one-way-unify1-lst$130 r$376)))
                                                                                                                                                                                                              (lambda (k$377 lst1$180 lst2$179)
                                                                                                                                                                                                                ((lambda (r$378)
                                                                                                                                                                                                                   (if r$378
                                                                                                                                                                                                                     ((lambda () (k$377 (null? lst2$179))))
                                                                                                                                                                                                                     ((lambda (r$379)
                                                                                                                                                                                                                        (if r$379
                                                                                                                                                                                                                          ((lambda () (k$377 #f)))
                                                                                                                                                                                                                          ((lambda (r$383)
                                                                                                                                                                                                                             ((lambda (r$384)
                                                                                                                                                                                                                                (one-way-unify1$131
                                                                                                                                                                                                                                  (lambda (r$380)
                                                                                                                                                                                                                                    (if r$380
                                                                                                                                                                                                                                      ((lambda ()
                                                                                                                                                                                                                                         ((lambda (r$381)
                                                                                                                                                                                                                                            ((lambda (r$382)
                                                                                                                                                                                                                                               (one-way-unify1-lst$130 k$377 r$381 r$382))
                                                                                                                                                                                                                                             (cdr lst2$179)))
                                                                                                                                                                                                                                          (cdr lst1$180))))
                                                                                                                                                                                                                                      ((lambda () (k$377 #f)))))
                                                                                                                                                                                                                                  r$383
                                                                                                                                                                                                                                  r$384))
                                                                                                                                                                                                                              (car lst2$179)))
                                                                                                                                                                                                                           (car lst1$180))))
                                                                                                                                                                                                                      (null? lst2$179))))
                                                                                                                                                                                                                 (null? lst1$180)))))
                                                                                                                                                                                                           (set! one-way-unify1$131 r$385)))
                                                                                                                                                                                                        (lambda (k$386 term1$182 term2$181)
                                                                                                                                                                                                          ((lambda (r$387)
                                                                                                                                                                                                             (if r$387
                                                                                                                                                                                                               ((lambda (r$388)
                                                                                                                                                                                                                  (if r$388
                                                                                                                                                                                                                    ((lambda (r$392)
                                                                                                                                                                                                                       ((lambda (r$393)
                                                                                                                                                                                                                          ((lambda (r$389)
                                                                                                                                                                                                                             (if r$389
                                                                                                                                                                                                                               ((lambda ()
                                                                                                                                                                                                                                  ((lambda (r$390)
                                                                                                                                                                                                                                     ((lambda (r$391)
                                                                                                                                                                                                                                        (one-way-unify1-lst$130 k$386 r$390 r$391))
                                                                                                                                                                                                                                      (cdr term2$181)))
                                                                                                                                                                                                                                   (cdr term1$182))))
                                                                                                                                                                                                                               ((lambda () (k$386 #f)))))
                                                                                                                                                                                                                           (eq? r$392 r$393)))
                                                                                                                                                                                                                        (car term2$181)))
                                                                                                                                                                                                                     (car term1$182))
                                                                                                                                                                                                                    ((lambda () (k$386 #f)))))
                                                                                                                                                                                                                (pair? term1$182))
                                                                                                                                                                                                               ((lambda ()
                                                                                                                                                                                                                  ((lambda (r$394)
                                                                                                                                                                                                                     ((lambda (temp-temp$183)
                                                                                                                                                                                                                        (if temp-temp$183
                                                                                                                                                                                                                          ((lambda ()
                                                                                                                                                                                                                             ((lambda (r$395)
                                                                                                                                                                                                                                (term-equal?$123 k$386 term1$182 r$395))
                                                                                                                                                                                                                              (cdr temp-temp$183))))
                                                                                                                                                                                                                          ((lambda (r$396)
                                                                                                                                                                                                                             (if r$396
                                                                                                                                                                                                                               ((lambda () (k$386 (equal? term1$182 term2$181))))
                                                                                                                                                                                                                               ((lambda ()
                                                                                                                                                                                                                                  ((lambda (r$399)
                                                                                                                                                                                                                                     ((lambda (r$398)
                                                                                                                                                                                                                                        ((lambda (r$397) (k$386 #t))
                                                                                                                                                                                                                                         (set! unify-subst$133 r$398)))
                                                                                                                                                                                                                                      (cons r$399 unify-subst$133)))
                                                                                                                                                                                                                                   (cons term2$181 term1$182))))))
                                                                                                                                                                                                                           (number? term2$181))))
                                                                                                                                                                                                                      r$394))
                                                                                                                                                                                                                   (assq term2$181 unify-subst$133))))))
                                                                                                                                                                                                           (pair? term2$181)))))
                                                                                                                                                                                                     (set! one-way-unify$132 r$400)))
                                                                                                                                                                                                  (lambda (k$401 term1$185 term2$184)
                                                                                                                                                                                                    ((lambda (r$403)
                                                                                                                                                                                                       ((lambda (r$402)
                                                                                                                                                                                                          (one-way-unify1$131 k$401 term1$185 term2$184))
                                                                                                                                                                                                        (set! unify-subst$133 r$403)))
                                                                                                                                                                                                     '()))))
                                                                                                                                                                                               (set! unify-subst$133 r$404)))
                                                                                                                                                                                            '*))
                                                                                                                                                                                         (set! rewrite-with-lemmas$134 r$405)))
                                                                                                                                                                                      (lambda (k$406 term$187 lst$186)
                                                                                                                                                                                        ((lambda (r$407)
                                                                                                                                                                                           (if r$407
                                                                                                                                                                                             ((lambda () (k$406 term$187)))
                                                                                                                                                                                             ((lambda (r$414)
                                                                                                                                                                                                ((lambda (r$413)
                                                                                                                                                                                                   (one-way-unify$132
                                                                                                                                                                                                     (lambda (r$408)
                                                                                                                                                                                                       (if r$408
                                                                                                                                                                                                         ((lambda ()
                                                                                                                                                                                                            ((lambda (r$411)
                                                                                                                                                                                                               ((lambda (r$410)
                                                                                                                                                                                                                  (apply-subst$143
                                                                                                                                                                                                                    (lambda (r$409) (rewrite$136 k$406 r$409))
                                                                                                                                                                                                                    unify-subst$133
                                                                                                                                                                                                                    r$410))
                                                                                                                                                                                                                (caddr r$411)))
                                                                                                                                                                                                             (car lst$186))))
                                                                                                                                                                                                         ((lambda ()
                                                                                                                                                                                                            ((lambda (r$412)
                                                                                                                                                                                                               (rewrite-with-lemmas$134 k$406 term$187 r$412))
                                                                                                                                                                                                             (cdr lst$186))))))
                                                                                                                                                                                                     term$187
                                                                                                                                                                                                     r$413))
                                                                                                                                                                                                 (cadr r$414)))
                                                                                                                                                                                              (car lst$186))))
                                                                                                                                                                                         (null? lst$186)))))
                                                                                                                                                                                   (set! rewrite-args$135 r$415)))
                                                                                                                                                                                (lambda (k$416 lst$188)
                                                                                                                                                                                  ((lambda (r$417)
                                                                                                                                                                                     (if r$417
                                                                                                                                                                                       ((lambda () (k$416 '())))
                                                                                                                                                                                       ((lambda ()
                                                                                                                                                                                          ((lambda (r$421)
                                                                                                                                                                                             (rewrite$136
                                                                                                                                                                                               (lambda (r$418)
                                                                                                                                                                                                 ((lambda (r$420)
                                                                                                                                                                                                    (rewrite-args$135
                                                                                                                                                                                                      (lambda (r$419)
                                                                                                                                                                                                        (scons$137 k$416 r$418 r$419 lst$188))
                                                                                                                                                                                                      r$420))
                                                                                                                                                                                                  (cdr lst$188)))
                                                                                                                                                                                               r$421))
                                                                                                                                                                                           (car lst$188))))))
                                                                                                                                                                                   (null? lst$188)))))
                                                                                                                                                                             (set! rewrite$136 r$422)))
                                                                                                                                                                          (lambda (k$423 term$189)
                                                                                                                                                                            ((lambda (r$432)
                                                                                                                                                                               ((lambda (r$424)
                                                                                                                                                                                  ((lambda (r$425)
                                                                                                                                                                                     (if r$425
                                                                                                                                                                                       ((lambda ()
                                                                                                                                                                                          ((lambda (r$429)
                                                                                                                                                                                             ((lambda (r$431)
                                                                                                                                                                                                (rewrite-args$135
                                                                                                                                                                                                  (lambda (r$430)
                                                                                                                                                                                                    (scons$137
                                                                                                                                                                                                      (lambda (r$426)
                                                                                                                                                                                                        ((lambda (r$428)
                                                                                                                                                                                                           (get-lemmas$148
                                                                                                                                                                                                             (lambda (r$427)
                                                                                                                                                                                                               (rewrite-with-lemmas$134 k$423 r$426 r$427))
                                                                                                                                                                                                             r$428))
                                                                                                                                                                                                         (car term$189)))
                                                                                                                                                                                                      r$429
                                                                                                                                                                                                      r$430
                                                                                                                                                                                                      term$189))
                                                                                                                                                                                                  r$431))
                                                                                                                                                                                              (cdr term$189)))
                                                                                                                                                                                           (car term$189))))
                                                                                                                                                                                       ((lambda () (k$423 term$189)))))
                                                                                                                                                                                   (pair? term$189)))
                                                                                                                                                                                (set! rewrite-count$138 r$432)))
                                                                                                                                                                             (+ rewrite-count$138 1)))))
                                                                                                                                                                       (set! scons$137 r$433)))
                                                                                                                                                                    (lambda (k$434 x$192 y$191 original$190)
                                                                                                                                                                      ((lambda (k$436)
                                                                                                                                                                         ((lambda (r$439)
                                                                                                                                                                            ((lambda (r$437)
                                                                                                                                                                               (if r$437
                                                                                                                                                                                 ((lambda (r$438) (k$436 (eq? y$191 r$438)))
                                                                                                                                                                                  (cdr original$190))
                                                                                                                                                                                 (k$436 #f)))
                                                                                                                                                                             (eq? x$192 r$439)))
                                                                                                                                                                          (car original$190)))
                                                                                                                                                                       (lambda (r$435)
                                                                                                                                                                         (if r$435
                                                                                                                                                                           (k$434 original$190)
                                                                                                                                                                           (k$434 (cons x$192 y$191))))))))
                                                                                                                                                                 (set! rewrite-count$138 0)))
                                                                                                                                                              (set! if-constructor$139 r$440)))
                                                                                                                                                           '*))
                                                                                                                                                        (set! tautologyp$140 r$441)))
                                                                                                                                                     (lambda (k$442 x$195 true-lst$194 false-lst$193)
                                                                                                                                                       (truep$128
                                                                                                                                                         (lambda (r$443)
                                                                                                                                                           (if r$443
                                                                                                                                                             ((lambda () (k$442 #t)))
                                                                                                                                                             (falsep$129
                                                                                                                                                               (lambda (r$444)
                                                                                                                                                                 (if r$444
                                                                                                                                                                   ((lambda () (k$442 #f)))
                                                                                                                                                                   ((lambda (r$445)
                                                                                                                                                                      (if r$445
                                                                                                                                                                        ((lambda (r$460)
                                                                                                                                                                           ((lambda (r$446)
                                                                                                                                                                              (if r$446
                                                                                                                                                                                ((lambda ()
                                                                                                                                                                                   ((lambda (r$459)
                                                                                                                                                                                      (truep$128
                                                                                                                                                                                        (lambda (r$447)
                                                                                                                                                                                          (if r$447
                                                                                                                                                                                            ((lambda ()
                                                                                                                                                                                               ((lambda (r$448)
                                                                                                                                                                                                  (tautologyp$140
                                                                                                                                                                                                    k$442
                                                                                                                                                                                                    r$448
                                                                                                                                                                                                    true-lst$194
                                                                                                                                                                                                    false-lst$193))
                                                                                                                                                                                                (caddr x$195))))
                                                                                                                                                                                            ((lambda (r$458)
                                                                                                                                                                                               (falsep$129
                                                                                                                                                                                                 (lambda (r$449)
                                                                                                                                                                                                   (if r$449
                                                                                                                                                                                                     ((lambda ()
                                                                                                                                                                                                        ((lambda (r$450)
                                                                                                                                                                                                           (tautologyp$140
                                                                                                                                                                                                             k$442
                                                                                                                                                                                                             r$450
                                                                                                                                                                                                             true-lst$194
                                                                                                                                                                                                             false-lst$193))
                                                                                                                                                                                                         (cadddr x$195))))
                                                                                                                                                                                                     ((lambda ()
                                                                                                                                                                                                        ((lambda (r$455)
                                                                                                                                                                                                           ((lambda (r$457)
                                                                                                                                                                                                              ((lambda (r$456)
                                                                                                                                                                                                                 (tautologyp$140
                                                                                                                                                                                                                   (lambda (r$451)
                                                                                                                                                                                                                     (if r$451
                                                                                                                                                                                                                       ((lambda (r$452)
                                                                                                                                                                                                                          ((lambda (r$454)
                                                                                                                                                                                                                             ((lambda (r$453)
                                                                                                                                                                                                                                (tautologyp$140 k$442 r$452 true-lst$194 r$453))
                                                                                                                                                                                                                              (cons r$454 false-lst$193)))
                                                                                                                                                                                                                           (cadr x$195)))
                                                                                                                                                                                                                        (cadddr x$195))
                                                                                                                                                                                                                       (k$442 #f)))
                                                                                                                                                                                                                   r$455
                                                                                                                                                                                                                   r$456
                                                                                                                                                                                                                   false-lst$193))
                                                                                                                                                                                                               (cons r$457 true-lst$194)))
                                                                                                                                                                                                            (cadr x$195)))
                                                                                                                                                                                                         (caddr x$195))))))
                                                                                                                                                                                                 r$458
                                                                                                                                                                                                 false-lst$193))
                                                                                                                                                                                             (cadr x$195))))
                                                                                                                                                                                        r$459
                                                                                                                                                                                        true-lst$194))
                                                                                                                                                                                    (cadr x$195))))
                                                                                                                                                                                ((lambda () (k$442 #f)))))
                                                                                                                                                                            (eq? r$460 if-constructor$139)))
                                                                                                                                                                         (car x$195))
                                                                                                                                                                        ((lambda () (k$442 #f)))))
                                                                                                                                                                    (pair? x$195))))
                                                                                                                                                               x$195
                                                                                                                                                               false-lst$193)))
                                                                                                                                                         x$195
                                                                                                                                                         true-lst$194))))
                                                                                                                                                  (set! tautp$141 r$461)))
                                                                                                                                               (lambda (k$462 x$196)
                                                                                                                                                 (rewrite$136
                                                                                                                                                   (lambda (r$463)
                                                                                                                                                     ((lambda (r$464)
                                                                                                                                                        ((lambda (r$465)
                                                                                                                                                           (tautologyp$140 k$462 r$463 r$464 r$465))
                                                                                                                                                         '()))
                                                                                                                                                      '()))
                                                                                                                                                   x$196))))
                                                                                                                                            (set! apply-subst-lst$142 r$466)))
                                                                                                                                         (lambda (k$467 alist$198 lst$197)
                                                                                                                                           ((lambda (r$468)
                                                                                                                                              (if r$468
                                                                                                                                                ((lambda () (k$467 '())))
                                                                                                                                                ((lambda ()
                                                                                                                                                   ((lambda (r$472)
                                                                                                                                                      (apply-subst$143
                                                                                                                                                        (lambda (r$469)
                                                                                                                                                          ((lambda (r$471)
                                                                                                                                                             (apply-subst-lst$142
                                                                                                                                                               (lambda (r$470) (k$467 (cons r$469 r$470)))
                                                                                                                                                               alist$198
                                                                                                                                                               r$471))
                                                                                                                                                           (cdr lst$197)))
                                                                                                                                                        alist$198
                                                                                                                                                        r$472))
                                                                                                                                                    (car lst$197))))))
                                                                                                                                            (null? lst$197)))))
                                                                                                                                      (set! apply-subst$143 r$473)))
                                                                                                                                   (lambda (k$474 alist$200 term$199)
                                                                                                                                     ((lambda (r$475)
                                                                                                                                        (if r$475
                                                                                                                                          ((lambda ()
                                                                                                                                             ((lambda (r$476)
                                                                                                                                                ((lambda (r$478)
                                                                                                                                                   (apply-subst-lst$142
                                                                                                                                                     (lambda (r$477) (k$474 (cons r$476 r$477)))
                                                                                                                                                     alist$200
                                                                                                                                                     r$478))
                                                                                                                                                 (cdr term$199)))
                                                                                                                                              (car term$199))))
                                                                                                                                          ((lambda ()
                                                                                                                                             ((lambda (r$479)
                                                                                                                                                ((lambda (temp-temp$201)
                                                                                                                                                   (if temp-temp$201
                                                                                                                                                     (k$474 (cdr temp-temp$201))
                                                                                                                                                     (k$474 term$199)))
                                                                                                                                                 r$479))
                                                                                                                                              (assq term$199 alist$200))))))
                                                                                                                                      (pair? term$199)))))
                                                                                                                                (set! translate-alist$144 r$480)))
                                                                                                                             (lambda (k$481 alist$202)
                                                                                                                               ((lambda (r$482)
                                                                                                                                  (if r$482
                                                                                                                                    ((lambda () (k$481 '())))
                                                                                                                                    ((lambda ()
                                                                                                                                       ((lambda (r$486)
                                                                                                                                          ((lambda (r$488)
                                                                                                                                             (translate-term$157
                                                                                                                                               (lambda (r$487)
                                                                                                                                                 ((lambda (r$483)
                                                                                                                                                    ((lambda (r$485)
                                                                                                                                                       (translate-alist$144
                                                                                                                                                         (lambda (r$484) (k$481 (cons r$483 r$484)))
                                                                                                                                                         r$485))
                                                                                                                                                     (cdr alist$202)))
                                                                                                                                                  (cons r$486 r$487)))
                                                                                                                                               r$488))
                                                                                                                                           (cdar alist$202)))
                                                                                                                                        (caar alist$202))))))
                                                                                                                                (null? alist$202)))))
                                                                                                                          (set! test$145 r$489)))
                                                                                                                       (lambda (k$490 alist$205 term$204 n$203)
                                                                                                                         (translate-alist$144
                                                                                                                           (lambda (r$492)
                                                                                                                             ((lambda (term$207 n$206)
                                                                                                                                ((lambda (lp$208)
                                                                                                                                   ((lambda (r$496)
                                                                                                                                      ((lambda (r$495)
                                                                                                                                         (lp$208
                                                                                                                                           (lambda (r$494)
                                                                                                                                             (translate-term$157
                                                                                                                                               (lambda (r$493)
                                                                                                                                                 (apply-subst$143
                                                                                                                                                   (lambda (r$491)
                                                                                                                                                     ((lambda (term$211) (tautp$141 k$490 term$211))
                                                                                                                                                      r$491))
                                                                                                                                                   r$492
                                                                                                                                                   r$493))
                                                                                                                                               r$494))
                                                                                                                                           term$207
                                                                                                                                           n$206))
                                                                                                                                       (set! lp$208 r$496)))
                                                                                                                                    (lambda (k$497 term$210 n$209)
                                                                                                                                      (zero? (lambda (r$498)
                                                                                                                                               (if r$498
                                                                                                                                                 (k$497 term$210)
                                                                                                                                                 ((lambda (r$501)
                                                                                                                                                    ((lambda (r$502)
                                                                                                                                                       (list (lambda (r$499)
                                                                                                                                                               ((lambda (r$500) (lp$208 k$497 r$499 r$500))
                                                                                                                                                                (- n$209 1)))
                                                                                                                                                             r$501
                                                                                                                                                             term$210
                                                                                                                                                             r$502))
                                                                                                                                                     '(f)))
                                                                                                                                                  'or)))
                                                                                                                                             n$209))))
                                                                                                                                 #f))
                                                                                                                              term$204
                                                                                                                              n$203))
                                                                                                                           alist$205))))
                                                                                                                    (set! symbol-record-equal?$146 r$503)))
                                                                                                                 (lambda (k$504 r1$213 r2$212)
                                                                                                                   (k$504 (eq? r1$213 r2$212)))))
                                                                                                              (set! get-name$147 r$505)))
                                                                                                           (lambda (k$506 symbol-record$214)
                                                                                                             (k$506 (vector-ref symbol-record$214 0)))))
                                                                                                        (set! get-lemmas$148 r$507)))
                                                                                                     (lambda (k$508 symbol-record$215)
                                                                                                       (k$508 (vector-ref symbol-record$215 1)))))
                                                                                                  (set! put-lemmas!$149 r$509)))
                                                                                               (lambda (k$510 symbol-record$217 lemmas$216)
                                                                                                 (k$510 (vector-set! symbol-record$217 1 lemmas$216)))))
                                                                                            (set! make-symbol-record$150 r$511)))
                                                                                         (lambda (k$512 sym$218)
                                                                                           ((lambda (r$513) (vector k$512 sym$218 r$513))
                                                                                            '()))))
                                                                                      (set! *symbol-records-alist*$151 r$514)))
                                                                                   '()))
                                                                                (set! symbol->symbol-record$152 r$515)))
                                                                             (lambda (k$516 sym$219)
                                                                               ((lambda (r$517)
                                                                                  ((lambda (x$220)
                                                                                     (if x$220
                                                                                       (k$516 (cdr x$220))
                                                                                       (make-symbol-record$150
                                                                                         (lambda (r$518)
                                                                                           ((lambda (r$221)
                                                                                              ((lambda (r$521)
                                                                                                 ((lambda (r$520)
                                                                                                    ((lambda (r$519) (k$516 r$221))
                                                                                                     (set! *symbol-records-alist*$151 r$520)))
                                                                                                  (cons r$521 *symbol-records-alist*$151)))
                                                                                               (cons sym$219 r$221)))
                                                                                            r$518))
                                                                                         sym$219)))
                                                                                   r$517))
                                                                                (assq sym$219 *symbol-records-alist*$151)))))
                                                                          (set! get$153 r$522)))
                                                                       (lambda (k$523 sym$223 property$222)
                                                                         (symbol->symbol-record$152
                                                                           (lambda (r$524) (get-lemmas$148 k$523 r$524))
                                                                           sym$223))))
                                                                    (set! put$154 r$525)))
                                                                 (lambda (k$526 sym$226 property$225 value$224)
                                                                   (symbol->symbol-record$152
                                                                     (lambda (r$527)
                                                                       (put-lemmas!$149 k$526 r$527 value$224))
                                                                     sym$226))))
                                                              (set! untranslate-term$155 r$528)))
                                                           (lambda (k$529 term$227)
                                                             ((lambda (r$530)
                                                                (if r$530
                                                                  ((lambda ()
                                                                     ((lambda (r$534)
                                                                        (get-name$147
                                                                          (lambda (r$531)
                                                                            ((lambda (r$533)
                                                                               (map (lambda (r$532) (k$529 (cons r$531 r$532)))
                                                                                    untranslate-term$155
                                                                                    r$533))
                                                                             (cdr term$227)))
                                                                          r$534))
                                                                      (car term$227))))
                                                                  ((lambda () (k$529 term$227)))))
                                                              (pair? term$227)))))
                                                        (set! translate-args$156 r$535)))
                                                     (lambda (k$536 lst$228)
                                                       ((lambda (r$537)
                                                          (if r$537
                                                            ((lambda () (k$536 '())))
                                                            ((lambda ()
                                                               ((lambda (r$541)
                                                                  (translate-term$157
                                                                    (lambda (r$538)
                                                                      ((lambda (r$540)
                                                                         (translate-args$156
                                                                           (lambda (r$539) (k$536 (cons r$538 r$539)))
                                                                           r$540))
                                                                       (cdr lst$228)))
                                                                    r$541))
                                                                (car lst$228))))))
                                                        (null? lst$228)))))
                                                  (set! translate-term$157 r$542)))
                                               (lambda (k$543 term$229)
                                                 ((lambda (r$544)
                                                    (if r$544
                                                      ((lambda ()
                                                         ((lambda (r$548)
                                                            (symbol->symbol-record$152
                                                              (lambda (r$545)
                                                                ((lambda (r$547)
                                                                   (translate-args$156
                                                                     (lambda (r$546) (k$543 (cons r$545 r$546)))
                                                                     r$547))
                                                                 (cdr term$229)))
                                                              r$548))
                                                          (car term$229))))
                                                      ((lambda () (k$543 term$229)))))
                                                  (pair? term$229)))))
                                            (set! add-lemma$158 r$549)))
                                         (lambda (k$550 term$230)
                                           ((lambda (k$561)
                                              ((lambda (r$562)
                                                 (if r$562
                                                   ((lambda (r$565)
                                                      ((lambda (r$566)
                                                         ((lambda (r$563)
                                                            (if r$563
                                                              ((lambda (r$564) (k$561 (pair? r$564)))
                                                               (cadr term$230))
                                                              (k$561 #f)))
                                                          (eq? r$565 r$566)))
                                                       'equal))
                                                    (car term$230))
                                                   (k$561 #f)))
                                               (pair? term$230)))
                                            (lambda (r$551)
                                              (if r$551
                                                ((lambda ()
                                                   ((lambda (r$560)
                                                      ((lambda (r$552)
                                                         ((lambda (r$553)
                                                            (translate-term$157
                                                              (lambda (r$555)
                                                                ((lambda (r$559)
                                                                   ((lambda (r$557)
                                                                      ((lambda (r$558)
                                                                         (get$153
                                                                           (lambda (r$556)
                                                                             ((lambda (r$554)
                                                                                (put$154 k$550 r$552 r$553 r$554))
                                                                              (cons r$555 r$556)))
                                                                           r$557
                                                                           r$558))
                                                                       'lemmas))
                                                                    (car r$559)))
                                                                 (cadr term$230)))
                                                              term$230))
                                                          'lemmas))
                                                       (car r$560)))
                                                    (cadr term$230))))
                                                ((lambda ()
                                                   (error k$550
                                                          #f
                                                          "ADD-LEMMA did not like term:  "
                                                          term$230)))))))))
                                      (set! add-lemma-lst$159 r$567)))
                                   (lambda (k$568 lst$231)
                                     ((lambda (r$569)
                                        (if r$569
                                          ((lambda () (k$568 #t)))
                                          ((lambda ()
                                             ((lambda (r$572)
                                                (add-lemma$158
                                                  (lambda (r$570)
                                                    ((lambda (r$571) (add-lemma-lst$159 k$568 r$571))
                                                     (cdr lst$231)))
                                                  r$572))
                                              (car lst$231))))))
                                      (null? lst$231)))))
                                (set! setup$160 r$573)))
                             (lambda (k$574)
                               ((lambda (r$575) (add-lemma-lst$159 k$574 r$575))
                                '((equal (compile form)
                                         (reverse (codegen (optimize form) (nil))))
                                  (equal (eqp x y) (equal (fix x) (fix y)))
                                  (equal (greaterp x y) (lessp y x))
                                  (equal (lesseqp x y) (not (lessp y x)))
                                  (equal (greatereqp x y) (not (lessp x y)))
                                  (equal (boolean x)
                                         (or (equal x (t)) (equal x (f))))
                                  (equal (iff x y)
                                         (and (implies x y) (implies y x)))
                                  (equal (even1 x)
                                         (if (zerop x) (t) (odd (_1- x))))
                                  (equal (countps- l pred)
                                         (countps-loop l pred (zero)))
                                  (equal (fact- i) (fact-loop i 1))
                                  (equal (reverse- x) (reverse-loop x (nil)))
                                  (equal (divides x y) (zerop (remainder y x)))
                                  (equal (assume-true var alist)
                                         (cons (cons var (t)) alist))
                                  (equal (assume-false var alist)
                                         (cons (cons var (f)) alist))
                                  (equal (tautology-checker x)
                                         (tautologyp (normalize x) (nil)))
                                  (equal (falsify x)
                                         (falsify1 (normalize x) (nil)))
                                  (equal (prime x)
                                         (and (not (zerop x))
                                              (not (equal x (add1 (zero))))
                                              (prime1 x (_1- x))))
                                  (equal (and p q) (if p (if q (t) (f)) (f)))
                                  (equal (or p q) (if p (t) (if q (t) (f))))
                                  (equal (not p) (if p (f) (t)))
                                  (equal (implies p q) (if p (if q (t) (f)) (t)))
                                  (equal (fix x) (if (numberp x) x (zero)))
                                  (equal (if (if a b c) d e)
                                         (if a (if b d e) (if c d e)))
                                  (equal (zerop x)
                                         (or (equal x (zero)) (not (numberp x))))
                                  (equal (plus (plus x y) z) (plus x (plus y z)))
                                  (equal (equal (plus a b) (zero))
                                         (and (zerop a) (zerop b)))
                                  (equal (difference x x) (zero))
                                  (equal (equal (plus a b) (plus a c))
                                         (equal (fix b) (fix c)))
                                  (equal (equal (zero) (difference x y))
                                         (not (lessp y x)))
                                  (equal (equal x (difference x y))
                                         (and (numberp x) (or (equal x (zero)) (zerop y))))
                                  (equal (meaning (plus-tree (append x y)) a)
                                         (plus (meaning (plus-tree x) a)
                                               (meaning (plus-tree y) a)))
                                  (equal (meaning (plus-tree (plus-fringe x)) a)
                                         (fix (meaning x a)))
                                  (equal (append (append x y) z)
                                         (append x (append y z)))
                                  (equal (reverse (append a b))
                                         (append (reverse b) (reverse a)))
                                  (equal (times x (plus y z))
                                         (plus (times x y) (times x z)))
                                  (equal (times (times x y) z)
                                         (times x (times y z)))
                                  (equal (equal (times x y) (zero))
                                         (or (zerop x) (zerop y)))
                                  (equal (exec (append x y) pds envrn)
                                         (exec y (exec x pds envrn) envrn))
                                  (equal (mc-flatten x y) (append (flatten x) y))
                                  (equal (member x (append a b))
                                         (or (member x a) (member x b)))
                                  (equal (member x (reverse y)) (member x y))
                                  (equal (length (reverse x)) (length x))
                                  (equal (member a (intersect b c))
                                         (and (member a b) (member a c)))
                                  (equal (nth (zero) i) (zero))
                                  (equal (exp i (plus j k))
                                         (times (exp i j) (exp i k)))
                                  (equal (exp i (times j k)) (exp (exp i j) k))
                                  (equal (reverse-loop x y) (append (reverse x) y))
                                  (equal (reverse-loop x (nil)) (reverse x))
                                  (equal (count-list z (sort-lp x y))
                                         (plus (count-list z x) (count-list z y)))
                                  (equal (equal (append a b) (append a c))
                                         (equal b c))
                                  (equal (plus (remainder x y) (times y (quotient x y)))
                                         (fix x))
                                  (equal (power-eval (big-plus1 l i base) base)
                                         (plus (power-eval l base) i))
                                  (equal (power-eval (big-plus x y i base) base)
                                         (plus i
                                               (plus (power-eval x base) (power-eval y base))))
                                  (equal (remainder y 1) (zero))
                                  (equal (lessp (remainder x y) y) (not (zerop y)))
                                  (equal (remainder x x) (zero))
                                  (equal (lessp (quotient i j) i)
                                         (and (not (zerop i))
                                              (or (zerop j) (not (equal j 1)))))
                                  (equal (lessp (remainder x y) x)
                                         (and (not (zerop y))
                                              (not (zerop x))
                                              (not (lessp x y))))
                                  (equal (power-eval (power-rep i base) base)
                                         (fix i))
                                  (equal (power-eval
                                           (big-plus
                                             (power-rep i base)
                                             (power-rep j base)
                                             (zero)
                                             base)
                                           base)
                                         (plus i j))
                                  (equal (gcd x y) (gcd y x))
                                  (equal (nth (append a b) i)
                                         (append
                                           (nth a i)
                                           (nth b (difference i (length a)))))
                                  (equal (difference (plus x y) x) (fix y))
                                  (equal (difference (plus y x) x) (fix y))
                                  (equal (difference (plus x y) (plus x z))
                                         (difference y z))
                                  (equal (times x (difference c w))
                                         (difference (times c x) (times w x)))
                                  (equal (remainder (times x z) z) (zero))
                                  (equal (difference (plus b (plus a c)) a)
                                         (plus b c))
                                  (equal (difference (add1 (plus y z)) z) (add1 y))
                                  (equal (lessp (plus x y) (plus x z)) (lessp y z))
                                  (equal (lessp (times x z) (times y z))
                                         (and (not (zerop z)) (lessp x y)))
                                  (equal (lessp y (plus x y)) (not (zerop x)))
                                  (equal (gcd (times x z) (times y z))
                                         (times z (gcd x y)))
                                  (equal (value (normalize x) a) (value x a))
                                  (equal (equal (flatten x) (cons y (nil)))
                                         (and (nlistp x) (equal x y)))
                                  (equal (listp (gopher x)) (listp x))
                                  (equal (samefringe x y)
                                         (equal (flatten x) (flatten y)))
                                  (equal (equal (greatest-factor x y) (zero))
                                         (and (or (zerop y) (equal y 1)) (equal x (zero))))
                                  (equal (equal (greatest-factor x y) 1)
                                         (equal x 1))
                                  (equal (numberp (greatest-factor x y))
                                         (not (and (or (zerop y) (equal y 1))
                                                   (not (numberp x)))))
                                  (equal (times-list (append x y))
                                         (times (times-list x) (times-list y)))
                                  (equal (prime-list (append x y))
                                         (and (prime-list x) (prime-list y)))
                                  (equal (equal z (times w z))
                                         (and (numberp z)
                                              (or (equal z (zero)) (equal w 1))))
                                  (equal (greatereqp x y) (not (lessp x y)))
                                  (equal (equal x (times x y))
                                         (or (equal x (zero))
                                             (and (numberp x) (equal y 1))))
                                  (equal (remainder (times y x) y) (zero))
                                  (equal (equal (times a b) 1)
                                         (and (not (equal a (zero)))
                                              (not (equal b (zero)))
                                              (numberp a)
                                              (numberp b)
                                              (equal (_1- a) (zero))
                                              (equal (_1- b) (zero))))
                                  (equal (lessp (length (delete x l)) (length l))
                                         (member x l))
                                  (equal (sort2 (delete x l)) (delete x (sort2 l)))
                                  (equal (dsort x) (sort2 x))
                                  (equal (length
                                           (cons x1
                                                 (cons x2
                                                       (cons x3 (cons x4 (cons x5 (cons x6 x7)))))))
                                         (plus 6 (length x7)))
                                  (equal (difference (add1 (add1 x)) 2) (fix x))
                                  (equal (quotient (plus x (plus x y)) 2)
                                         (plus x (quotient y 2)))
                                  (equal (sigma (zero) i)
                                         (quotient (times i (add1 i)) 2))
                                  (equal (plus x (add1 y))
                                         (if (numberp y) (add1 (plus x y)) (add1 x)))
                                  (equal (equal (difference x y) (difference z y))
                                         (if (lessp x y)
                                           (not (lessp y z))
                                           (if (lessp z y)
                                             (not (lessp y x))
                                             (equal (fix x) (fix z)))))
                                  (equal (meaning (plus-tree (delete x y)) a)
                                         (if (member x y)
                                           (difference
                                             (meaning (plus-tree y) a)
                                             (meaning x a))
                                           (meaning (plus-tree y) a)))
                                  (equal (times x (add1 y))
                                         (if (numberp y) (plus x (times x y)) (fix x)))
                                  (equal (nth (nil) i) (if (zerop i) (nil) (zero)))
                                  (equal (last (append a b))
                                         (if (listp b)
                                           (last b)
                                           (if (listp a) (cons (car (last a)) b) b)))
                                  (equal (equal (lessp x y) z)
                                         (if (lessp x y) (equal (t) z) (equal (f) z)))
                                  (equal (assignment x (append a b))
                                         (if (assignedp x a)
                                           (assignment x a)
                                           (assignment x b)))
                                  (equal (car (gopher x))
                                         (if (listp x) (car (flatten x)) (zero)))
                                  (equal (flatten (cdr (gopher x)))
                                         (if (listp x)
                                           (cdr (flatten x))
                                           (cons (zero) (nil))))
                                  (equal (quotient (times y x) y)
                                         (if (zerop y) (zero) (fix x)))
                                  (equal (get j (set i val mem))
                                         (if (eqp j i) val (get j mem))))))))
                          #f
                          #f
                          #f
                          #f
                          #f
                          #f
                          #f
                          #f
                          #f
                          #f
                          #f
                          #f
                          #f
                          #f
                          #f
                          #f
                          #f
                          #f
                          #f
                          #f
                          #f
                          #f
                          #f
                          #f
                          #f
                          #f
                          #f
                          #f
                          #f
                          #f
                          #f
                          #f
                          #f
                          #f
                          #f
                          #f
                          #f
                          #f
                          #f
                          #f))))
                    (set! term r$576)))
                 '(implies
                    (and (implies x y)
                         (and (implies y z) (and (implies z u) (implies u w))))
                    (implies x w))))
              (set! alist r$577)))
           '((x f (plus (plus a b) (plus c (zero))))
             (y f (times (times a b) (plus c d)))
             (z f (reverse (append (append a b) (nil))))
             (u equal (plus a b) (difference x y))
             (w lessp (remainder a b) (member a (length b))))))
        0))))
  #f
  #f
  #f
  #f
  #f
  #f
  #f
  #f
  #f
  #f
  #f
  #f
  #f
  #f
  #f
  #f
  #f
  #f
  #f
  #f
  #f
  #f
  #f
  #f
  #f
  #f
  #f
  #f
  #f
  #f
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
#;1> 

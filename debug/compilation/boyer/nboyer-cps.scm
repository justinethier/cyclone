
CHICKEN
(c)2008-2011 The Chicken Team
(c)2000-2007 Felix L. Winkelmann
Version 4.7.0 
linux-unix-gnu-x86 [ manyargs dload ptables ]
compiled 2011-10-17 on roseapple (Linux)

; loading tmp.scm ...
((define main
   (lambda (k$633)
     (read (lambda (r$634)
             ((lambda (count$254)
                (read (lambda (r$635)
                        ((lambda (input$255)
                           (read (lambda (r$636)
                                   ((lambda (output$256)
                                      ((lambda (r$637)
                                         ((lambda (s2$257)
                                            ((lambda (r$638)
                                               ((lambda (s1$258)
                                                  ((lambda (name$259)
                                                     ((lambda ()
                                                        ((lambda (r$639)
                                                           ((lambda (r$640)
                                                              ((lambda (r$641)
                                                                 (run-r7rs-benchmark
                                                                   k$633
                                                                   r$639
                                                                   count$254
                                                                   r$640
                                                                   r$641))
                                                               (lambda (k$642 rewrites$260)
                                                                 ((lambda (r$643)
                                                                    (if r$643
                                                                      (k$642 (= rewrites$260 output$256))
                                                                      (k$642 #f)))
                                                                  (number? rewrites$260)))))
                                                            (lambda (k$644)
                                                              (setup-boyer
                                                                (lambda (r$645)
                                                                  (hide (lambda (r$646)
                                                                          (test-boyer k$644 alist term r$646))
                                                                        count$254
                                                                        input$255))))))
                                                         (string-append name$259 ":" s1$258 ":" s2$257)))))
                                                   "nboyer"))
                                                r$638))
                                             (number->string input$255)))
                                          r$637))
                                       (number->string count$254)))
                                    r$636))))
                         r$635))))
              r$634)))))
 (define alist #f)
 (define term #f)
 (define setup-boyer (lambda (k$626 . args$253) (k$626 #t)))
 (define test-boyer (lambda (k$623 . args$252) (k$623 #t)))
 (define hide
   (lambda (k$609 r$248 x$247)
     ((lambda (r$610)
        ((lambda (r$611) (call-with-values k$609 r$610 r$611))
         (lambda (k$612 v$250 i$249)
           ((lambda (r$613) (r$613 k$612 x$247)) (vector-ref v$250 i$249)))))
      (lambda (k$614)
        ((lambda (r$619)
           (vector
             (lambda (r$615)
               ((lambda (k$617)
                  ((lambda (r$618) (if r$618 (k$617 0) (k$617 1)))
                   (< r$248 100)))
                (lambda (r$616) (values k$614 r$615 r$616))))
             values
             r$619))
         (lambda (k$620 x$251) (k$620 x$251)))))))
 (define run-r7rs-benchmark
   (lambda (k$568 name$229 count$228 thunk$227 ok?$226)
     ((lambda (rounded$231)
        ((lambda (rounded$232)
           ((lambda (r$603)
              ((lambda (r$569)
                 (display
                   (lambda (r$570)
                     (display
                       (lambda (r$571)
                         (newline
                           (lambda (r$572)
                             (flush-output-port
                               (lambda (r$573)
                                 (jiffies-per-second
                                   (lambda (r$574)
                                     ((lambda (j/s$233)
                                        (current-second
                                          (lambda (r$575)
                                            ((lambda (t0$234)
                                               (current-jiffy
                                                 (lambda (r$576)
                                                   ((lambda (j0$235)
                                                      ((lambda ()
                                                         ((lambda (k$602) (if #f (k$602 #f) (k$602 #f)))
                                                          (lambda (r$577)
                                                            ((lambda (i$237 result$236)
                                                               ((lambda (loop$238)
                                                                  ((lambda (r$579)
                                                                     ((lambda (r$578)
                                                                        (loop$238 k$568 i$237 result$236))
                                                                      (set! loop$238 r$579)))
                                                                   (lambda (k$580 i$240 result$239)
                                                                     ((lambda (r$581)
                                                                        (if r$581
                                                                          ((lambda ()
                                                                             ((lambda (r$582)
                                                                                (thunk$227
                                                                                  (lambda (r$583) (loop$238 k$580 r$582 r$583))))
                                                                              (+ i$240 1))))
                                                                          (ok?$226
                                                                            (lambda (r$584)
                                                                              (if r$584
                                                                                ((lambda ()
                                                                                   (current-jiffy
                                                                                     (lambda (r$586)
                                                                                       ((lambda (j1$241)
                                                                                          (current-second
                                                                                            (lambda (r$587)
                                                                                              ((lambda (t1$242)
                                                                                                 ((lambda (r$588)
                                                                                                    ((lambda (jifs$243)
                                                                                                       ((lambda (r$598)
                                                                                                          (inexact
                                                                                                            (lambda (r$589)
                                                                                                              ((lambda (secs$244)
                                                                                                                 ((lambda (r$597)
                                                                                                                    (rounded$231
                                                                                                                      (lambda (r$590)
                                                                                                                        ((lambda (secs2$245)
                                                                                                                           ((lambda ()
                                                                                                                              (display
                                                                                                                                (lambda (r$591)
                                                                                                                                  (write (lambda (r$592)
                                                                                                                                           (display
                                                                                                                                             (lambda (r$593)
                                                                                                                                               (write (lambda (r$594)
                                                                                                                                                        (display
                                                                                                                                                          (lambda (r$595)
                                                                                                                                                            (display
                                                                                                                                                              (lambda (r$596)
                                                                                                                                                                (newline (lambda (r$585) (k$580 result$239))))
                                                                                                                                                              name$229))
                                                                                                                                                          ") for "))
                                                                                                                                                      secs2$245))
                                                                                                                                             " seconds ("))
                                                                                                                                         secs$244))
                                                                                                                                "Elapsed time: "))))
                                                                                                                         r$590))
                                                                                                                      r$597))
                                                                                                                  (- t1$242 t0$234)))
                                                                                                               r$589))
                                                                                                            r$598))
                                                                                                        (/ jifs$243 j/s$233)))
                                                                                                     r$588))
                                                                                                  (- j1$241 j0$235)))
                                                                                               r$587))))
                                                                                        r$586)))))
                                                                                ((lambda ()
                                                                                   (display
                                                                                     (lambda (r$599)
                                                                                       (write (lambda (r$600)
                                                                                                (newline (lambda (r$601) (k$580 result$239))))
                                                                                              result$239))
                                                                                     "ERROR: returned incorrect result: ")))))
                                                                            result$239)))
                                                                      (< i$240 count$228)))))
                                                                #f))
                                                             0
                                                             r$577))))))
                                                    r$576))))
                                             r$575))))
                                      r$574))))))))
                       name$229))
                   "Running "))
               (set! rounded$231 r$603)))
            (lambda (k$604 x$246)
              ((lambda (r$606)
                 (round (lambda (r$605) (k$604 (/ r$605 1000))) r$606))
               (* 1000 x$246)))))
         #f))
      #f)))
 ((lambda (*symbol-records-alist*$118
           add-lemma$117
           add-lemma-lst$116
           apply-subst$115
           apply-subst-lst$114
           false-term$113
           falsep$112
           get$111
           get-lemmas$110
           get-name$109
           if-constructor$108
           make-symbol-record$107
           one-way-unify$106
           one-way-unify1$105
           one-way-unify1-lst$104
           put$103
           put-lemmas!$102
           rewrite$101
           rewrite-args$100
           rewrite-count$99
           rewrite-with-lemmas$98
           setup$97
           symbol->symbol-record$96
           symbol-record-equal?$95
           tautologyp$94
           tautp$93
           term-args-equal?$92
           term-equal?$91
           term-member?$90
           test$89
           trans-of-implies$88
           trans-of-implies1$87
           translate-alist$86
           translate-args$85
           translate-term$84
           true-term$83
           truep$82
           unify-subst$81
           untranslate-term$80)
    ((lambda ()
       ((lambda (r$261)
          ((lambda (r$565)
             ((lambda (r$262)
                ((lambda (r$564)
                   ((lambda (r$263)
                      ((lambda ()
                         ((lambda (setup$157
                                   add-lemma-lst$156
                                   add-lemma$155
                                   translate-term$154
                                   translate-args$153
                                   untranslate-term$152
                                   put$151
                                   get$150
                                   symbol->symbol-record$149
                                   *symbol-records-alist*$148
                                   make-symbol-record$147
                                   put-lemmas!$146
                                   get-lemmas$145
                                   get-name$144
                                   symbol-record-equal?$143
                                   test$142
                                   translate-alist$141
                                   apply-subst$140
                                   apply-subst-lst$139
                                   tautp$138
                                   tautologyp$137
                                   if-constructor$136
                                   rewrite-count$135
                                   rewrite$134
                                   rewrite-args$133
                                   rewrite-with-lemmas$132
                                   unify-subst$131
                                   one-way-unify$130
                                   one-way-unify1$129
                                   one-way-unify1-lst$128
                                   falsep$127
                                   truep$126
                                   false-term$125
                                   true-term$124
                                   trans-of-implies$123
                                   trans-of-implies1$122
                                   term-equal?$121
                                   term-args-equal?$120
                                   term-member?$119)
                            ((lambda (r$561)
                               ((lambda (r$265)
                                  ((lambda (r$555)
                                     ((lambda (r$266)
                                        ((lambda (r$537)
                                           ((lambda (r$267)
                                              ((lambda (r$530)
                                                 ((lambda (r$268)
                                                    ((lambda (r$523)
                                                       ((lambda (r$269)
                                                          ((lambda (r$516)
                                                             ((lambda (r$270)
                                                                ((lambda (r$513)
                                                                   ((lambda (r$271)
                                                                      ((lambda (r$510)
                                                                         ((lambda (r$272)
                                                                            ((lambda (r$503)
                                                                               ((lambda (r$273)
                                                                                  ((lambda (r$502)
                                                                                     ((lambda (r$274)
                                                                                        ((lambda (r$499)
                                                                                           ((lambda (r$275)
                                                                                              ((lambda (r$497)
                                                                                                 ((lambda (r$276)
                                                                                                    ((lambda (r$495)
                                                                                                       ((lambda (r$277)
                                                                                                          ((lambda (r$493)
                                                                                                             ((lambda (r$278)
                                                                                                                ((lambda (r$491)
                                                                                                                   ((lambda (r$279)
                                                                                                                      ((lambda (r$477)
                                                                                                                         ((lambda (r$280)
                                                                                                                            ((lambda (r$468)
                                                                                                                               ((lambda (r$281)
                                                                                                                                  ((lambda (r$461)
                                                                                                                                     ((lambda (r$282)
                                                                                                                                        ((lambda (r$454)
                                                                                                                                           ((lambda (r$283)
                                                                                                                                              ((lambda (r$449)
                                                                                                                                                 ((lambda (r$284)
                                                                                                                                                    ((lambda (r$429)
                                                                                                                                                       ((lambda (r$285)
                                                                                                                                                          ((lambda (r$428)
                                                                                                                                                             ((lambda (r$286)
                                                                                                                                                                ((lambda (r$287)
                                                                                                                                                                   ((lambda (r$417)
                                                                                                                                                                      ((lambda (r$288)
                                                                                                                                                                         ((lambda (r$410)
                                                                                                                                                                            ((lambda (r$289)
                                                                                                                                                                               ((lambda (r$400)
                                                                                                                                                                                  ((lambda (r$290)
                                                                                                                                                                                     ((lambda (r$399)
                                                                                                                                                                                        ((lambda (r$291)
                                                                                                                                                                                           ((lambda (r$395)
                                                                                                                                                                                              ((lambda (r$292)
                                                                                                                                                                                                 ((lambda (r$380)
                                                                                                                                                                                                    ((lambda (r$293)
                                                                                                                                                                                                       ((lambda (r$371)
                                                                                                                                                                                                          ((lambda (r$294)
                                                                                                                                                                                                             ((lambda (r$368)
                                                                                                                                                                                                                ((lambda (r$295)
                                                                                                                                                                                                                   ((lambda (r$365)
                                                                                                                                                                                                                      ((lambda (r$296)
                                                                                                                                                                                                                         ((lambda (r$364)
                                                                                                                                                                                                                            ((lambda (r$297)
                                                                                                                                                                                                                               ((lambda (r$363)
                                                                                                                                                                                                                                  ((lambda (r$298)
                                                                                                                                                                                                                                     ((lambda (r$356)
                                                                                                                                                                                                                                        ((lambda (r$299)
                                                                                                                                                                                                                                           ((lambda (r$346)
                                                                                                                                                                                                                                              ((lambda (r$300)
                                                                                                                                                                                                                                                 ((lambda (r$337)
                                                                                                                                                                                                                                                    ((lambda (r$301)
                                                                                                                                                                                                                                                       ((lambda (r$328)
                                                                                                                                                                                                                                                          ((lambda (r$302)
                                                                                                                                                                                                                                                             ((lambda (r$322)
                                                                                                                                                                                                                                                                ((lambda (r$303)
                                                                                                                                                                                                                                                                   ((lambda (r$309)
                                                                                                                                                                                                                                                                      ((lambda (r$304)
                                                                                                                                                                                                                                                                         ((lambda (r$305)
                                                                                                                                                                                                                                                                            ((lambda (r$264) (main %halt))
                                                                                                                                                                                                                                                                             (set! test-boyer r$305)))
                                                                                                                                                                                                                                                                          (lambda (k$306 alist$160 term$159 n$158)
                                                                                                                                                                                                                                                                            ((lambda (r$307)
                                                                                                                                                                                                                                                                               (test$142
                                                                                                                                                                                                                                                                                 (lambda (r$308)
                                                                                                                                                                                                                                                                                   ((lambda (answer$161)
                                                                                                                                                                                                                                                                                      (if answer$161
                                                                                                                                                                                                                                                                                        (k$306 rewrite-count$135)
                                                                                                                                                                                                                                                                                        (k$306 #f)))
                                                                                                                                                                                                                                                                                    r$308))
                                                                                                                                                                                                                                                                                 alist$160
                                                                                                                                                                                                                                                                                 term$159
                                                                                                                                                                                                                                                                                 n$158))
                                                                                                                                                                                                                                                                             (set! rewrite-count$135 0)))))
                                                                                                                                                                                                                                                                       (set! setup-boyer r$309)))
                                                                                                                                                                                                                                                                    (lambda (k$310)
                                                                                                                                                                                                                                                                      ((lambda (r$321)
                                                                                                                                                                                                                                                                         ((lambda (r$311)
                                                                                                                                                                                                                                                                            ((lambda (r$320)
                                                                                                                                                                                                                                                                               (symbol->symbol-record$149
                                                                                                                                                                                                                                                                                 (lambda (r$319)
                                                                                                                                                                                                                                                                                   ((lambda (r$312)
                                                                                                                                                                                                                                                                                      ((lambda (r$318)
                                                                                                                                                                                                                                                                                         (translate-term$154
                                                                                                                                                                                                                                                                                           (lambda (r$317)
                                                                                                                                                                                                                                                                                             ((lambda (r$313)
                                                                                                                                                                                                                                                                                                ((lambda (r$316)
                                                                                                                                                                                                                                                                                                   (translate-term$154
                                                                                                                                                                                                                                                                                                     (lambda (r$315)
                                                                                                                                                                                                                                                                                                       ((lambda (r$314) (setup$157 k$310))
                                                                                                                                                                                                                                                                                                        (set! true-term$124 r$315)))
                                                                                                                                                                                                                                                                                                     r$316))
                                                                                                                                                                                                                                                                                                 '(t)))
                                                                                                                                                                                                                                                                                              (set! false-term$125 r$317)))
                                                                                                                                                                                                                                                                                           r$318))
                                                                                                                                                                                                                                                                                       '(f)))
                                                                                                                                                                                                                                                                                    (set! if-constructor$136 r$319)))
                                                                                                                                                                                                                                                                                 r$320))
                                                                                                                                                                                                                                                                             'if))
                                                                                                                                                                                                                                                                          (set! *symbol-records-alist*$148 r$321)))
                                                                                                                                                                                                                                                                       '()))))
                                                                                                                                                                                                                                                                 (set! term-member?$119 r$322)))
                                                                                                                                                                                                                                                              (lambda (k$323 x$163 lst$162)
                                                                                                                                                                                                                                                                ((lambda (r$324)
                                                                                                                                                                                                                                                                   (if r$324
                                                                                                                                                                                                                                                                     ((lambda () (k$323 #f)))
                                                                                                                                                                                                                                                                     ((lambda (r$327)
                                                                                                                                                                                                                                                                        (term-equal?$121
                                                                                                                                                                                                                                                                          (lambda (r$325)
                                                                                                                                                                                                                                                                            (if r$325
                                                                                                                                                                                                                                                                              ((lambda () (k$323 #t)))
                                                                                                                                                                                                                                                                              ((lambda ()
                                                                                                                                                                                                                                                                                 ((lambda (r$326)
                                                                                                                                                                                                                                                                                    (term-member?$119 k$323 x$163 r$326))
                                                                                                                                                                                                                                                                                  (cdr lst$162))))))
                                                                                                                                                                                                                                                                          x$163
                                                                                                                                                                                                                                                                          r$327))
                                                                                                                                                                                                                                                                      (car lst$162))))
                                                                                                                                                                                                                                                                 (null? lst$162)))))
                                                                                                                                                                                                                                                           (set! term-args-equal?$120 r$328)))
                                                                                                                                                                                                                                                        (lambda (k$329 lst1$165 lst2$164)
                                                                                                                                                                                                                                                          ((lambda (r$330)
                                                                                                                                                                                                                                                             (if r$330
                                                                                                                                                                                                                                                               ((lambda () (k$329 (null? lst2$164))))
                                                                                                                                                                                                                                                               ((lambda (r$331)
                                                                                                                                                                                                                                                                  (if r$331
                                                                                                                                                                                                                                                                    ((lambda () (k$329 #f)))
                                                                                                                                                                                                                                                                    ((lambda (r$335)
                                                                                                                                                                                                                                                                       ((lambda (r$336)
                                                                                                                                                                                                                                                                          (term-equal?$121
                                                                                                                                                                                                                                                                            (lambda (r$332)
                                                                                                                                                                                                                                                                              (if r$332
                                                                                                                                                                                                                                                                                ((lambda ()
                                                                                                                                                                                                                                                                                   ((lambda (r$333)
                                                                                                                                                                                                                                                                                      ((lambda (r$334)
                                                                                                                                                                                                                                                                                         (term-args-equal?$120 k$329 r$333 r$334))
                                                                                                                                                                                                                                                                                       (cdr lst2$164)))
                                                                                                                                                                                                                                                                                    (cdr lst1$165))))
                                                                                                                                                                                                                                                                                ((lambda () (k$329 #f)))))
                                                                                                                                                                                                                                                                            r$335
                                                                                                                                                                                                                                                                            r$336))
                                                                                                                                                                                                                                                                        (car lst2$164)))
                                                                                                                                                                                                                                                                     (car lst1$165))))
                                                                                                                                                                                                                                                                (null? lst2$164))))
                                                                                                                                                                                                                                                           (null? lst1$165)))))
                                                                                                                                                                                                                                                     (set! term-equal?$121 r$337)))
                                                                                                                                                                                                                                                  (lambda (k$338 x$167 y$166)
                                                                                                                                                                                                                                                    ((lambda (r$339)
                                                                                                                                                                                                                                                       (if r$339
                                                                                                                                                                                                                                                         ((lambda ()
                                                                                                                                                                                                                                                            ((lambda (r$340)
                                                                                                                                                                                                                                                               (if r$340
                                                                                                                                                                                                                                                                 ((lambda (r$344)
                                                                                                                                                                                                                                                                    ((lambda (r$345)
                                                                                                                                                                                                                                                                       (symbol-record-equal?$143
                                                                                                                                                                                                                                                                         (lambda (r$341)
                                                                                                                                                                                                                                                                           (if r$341
                                                                                                                                                                                                                                                                             ((lambda (r$342)
                                                                                                                                                                                                                                                                                ((lambda (r$343)
                                                                                                                                                                                                                                                                                   (term-args-equal?$120 k$338 r$342 r$343))
                                                                                                                                                                                                                                                                                 (cdr y$166)))
                                                                                                                                                                                                                                                                              (cdr x$167))
                                                                                                                                                                                                                                                                             (k$338 #f)))
                                                                                                                                                                                                                                                                         r$344
                                                                                                                                                                                                                                                                         r$345))
                                                                                                                                                                                                                                                                     (car y$166)))
                                                                                                                                                                                                                                                                  (car x$167))
                                                                                                                                                                                                                                                                 (k$338 #f)))
                                                                                                                                                                                                                                                             (pair? y$166))))
                                                                                                                                                                                                                                                         ((lambda () (k$338 (equal? x$167 y$166))))))
                                                                                                                                                                                                                                                     (pair? x$167)))))
                                                                                                                                                                                                                                               (set! trans-of-implies1$122 r$346)))
                                                                                                                                                                                                                                            (lambda (k$347 n$168)
                                                                                                                                                                                                                                              ((lambda (r$348)
                                                                                                                                                                                                                                                 (if r$348
                                                                                                                                                                                                                                                   ((lambda ()
                                                                                                                                                                                                                                                      ((lambda (r$349) (list k$347 r$349 0 1))
                                                                                                                                                                                                                                                       'implies)))
                                                                                                                                                                                                                                                   ((lambda ()
                                                                                                                                                                                                                                                      ((lambda (r$350)
                                                                                                                                                                                                                                                         ((lambda (r$354)
                                                                                                                                                                                                                                                            ((lambda (r$355)
                                                                                                                                                                                                                                                               (list (lambda (r$351)
                                                                                                                                                                                                                                                                       ((lambda (r$353)
                                                                                                                                                                                                                                                                          (trans-of-implies1$122
                                                                                                                                                                                                                                                                            (lambda (r$352) (list k$347 r$350 r$351 r$352))
                                                                                                                                                                                                                                                                            r$353))
                                                                                                                                                                                                                                                                        (- n$168 1)))
                                                                                                                                                                                                                                                                     r$354
                                                                                                                                                                                                                                                                     r$355
                                                                                                                                                                                                                                                                     n$168))
                                                                                                                                                                                                                                                             (- n$168 1)))
                                                                                                                                                                                                                                                          'implies))
                                                                                                                                                                                                                                                       'and)))))
                                                                                                                                                                                                                                               (equal? n$168 1)))))
                                                                                                                                                                                                                                         (set! trans-of-implies$123 r$356)))
                                                                                                                                                                                                                                      (lambda (k$357 n$169)
                                                                                                                                                                                                                                        ((lambda (r$359)
                                                                                                                                                                                                                                           (trans-of-implies1$122
                                                                                                                                                                                                                                             (lambda (r$360)
                                                                                                                                                                                                                                               ((lambda (r$362)
                                                                                                                                                                                                                                                  (list (lambda (r$361)
                                                                                                                                                                                                                                                          (list (lambda (r$358) (translate-term$154 k$357 r$358))
                                                                                                                                                                                                                                                                r$359
                                                                                                                                                                                                                                                                r$360
                                                                                                                                                                                                                                                                r$361))
                                                                                                                                                                                                                                                        r$362
                                                                                                                                                                                                                                                        0
                                                                                                                                                                                                                                                        n$169))
                                                                                                                                                                                                                                                'implies))
                                                                                                                                                                                                                                             n$169))
                                                                                                                                                                                                                                         'implies))))
                                                                                                                                                                                                                                   (set! true-term$124 r$363)))
                                                                                                                                                                                                                                '*))
                                                                                                                                                                                                                             (set! false-term$125 r$364)))
                                                                                                                                                                                                                          '*))
                                                                                                                                                                                                                       (set! truep$126 r$365)))
                                                                                                                                                                                                                    (lambda (k$366 x$171 lst$170)
                                                                                                                                                                                                                      (term-equal?$121
                                                                                                                                                                                                                        (lambda (r$367)
                                                                                                                                                                                                                          ((lambda (tmp$172)
                                                                                                                                                                                                                             (if tmp$172
                                                                                                                                                                                                                               (k$366 tmp$172)
                                                                                                                                                                                                                               (term-member?$119 k$366 x$171 lst$170)))
                                                                                                                                                                                                                           r$367))
                                                                                                                                                                                                                        x$171
                                                                                                                                                                                                                        true-term$124))))
                                                                                                                                                                                                                 (set! falsep$127 r$368)))
                                                                                                                                                                                                              (lambda (k$369 x$174 lst$173)
                                                                                                                                                                                                                (term-equal?$121
                                                                                                                                                                                                                  (lambda (r$370)
                                                                                                                                                                                                                    ((lambda (tmp$175)
                                                                                                                                                                                                                       (if tmp$175
                                                                                                                                                                                                                         (k$369 tmp$175)
                                                                                                                                                                                                                         (term-member?$119 k$369 x$174 lst$173)))
                                                                                                                                                                                                                     r$370))
                                                                                                                                                                                                                  x$174
                                                                                                                                                                                                                  false-term$125))))
                                                                                                                                                                                                           (set! one-way-unify1-lst$128 r$371)))
                                                                                                                                                                                                        (lambda (k$372 lst1$177 lst2$176)
                                                                                                                                                                                                          ((lambda (r$373)
                                                                                                                                                                                                             (if r$373
                                                                                                                                                                                                               ((lambda () (k$372 (null? lst2$176))))
                                                                                                                                                                                                               ((lambda (r$374)
                                                                                                                                                                                                                  (if r$374
                                                                                                                                                                                                                    ((lambda () (k$372 #f)))
                                                                                                                                                                                                                    ((lambda (r$378)
                                                                                                                                                                                                                       ((lambda (r$379)
                                                                                                                                                                                                                          (one-way-unify1$129
                                                                                                                                                                                                                            (lambda (r$375)
                                                                                                                                                                                                                              (if r$375
                                                                                                                                                                                                                                ((lambda ()
                                                                                                                                                                                                                                   ((lambda (r$376)
                                                                                                                                                                                                                                      ((lambda (r$377)
                                                                                                                                                                                                                                         (one-way-unify1-lst$128 k$372 r$376 r$377))
                                                                                                                                                                                                                                       (cdr lst2$176)))
                                                                                                                                                                                                                                    (cdr lst1$177))))
                                                                                                                                                                                                                                ((lambda () (k$372 #f)))))
                                                                                                                                                                                                                            r$378
                                                                                                                                                                                                                            r$379))
                                                                                                                                                                                                                        (car lst2$176)))
                                                                                                                                                                                                                     (car lst1$177))))
                                                                                                                                                                                                                (null? lst2$176))))
                                                                                                                                                                                                           (null? lst1$177)))))
                                                                                                                                                                                                     (set! one-way-unify1$129 r$380)))
                                                                                                                                                                                                  (lambda (k$381 term1$179 term2$178)
                                                                                                                                                                                                    ((lambda (r$382)
                                                                                                                                                                                                       (if r$382
                                                                                                                                                                                                         ((lambda (r$383)
                                                                                                                                                                                                            (if r$383
                                                                                                                                                                                                              ((lambda (r$387)
                                                                                                                                                                                                                 ((lambda (r$388)
                                                                                                                                                                                                                    ((lambda (r$384)
                                                                                                                                                                                                                       (if r$384
                                                                                                                                                                                                                         ((lambda ()
                                                                                                                                                                                                                            ((lambda (r$385)
                                                                                                                                                                                                                               ((lambda (r$386)
                                                                                                                                                                                                                                  (one-way-unify1-lst$128 k$381 r$385 r$386))
                                                                                                                                                                                                                                (cdr term2$178)))
                                                                                                                                                                                                                             (cdr term1$179))))
                                                                                                                                                                                                                         ((lambda () (k$381 #f)))))
                                                                                                                                                                                                                     (eq? r$387 r$388)))
                                                                                                                                                                                                                  (car term2$178)))
                                                                                                                                                                                                               (car term1$179))
                                                                                                                                                                                                              ((lambda () (k$381 #f)))))
                                                                                                                                                                                                          (pair? term1$179))
                                                                                                                                                                                                         ((lambda ()
                                                                                                                                                                                                            ((lambda (r$389)
                                                                                                                                                                                                               ((lambda (temp-temp$180)
                                                                                                                                                                                                                  (if temp-temp$180
                                                                                                                                                                                                                    ((lambda ()
                                                                                                                                                                                                                       ((lambda (r$390)
                                                                                                                                                                                                                          (term-equal?$121 k$381 term1$179 r$390))
                                                                                                                                                                                                                        (cdr temp-temp$180))))
                                                                                                                                                                                                                    ((lambda (r$391)
                                                                                                                                                                                                                       (if r$391
                                                                                                                                                                                                                         ((lambda () (k$381 (equal? term1$179 term2$178))))
                                                                                                                                                                                                                         ((lambda ()
                                                                                                                                                                                                                            ((lambda (r$394)
                                                                                                                                                                                                                               ((lambda (r$393)
                                                                                                                                                                                                                                  ((lambda (r$392) (k$381 #t))
                                                                                                                                                                                                                                   (set! unify-subst$131 r$393)))
                                                                                                                                                                                                                                (cons r$394 unify-subst$131)))
                                                                                                                                                                                                                             (cons term2$178 term1$179))))))
                                                                                                                                                                                                                     (number? term2$178))))
                                                                                                                                                                                                                r$389))
                                                                                                                                                                                                             (assq term2$178 unify-subst$131))))))
                                                                                                                                                                                                     (pair? term2$178)))))
                                                                                                                                                                                               (set! one-way-unify$130 r$395)))
                                                                                                                                                                                            (lambda (k$396 term1$182 term2$181)
                                                                                                                                                                                              ((lambda (r$398)
                                                                                                                                                                                                 ((lambda (r$397)
                                                                                                                                                                                                    (one-way-unify1$129 k$396 term1$182 term2$181))
                                                                                                                                                                                                  (set! unify-subst$131 r$398)))
                                                                                                                                                                                               '()))))
                                                                                                                                                                                         (set! unify-subst$131 r$399)))
                                                                                                                                                                                      '*))
                                                                                                                                                                                   (set! rewrite-with-lemmas$132 r$400)))
                                                                                                                                                                                (lambda (k$401 term$184 lst$183)
                                                                                                                                                                                  ((lambda (r$402)
                                                                                                                                                                                     (if r$402
                                                                                                                                                                                       ((lambda () (k$401 term$184)))
                                                                                                                                                                                       ((lambda (r$409)
                                                                                                                                                                                          ((lambda (r$408)
                                                                                                                                                                                             (one-way-unify$130
                                                                                                                                                                                               (lambda (r$403)
                                                                                                                                                                                                 (if r$403
                                                                                                                                                                                                   ((lambda ()
                                                                                                                                                                                                      ((lambda (r$406)
                                                                                                                                                                                                         ((lambda (r$405)
                                                                                                                                                                                                            (apply-subst$140
                                                                                                                                                                                                              (lambda (r$404) (rewrite$134 k$401 r$404))
                                                                                                                                                                                                              unify-subst$131
                                                                                                                                                                                                              r$405))
                                                                                                                                                                                                          (caddr r$406)))
                                                                                                                                                                                                       (car lst$183))))
                                                                                                                                                                                                   ((lambda ()
                                                                                                                                                                                                      ((lambda (r$407)
                                                                                                                                                                                                         (rewrite-with-lemmas$132 k$401 term$184 r$407))
                                                                                                                                                                                                       (cdr lst$183))))))
                                                                                                                                                                                               term$184
                                                                                                                                                                                               r$408))
                                                                                                                                                                                           (cadr r$409)))
                                                                                                                                                                                        (car lst$183))))
                                                                                                                                                                                   (null? lst$183)))))
                                                                                                                                                                             (set! rewrite-args$133 r$410)))
                                                                                                                                                                          (lambda (k$411 lst$185)
                                                                                                                                                                            ((lambda (r$412)
                                                                                                                                                                               (if r$412
                                                                                                                                                                                 ((lambda () (k$411 '())))
                                                                                                                                                                                 ((lambda ()
                                                                                                                                                                                    ((lambda (r$416)
                                                                                                                                                                                       (rewrite$134
                                                                                                                                                                                         (lambda (r$413)
                                                                                                                                                                                           ((lambda (r$415)
                                                                                                                                                                                              (rewrite-args$133
                                                                                                                                                                                                (lambda (r$414) (k$411 (cons r$413 r$414)))
                                                                                                                                                                                                r$415))
                                                                                                                                                                                            (cdr lst$185)))
                                                                                                                                                                                         r$416))
                                                                                                                                                                                     (car lst$185))))))
                                                                                                                                                                             (null? lst$185)))))
                                                                                                                                                                       (set! rewrite$134 r$417)))
                                                                                                                                                                    (lambda (k$418 term$186)
                                                                                                                                                                      ((lambda (r$427)
                                                                                                                                                                         ((lambda (r$419)
                                                                                                                                                                            ((lambda (r$420)
                                                                                                                                                                               (if r$420
                                                                                                                                                                                 ((lambda ()
                                                                                                                                                                                    ((lambda (r$424)
                                                                                                                                                                                       ((lambda (r$426)
                                                                                                                                                                                          (rewrite-args$133
                                                                                                                                                                                            (lambda (r$425)
                                                                                                                                                                                              ((lambda (r$421)
                                                                                                                                                                                                 ((lambda (r$423)
                                                                                                                                                                                                    (get-lemmas$145
                                                                                                                                                                                                      (lambda (r$422)
                                                                                                                                                                                                        (rewrite-with-lemmas$132 k$418 r$421 r$422))
                                                                                                                                                                                                      r$423))
                                                                                                                                                                                                  (car term$186)))
                                                                                                                                                                                               (cons r$424 r$425)))
                                                                                                                                                                                            r$426))
                                                                                                                                                                                        (cdr term$186)))
                                                                                                                                                                                     (car term$186))))
                                                                                                                                                                                 ((lambda () (k$418 term$186)))))
                                                                                                                                                                             (pair? term$186)))
                                                                                                                                                                          (set! rewrite-count$135 r$427)))
                                                                                                                                                                       (+ rewrite-count$135 1)))))
                                                                                                                                                                 (set! rewrite-count$135 0)))
                                                                                                                                                              (set! if-constructor$136 r$428)))
                                                                                                                                                           '*))
                                                                                                                                                        (set! tautologyp$137 r$429)))
                                                                                                                                                     (lambda (k$430 x$189 true-lst$188 false-lst$187)
                                                                                                                                                       (truep$126
                                                                                                                                                         (lambda (r$431)
                                                                                                                                                           (if r$431
                                                                                                                                                             ((lambda () (k$430 #t)))
                                                                                                                                                             (falsep$127
                                                                                                                                                               (lambda (r$432)
                                                                                                                                                                 (if r$432
                                                                                                                                                                   ((lambda () (k$430 #f)))
                                                                                                                                                                   ((lambda (r$433)
                                                                                                                                                                      (if r$433
                                                                                                                                                                        ((lambda (r$448)
                                                                                                                                                                           ((lambda (r$434)
                                                                                                                                                                              (if r$434
                                                                                                                                                                                ((lambda ()
                                                                                                                                                                                   ((lambda (r$447)
                                                                                                                                                                                      (truep$126
                                                                                                                                                                                        (lambda (r$435)
                                                                                                                                                                                          (if r$435
                                                                                                                                                                                            ((lambda ()
                                                                                                                                                                                               ((lambda (r$436)
                                                                                                                                                                                                  (tautologyp$137
                                                                                                                                                                                                    k$430
                                                                                                                                                                                                    r$436
                                                                                                                                                                                                    true-lst$188
                                                                                                                                                                                                    false-lst$187))
                                                                                                                                                                                                (caddr x$189))))
                                                                                                                                                                                            ((lambda (r$446)
                                                                                                                                                                                               (falsep$127
                                                                                                                                                                                                 (lambda (r$437)
                                                                                                                                                                                                   (if r$437
                                                                                                                                                                                                     ((lambda ()
                                                                                                                                                                                                        ((lambda (r$438)
                                                                                                                                                                                                           (tautologyp$137
                                                                                                                                                                                                             k$430
                                                                                                                                                                                                             r$438
                                                                                                                                                                                                             true-lst$188
                                                                                                                                                                                                             false-lst$187))
                                                                                                                                                                                                         (cadddr x$189))))
                                                                                                                                                                                                     ((lambda ()
                                                                                                                                                                                                        ((lambda (r$443)
                                                                                                                                                                                                           ((lambda (r$445)
                                                                                                                                                                                                              ((lambda (r$444)
                                                                                                                                                                                                                 (tautologyp$137
                                                                                                                                                                                                                   (lambda (r$439)
                                                                                                                                                                                                                     (if r$439
                                                                                                                                                                                                                       ((lambda (r$440)
                                                                                                                                                                                                                          ((lambda (r$442)
                                                                                                                                                                                                                             ((lambda (r$441)
                                                                                                                                                                                                                                (tautologyp$137 k$430 r$440 true-lst$188 r$441))
                                                                                                                                                                                                                              (cons r$442 false-lst$187)))
                                                                                                                                                                                                                           (cadr x$189)))
                                                                                                                                                                                                                        (cadddr x$189))
                                                                                                                                                                                                                       (k$430 #f)))
                                                                                                                                                                                                                   r$443
                                                                                                                                                                                                                   r$444
                                                                                                                                                                                                                   false-lst$187))
                                                                                                                                                                                                               (cons r$445 true-lst$188)))
                                                                                                                                                                                                            (cadr x$189)))
                                                                                                                                                                                                         (caddr x$189))))))
                                                                                                                                                                                                 r$446
                                                                                                                                                                                                 false-lst$187))
                                                                                                                                                                                             (cadr x$189))))
                                                                                                                                                                                        r$447
                                                                                                                                                                                        true-lst$188))
                                                                                                                                                                                    (cadr x$189))))
                                                                                                                                                                                ((lambda () (k$430 #f)))))
                                                                                                                                                                            (eq? r$448 if-constructor$136)))
                                                                                                                                                                         (car x$189))
                                                                                                                                                                        ((lambda () (k$430 #f)))))
                                                                                                                                                                    (pair? x$189))))
                                                                                                                                                               x$189
                                                                                                                                                               false-lst$187)))
                                                                                                                                                         x$189
                                                                                                                                                         true-lst$188))))
                                                                                                                                                  (set! tautp$138 r$449)))
                                                                                                                                               (lambda (k$450 x$190)
                                                                                                                                                 (rewrite$134
                                                                                                                                                   (lambda (r$451)
                                                                                                                                                     ((lambda (r$452)
                                                                                                                                                        ((lambda (r$453)
                                                                                                                                                           (tautologyp$137 k$450 r$451 r$452 r$453))
                                                                                                                                                         '()))
                                                                                                                                                      '()))
                                                                                                                                                   x$190))))
                                                                                                                                            (set! apply-subst-lst$139 r$454)))
                                                                                                                                         (lambda (k$455 alist$192 lst$191)
                                                                                                                                           ((lambda (r$456)
                                                                                                                                              (if r$456
                                                                                                                                                ((lambda () (k$455 '())))
                                                                                                                                                ((lambda ()
                                                                                                                                                   ((lambda (r$460)
                                                                                                                                                      (apply-subst$140
                                                                                                                                                        (lambda (r$457)
                                                                                                                                                          ((lambda (r$459)
                                                                                                                                                             (apply-subst-lst$139
                                                                                                                                                               (lambda (r$458) (k$455 (cons r$457 r$458)))
                                                                                                                                                               alist$192
                                                                                                                                                               r$459))
                                                                                                                                                           (cdr lst$191)))
                                                                                                                                                        alist$192
                                                                                                                                                        r$460))
                                                                                                                                                    (car lst$191))))))
                                                                                                                                            (null? lst$191)))))
                                                                                                                                      (set! apply-subst$140 r$461)))
                                                                                                                                   (lambda (k$462 alist$194 term$193)
                                                                                                                                     ((lambda (r$463)
                                                                                                                                        (if r$463
                                                                                                                                          ((lambda ()
                                                                                                                                             ((lambda (r$464)
                                                                                                                                                ((lambda (r$466)
                                                                                                                                                   (apply-subst-lst$139
                                                                                                                                                     (lambda (r$465) (k$462 (cons r$464 r$465)))
                                                                                                                                                     alist$194
                                                                                                                                                     r$466))
                                                                                                                                                 (cdr term$193)))
                                                                                                                                              (car term$193))))
                                                                                                                                          ((lambda ()
                                                                                                                                             ((lambda (r$467)
                                                                                                                                                ((lambda (temp-temp$195)
                                                                                                                                                   (if temp-temp$195
                                                                                                                                                     (k$462 (cdr temp-temp$195))
                                                                                                                                                     (k$462 term$193)))
                                                                                                                                                 r$467))
                                                                                                                                              (assq term$193 alist$194))))))
                                                                                                                                      (pair? term$193)))))
                                                                                                                                (set! translate-alist$141 r$468)))
                                                                                                                             (lambda (k$469 alist$196)
                                                                                                                               ((lambda (r$470)
                                                                                                                                  (if r$470
                                                                                                                                    ((lambda () (k$469 '())))
                                                                                                                                    ((lambda ()
                                                                                                                                       ((lambda (r$474)
                                                                                                                                          ((lambda (r$476)
                                                                                                                                             (translate-term$154
                                                                                                                                               (lambda (r$475)
                                                                                                                                                 ((lambda (r$471)
                                                                                                                                                    ((lambda (r$473)
                                                                                                                                                       (translate-alist$141
                                                                                                                                                         (lambda (r$472) (k$469 (cons r$471 r$472)))
                                                                                                                                                         r$473))
                                                                                                                                                     (cdr alist$196)))
                                                                                                                                                  (cons r$474 r$475)))
                                                                                                                                               r$476))
                                                                                                                                           (cdar alist$196)))
                                                                                                                                        (caar alist$196))))))
                                                                                                                                (null? alist$196)))))
                                                                                                                          (set! test$142 r$477)))
                                                                                                                       (lambda (k$478 alist$199 term$198 n$197)
                                                                                                                         (translate-alist$141
                                                                                                                           (lambda (r$480)
                                                                                                                             ((lambda (term$201 n$200)
                                                                                                                                ((lambda (lp$202)
                                                                                                                                   ((lambda (r$484)
                                                                                                                                      ((lambda (r$483)
                                                                                                                                         (lp$202
                                                                                                                                           (lambda (r$482)
                                                                                                                                             (translate-term$154
                                                                                                                                               (lambda (r$481)
                                                                                                                                                 (apply-subst$140
                                                                                                                                                   (lambda (r$479)
                                                                                                                                                     ((lambda (term$205) (tautp$138 k$478 term$205))
                                                                                                                                                      r$479))
                                                                                                                                                   r$480
                                                                                                                                                   r$481))
                                                                                                                                               r$482))
                                                                                                                                           term$201
                                                                                                                                           n$200))
                                                                                                                                       (set! lp$202 r$484)))
                                                                                                                                    (lambda (k$485 term$204 n$203)
                                                                                                                                      (zero? (lambda (r$486)
                                                                                                                                               (if r$486
                                                                                                                                                 (k$485 term$204)
                                                                                                                                                 ((lambda (r$489)
                                                                                                                                                    ((lambda (r$490)
                                                                                                                                                       (list (lambda (r$487)
                                                                                                                                                               ((lambda (r$488) (lp$202 k$485 r$487 r$488))
                                                                                                                                                                (- n$203 1)))
                                                                                                                                                             r$489
                                                                                                                                                             term$204
                                                                                                                                                             r$490))
                                                                                                                                                     '(f)))
                                                                                                                                                  'or)))
                                                                                                                                             n$203))))
                                                                                                                                 #f))
                                                                                                                              term$198
                                                                                                                              n$197))
                                                                                                                           alist$199))))
                                                                                                                    (set! symbol-record-equal?$143 r$491)))
                                                                                                                 (lambda (k$492 r1$207 r2$206)
                                                                                                                   (k$492 (eq? r1$207 r2$206)))))
                                                                                                              (set! get-name$144 r$493)))
                                                                                                           (lambda (k$494 symbol-record$208)
                                                                                                             (k$494 (vector-ref symbol-record$208 0)))))
                                                                                                        (set! get-lemmas$145 r$495)))
                                                                                                     (lambda (k$496 symbol-record$209)
                                                                                                       (k$496 (vector-ref symbol-record$209 1)))))
                                                                                                  (set! put-lemmas!$146 r$497)))
                                                                                               (lambda (k$498 symbol-record$211 lemmas$210)
                                                                                                 (k$498 (vector-set! symbol-record$211 1 lemmas$210)))))
                                                                                            (set! make-symbol-record$147 r$499)))
                                                                                         (lambda (k$500 sym$212)
                                                                                           ((lambda (r$501) (vector k$500 sym$212 r$501))
                                                                                            '()))))
                                                                                      (set! *symbol-records-alist*$148 r$502)))
                                                                                   '()))
                                                                                (set! symbol->symbol-record$149 r$503)))
                                                                             (lambda (k$504 sym$213)
                                                                               ((lambda (r$505)
                                                                                  ((lambda (x$214)
                                                                                     (if x$214
                                                                                       (k$504 (cdr x$214))
                                                                                       (make-symbol-record$147
                                                                                         (lambda (r$506)
                                                                                           ((lambda (r$215)
                                                                                              ((lambda (r$509)
                                                                                                 ((lambda (r$508)
                                                                                                    ((lambda (r$507) (k$504 r$215))
                                                                                                     (set! *symbol-records-alist*$148 r$508)))
                                                                                                  (cons r$509 *symbol-records-alist*$148)))
                                                                                               (cons sym$213 r$215)))
                                                                                            r$506))
                                                                                         sym$213)))
                                                                                   r$505))
                                                                                (assq sym$213 *symbol-records-alist*$148)))))
                                                                          (set! get$150 r$510)))
                                                                       (lambda (k$511 sym$217 property$216)
                                                                         (symbol->symbol-record$149
                                                                           (lambda (r$512) (get-lemmas$145 k$511 r$512))
                                                                           sym$217))))
                                                                    (set! put$151 r$513)))
                                                                 (lambda (k$514 sym$220 property$219 value$218)
                                                                   (symbol->symbol-record$149
                                                                     (lambda (r$515)
                                                                       (put-lemmas!$146 k$514 r$515 value$218))
                                                                     sym$220))))
                                                              (set! untranslate-term$152 r$516)))
                                                           (lambda (k$517 term$221)
                                                             ((lambda (r$518)
                                                                (if r$518
                                                                  ((lambda ()
                                                                     ((lambda (r$522)
                                                                        (get-name$144
                                                                          (lambda (r$519)
                                                                            ((lambda (r$521)
                                                                               (map (lambda (r$520) (k$517 (cons r$519 r$520)))
                                                                                    untranslate-term$152
                                                                                    r$521))
                                                                             (cdr term$221)))
                                                                          r$522))
                                                                      (car term$221))))
                                                                  ((lambda () (k$517 term$221)))))
                                                              (pair? term$221)))))
                                                        (set! translate-args$153 r$523)))
                                                     (lambda (k$524 lst$222)
                                                       ((lambda (r$525)
                                                          (if r$525
                                                            ((lambda () (k$524 '())))
                                                            ((lambda ()
                                                               ((lambda (r$529)
                                                                  (translate-term$154
                                                                    (lambda (r$526)
                                                                      ((lambda (r$528)
                                                                         (translate-args$153
                                                                           (lambda (r$527) (k$524 (cons r$526 r$527)))
                                                                           r$528))
                                                                       (cdr lst$222)))
                                                                    r$529))
                                                                (car lst$222))))))
                                                        (null? lst$222)))))
                                                  (set! translate-term$154 r$530)))
                                               (lambda (k$531 term$223)
                                                 ((lambda (r$532)
                                                    (if r$532
                                                      ((lambda ()
                                                         ((lambda (r$536)
                                                            (symbol->symbol-record$149
                                                              (lambda (r$533)
                                                                ((lambda (r$535)
                                                                   (translate-args$153
                                                                     (lambda (r$534) (k$531 (cons r$533 r$534)))
                                                                     r$535))
                                                                 (cdr term$223)))
                                                              r$536))
                                                          (car term$223))))
                                                      ((lambda () (k$531 term$223)))))
                                                  (pair? term$223)))))
                                            (set! add-lemma$155 r$537)))
                                         (lambda (k$538 term$224)
                                           ((lambda (k$549)
                                              ((lambda (r$550)
                                                 (if r$550
                                                   ((lambda (r$553)
                                                      ((lambda (r$554)
                                                         ((lambda (r$551)
                                                            (if r$551
                                                              ((lambda (r$552) (k$549 (pair? r$552)))
                                                               (cadr term$224))
                                                              (k$549 #f)))
                                                          (eq? r$553 r$554)))
                                                       'equal))
                                                    (car term$224))
                                                   (k$549 #f)))
                                               (pair? term$224)))
                                            (lambda (r$539)
                                              (if r$539
                                                ((lambda ()
                                                   ((lambda (r$548)
                                                      ((lambda (r$540)
                                                         ((lambda (r$541)
                                                            (translate-term$154
                                                              (lambda (r$543)
                                                                ((lambda (r$547)
                                                                   ((lambda (r$545)
                                                                      ((lambda (r$546)
                                                                         (get$150
                                                                           (lambda (r$544)
                                                                             ((lambda (r$542)
                                                                                (put$151 k$538 r$540 r$541 r$542))
                                                                              (cons r$543 r$544)))
                                                                           r$545
                                                                           r$546))
                                                                       'lemmas))
                                                                    (car r$547)))
                                                                 (cadr term$224)))
                                                              term$224))
                                                          'lemmas))
                                                       (car r$548)))
                                                    (cadr term$224))))
                                                ((lambda ()
                                                   (error k$538
                                                          #f
                                                          "ADD-LEMMA did not like term:  "
                                                          term$224)))))))))
                                      (set! add-lemma-lst$156 r$555)))
                                   (lambda (k$556 lst$225)
                                     ((lambda (r$557)
                                        (if r$557
                                          ((lambda () (k$556 #t)))
                                          ((lambda ()
                                             ((lambda (r$560)
                                                (add-lemma$155
                                                  (lambda (r$558)
                                                    ((lambda (r$559) (add-lemma-lst$156 k$556 r$559))
                                                     (cdr lst$225)))
                                                  r$560))
                                              (car lst$225))))))
                                      (null? lst$225)))))
                                (set! setup$157 r$561)))
                             (lambda (k$562)
                               ((lambda (r$563) (add-lemma-lst$156 k$562 r$563))
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
                          #f))))
                    (set! term r$564)))
                 '(implies
                    (and (implies x y)
                         (and (implies y z) (and (implies z u) (implies u w))))
                    (implies x w))))
              (set! alist r$565)))
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
  #f))
#;1> 

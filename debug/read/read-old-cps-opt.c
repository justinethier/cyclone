((define read #f)
 (define *in-port-table* #f)
 (define reg-port
   #((record-marker)
     #((record-marker) #f (id args body))
     #(630
       (k$812 fp$262)
       ((#((record-marker)
           #((record-marker) #f (id args body))
           #(628
             (r$263)
             ((if r$263
                (#((record-marker)
                   #((record-marker) #f (id args body))
                   #(622 () ((k$812 r$263)))))
                (#((record-marker)
                   #((record-marker) #f (id args body))
                   #(627
                     ()
                     ((list #((record-marker)
                              #((record-marker) #f (id args body))
                              #(626
                                (r$817)
                                ((#((record-marker)
                                    #((record-marker) #f (id args body))
                                    #(625
                                      (r$814)
                                      ((#((record-marker)
                                          #((record-marker) #f (id args body))
                                          #(624
                                            (r$816)
                                            ((#((record-marker)
                                                #((record-marker)
                                                  #f
                                                  (id args body))
                                                #(623 (r$815) ((k$812 r$263))))
                                              (set! *in-port-table* r$816)))))
                                        (cons r$263 *in-port-table*)))))
                                  (set! r$263 r$817)))))
                            fp$262
                            #f
                            1
                            0)))))))))
         (assoc fp$262 *in-port-table*))))))
 (define in-port:read-buf!
   #((record-marker)
     #((record-marker) #f (id args body))
     #(621
       (k$807 ptbl$260)
       ((#((record-marker)
           #((record-marker) #f (id args body))
           #(619
             (result$261)
             ((in-port:set-buf!
                #((record-marker)
                  #((record-marker) #f (id args body))
                  #(618 (r$809) ((k$807 result$261))))
                ptbl$260
                #f))))
         (cadr ptbl$260))))))
 (define in-port:get-buf
   #((record-marker)
     #((record-marker) #f (id args body))
     #(617 (k$804 ptbl$259) ((k$804 (cadr ptbl$259))))))
 (define in-port:set-buf!
   #((record-marker)
     #((record-marker) #f (id args body))
     #(616
       (k$800 ptbl$258 buf$257)
       ((#((record-marker)
           #((record-marker) #f (id args body))
           #(615 (r$801) ((k$800 (set-car! r$801 buf$257)))))
         (cdr ptbl$258))))))
 (define in-port:get-lnum
   #((record-marker)
     #((record-marker) #f (id args body))
     #(614
       (k$797 ptbl$256)
       ((k$797 (caddr ptbl$256))))))
 (define in-port:set-lnum!
   #((record-marker)
     #((record-marker) #f (id args body))
     #(613
       (k$793 ptbl$255 lnum$254)
       ((#((record-marker)
           #((record-marker) #f (id args body))
           #(612
             (r$794)
             ((k$793 (set-car! r$794 lnum$254)))))
         (cddr ptbl$255))))))
 (define in-port:get-cnum
   #((record-marker)
     #((record-marker) #f (id args body))
     #(611
       (k$790 ptbl$253)
       ((k$790 (cadddr ptbl$253))))))
 (define in-port:set-cnum!
   #((record-marker)
     #((record-marker) #f (id args body))
     #(610
       (k$786 ptbl$252 cnum$251)
       ((#((record-marker)
           #((record-marker) #f (id args body))
           #(609
             (r$787)
             ((k$786 (set-car! r$787 cnum$251)))))
         (cdddr ptbl$252))))))
 (define add-tok
   #((record-marker)
     #((record-marker) #f (id args body))
     #(608
       (k$783 tok$250 toks$249)
       ((k$783 (cons tok$250 toks$249))))))
 (define get-toks
   #((record-marker)
     #((record-marker) #f (id args body))
     #(607
       (k$778 tok$248 toks$247)
       ((#((record-marker)
           #((record-marker) #f (id args body))
           #(606
             (r$779)
             ((if r$779
                (k$778 toks$247)
                (->tok #((record-marker)
                         #((record-marker) #f (id args body))
                         #(605 (r$780) ((add-tok k$778 r$780 toks$247))))
                       tok$248)))))
         (null? tok$248))))))
 (define ->tok
   #((record-marker)
     #((record-marker) #f (id args body))
     #(604
       (k$774 lst$246)
       ((reverse
          #((record-marker)
            #((record-marker) #f (id args body))
            #(603 (r$775) ((parse-atom k$774 r$775))))
          lst$246)))))
 (define dotted?
   #((record-marker)
     #((record-marker) #f (id args body))
     #(602
       (k$766 lst$245)
       ((#((record-marker)
           #((record-marker) #f (id args body))
           #(601
             (r$771)
             ((#((record-marker)
                 #((record-marker) #f (id args body))
                 #(600
                   (r$767)
                   ((if r$767
                      (reverse
                        #((record-marker)
                          #((record-marker) #f (id args body))
                          #(599
                            (r$770)
                            ((#((record-marker)
                                #((record-marker) #f (id args body))
                                #(598
                                  (r$768)
                                  ((#((record-marker)
                                      #((record-marker) #f (id args body))
                                      #(597
                                        (r$769)
                                        ((k$766 (equal? r$768 r$769)))))
                                    (string->symbol ".")))))
                              (cadr r$770)))))
                        lst$245)
                      (k$766 #f)))))
               (> r$771 2)))))
         (length lst$245))))))
 (define ->dotted-list
   #((record-marker)
     #((record-marker) #f (id args body))
     #(596
       (k$756 lst$244)
       ((#((record-marker)
           #((record-marker) #f (id args body))
           #(595
             (r$757)
             ((if r$757
                (#((record-marker)
                   #((record-marker) #f (id args body))
                   #(586 () ((k$756 '())))))
                (#((record-marker)
                   #((record-marker) #f (id args body))
                   #(594
                     (r$762)
                     ((#((record-marker)
                         #((record-marker) #f (id args body))
                         #(593
                           (r$763)
                           ((#((record-marker)
                               #((record-marker) #f (id args body))
                               #(592
                                 (r$758)
                                 ((if r$758
                                    (#((record-marker)
                                       #((record-marker) #f (id args body))
                                       #(587 () ((k$756 (cadr lst$244))))))
                                    (#((record-marker)
                                       #((record-marker) #f (id args body))
                                       #(591
                                         ()
                                         ((#((record-marker)
                                             #((record-marker)
                                               #f
                                               (id args body))
                                             #(590
                                               (r$759)
                                               ((#((record-marker)
                                                   #((record-marker)
                                                     #f
                                                     (id args body))
                                                   #(589
                                                     (r$761)
                                                     ((->dotted-list
                                                        #((record-marker)
                                                          #((record-marker)
                                                            #f
                                                            (id args body))
                                                          #(588
                                                            (r$760)
                                                            ((k$756 (cons r$759
                                                                          r$760)))))
                                                        r$761))))
                                                 (cdr lst$244)))))
                                           (car lst$244))))))))))
                             (equal? r$762 r$763)))))
                       (string->symbol ".")))))
                 (car lst$244))))))
         (null? lst$244))))))
 (define parse-error
   #((record-marker)
     #((record-marker) #f (id args body))
     #(585
       (k$750 msg$243 lnum$242 cnum$241)
       ((#((record-marker)
           #((record-marker) #f (id args body))
           #(584
             (r$752)
             ((#((record-marker)
                 #((record-marker) #f (id args body))
                 #(583
                   (r$753)
                   ((#((record-marker)
                       #((record-marker) #f (id args body))
                       #(582 (r$751) ((error k$750 r$751))))
                     (string-append
                       "Error (line "
                       r$752
                       ", char "
                       r$753
                       "): "
                       msg$243)))))
               (number->string cnum$241)))))
         (number->string lnum$242))))))
 (define parse/tok
   #((record-marker)
     #((record-marker) #f (id args body))
     #(581
       (k$739 fp$240
              tok$239
              toks$238
              all?$237
              comment?$236
              parens$235
              ptbl$234
              curr-char$233)
       ((#((record-marker)
           #((record-marker) #f (id args body))
           #(580
             (r$740)
             ((if r$740
                (#((record-marker)
                   #((record-marker) #f (id args body))
                   #(571
                     ()
                     ((#((record-marker)
                         #((record-marker) #f (id args body))
                         #(570
                           (r$741)
                           ((parse k$739
                                   fp$240
                                   r$741
                                   toks$238
                                   all?$237
                                   comment?$236
                                   parens$235
                                   ptbl$234))))
                       '())))))
                (if all?$237
                  (#((record-marker)
                     #((record-marker) #f (id args body))
                     #(575
                       ()
                       ((#((record-marker)
                           #((record-marker) #f (id args body))
                           #(574
                             (r$742)
                             ((->tok #((record-marker)
                                       #((record-marker) #f (id args body))
                                       #(573
                                         (r$744)
                                         ((add-tok
                                            #((record-marker)
                                              #((record-marker)
                                                #f
                                                (id args body))
                                              #(572
                                                (r$743)
                                                ((parse k$739
                                                        fp$240
                                                        r$742
                                                        r$743
                                                        all?$237
                                                        comment?$236
                                                        parens$235
                                                        ptbl$234))))
                                            r$744
                                            toks$238))))
                                     tok$239))))
                         '())))))
                  (#((record-marker)
                     #((record-marker) #f (id args body))
                     #(579
                       ()
                       ((in-port:set-buf!
                          #((record-marker)
                            #((record-marker) #f (id args body))
                            #(578
                              (r$745)
                              ((->tok #((record-marker)
                                        #((record-marker) #f (id args body))
                                        #(577
                                          (r$747)
                                          ((add-tok
                                             #((record-marker)
                                               #((record-marker)
                                                 #f
                                                 (id args body))
                                               #(576
                                                 (r$746)
                                                 ((k$739 (car r$746)))))
                                             r$747
                                             toks$238))))
                                      tok$239))))
                          ptbl$234
                          curr-char$233))))))))))
         (null? tok$239))))))
 (define parse
   #((record-marker)
     #((record-marker) #f (id args body))
     #(569
       (k$497 fp$202
              tok$201
              toks$200
              all?$199
              comment?$198
              parens$197
              ptbl$196)
       ((#((record-marker)
           #((record-marker) #f (id args body))
           #(568
             ()
             ((in-port:get-cnum
                #((record-marker)
                  #((record-marker) #f (id args body))
                  #(567
                    (r$736)
                    ((#((record-marker)
                        #((record-marker) #f (id args body))
                        #(566
                          (r$735)
                          ((in-port:set-cnum!
                             #((record-marker)
                               #((record-marker) #f (id args body))
                               #(565
                                 (r$498)
                                 ((#((record-marker)
                                     #((record-marker) #f (id args body))
                                     #(564
                                       (k$733)
                                       ((in-port:get-buf
                                          #((record-marker)
                                            #((record-marker) #f (id args body))
                                            #(563
                                              (r$734)
                                              ((if r$734
                                                 (in-port:read-buf!
                                                   k$733
                                                   ptbl$196)
                                                 (k$733 (read-char fp$202))))))
                                          ptbl$196))))
                                   #((record-marker)
                                     #((record-marker) #f (id args body))
                                     #(561
                                       (c$204)
                                       ((#((record-marker)
                                           #((record-marker) #f (id args body))
                                           #(560
                                             (r$500)
                                             ((if r$500
                                                (#((record-marker)
                                                   #((record-marker)
                                                     #f
                                                     (id args body))
                                                   #(277
                                                     ()
                                                     ((#((record-marker)
                                                         #((record-marker)
                                                           #f
                                                           (id args body))
                                                         #(276
                                                           (k$506)
                                                           ((#((record-marker)
                                                               #((record-marker)
                                                                 #f
                                                                 (id args body))
                                                               #(275
                                                                 (r$507)
                                                                 ((if r$507
                                                                    (in-port:get-lnum
                                                                      #((record-marker)
                                                                        #((record-marker)
                                                                          #f
                                                                          (id args
                                                                              body))
                                                                        #(274
                                                                          (r$508)
                                                                          ((in-port:get-cnum
                                                                             #((record-marker)
                                                                               #((record-marker)
                                                                                 #f
                                                                                 (id args
                                                                                     body))
                                                                               #(273
                                                                                 (r$509)
                                                                                 ((parse-error
                                                                                    k$506
                                                                                    "missing closing parenthesis"
                                                                                    r$508
                                                                                    r$509))))
                                                                             ptbl$196))))
                                                                      ptbl$196)
                                                                    (k$506 #f)))))
                                                             (> parens$197
                                                                0)))))
                                                       #((record-marker)
                                                         #((record-marker)
                                                           #f
                                                           (id args body))
                                                         #(272
                                                           (r$501)
                                                           ((if all?$199
                                                              (get-toks
                                                                #((record-marker)
                                                                  #((record-marker)
                                                                    #f
                                                                    (id args
                                                                        body))
                                                                  #(267
                                                                    (r$502)
                                                                    ((reverse
                                                                       k$497
                                                                       r$502))))
                                                                tok$201
                                                                toks$200)
                                                              (get-toks
                                                                #((record-marker)
                                                                  #((record-marker)
                                                                    #f
                                                                    (id args
                                                                        body))
                                                                  #(270
                                                                    (last$232)
                                                                    ((#((record-marker)
                                                                        #((record-marker)
                                                                          #f
                                                                          (id args
                                                                              body))
                                                                        #(269
                                                                          (r$505)
                                                                          ((#((record-marker)
                                                                              #((record-marker)
                                                                                #f
                                                                                (id args
                                                                                    body))
                                                                              #(268
                                                                                (r$504)
                                                                                ((if r$504
                                                                                   (k$497 (car last$232))
                                                                                   (k$497 c$204)))))
                                                                            (> r$505
                                                                               0)))))
                                                                      (length
                                                                        last$232)))))
                                                                tok$201
                                                                toks$200))))))))))
                                                (if comment?$198
                                                  (#((record-marker)
                                                     #((record-marker)
                                                       #f
                                                       (id args body))
                                                     #(286
                                                       ()
                                                       ((#((record-marker)
                                                           #((record-marker)
                                                             #f
                                                             (id args body))
                                                           #(285
                                                             (r$510)
                                                             ((if r$510
                                                                (#((record-marker)
                                                                   #((record-marker)
                                                                     #f
                                                                     (id args
                                                                         body))
                                                                   #(283
                                                                     ()
                                                                     ((in-port:get-lnum
                                                                        #((record-marker)
                                                                          #((record-marker)
                                                                            #f
                                                                            (id args
                                                                                body))
                                                                          #(282
                                                                            (r$515)
                                                                            ((#((record-marker)
                                                                                #((record-marker)
                                                                                  #f
                                                                                  (id args
                                                                                      body))
                                                                                #(281
                                                                                  (r$514)
                                                                                  ((in-port:set-lnum!
                                                                                     #((record-marker)
                                                                                       #((record-marker)
                                                                                         #f
                                                                                         (id args
                                                                                             body))
                                                                                       #(280
                                                                                         (r$511)
                                                                                         ((in-port:set-cnum!
                                                                                            #((record-marker)
                                                                                              #((record-marker)
                                                                                                #f
                                                                                                (id args
                                                                                                    body))
                                                                                              #(279
                                                                                                (r$512)
                                                                                                ((#((record-marker)
                                                                                                    #((record-marker)
                                                                                                      #f
                                                                                                      (id args
                                                                                                          body))
                                                                                                    #(278
                                                                                                      (r$513)
                                                                                                      ((parse k$497
                                                                                                              fp$202
                                                                                                              r$513
                                                                                                              toks$200
                                                                                                              all?$199
                                                                                                              #f
                                                                                                              parens$197
                                                                                                              ptbl$196))))
                                                                                                  '()))))
                                                                                            ptbl$196
                                                                                            0))))
                                                                                     ptbl$196
                                                                                     r$514))))
                                                                              (+ 1
                                                                                 r$515)))))
                                                                        ptbl$196)))))
                                                                (#((record-marker)
                                                                   #((record-marker)
                                                                     #f
                                                                     (id args
                                                                         body))
                                                                   #(284
                                                                     (r$516)
                                                                     ((parse k$497
                                                                             fp$202
                                                                             r$516
                                                                             toks$200
                                                                             all?$199
                                                                             #t
                                                                             parens$197
                                                                             ptbl$196))))
                                                                 '())))))
                                                         (eq? c$204
                                                              #\newline))))))
                                                  (char-whitespace?
                                                    #((record-marker)
                                                      #((record-marker)
                                                        #f
                                                        (id args body))
                                                      #(559
                                                        (r$517)
                                                        ((if r$517
                                                           (#((record-marker)
                                                              #((record-marker)
                                                                #f
                                                                (id args body))
                                                              #(295
                                                                ()
                                                                ((#((record-marker)
                                                                    #((record-marker)
                                                                      #f
                                                                      (id args
                                                                          body))
                                                                    #(294
                                                                      (k$522)
                                                                      ((#((record-marker)
                                                                          #((record-marker)
                                                                            #f
                                                                            (id args
                                                                                body))
                                                                          #(293
                                                                            (r$523)
                                                                            ((if r$523
                                                                               (in-port:get-lnum
                                                                                 #((record-marker)
                                                                                   #((record-marker)
                                                                                     #f
                                                                                     (id args
                                                                                         body))
                                                                                   #(292
                                                                                     (r$525)
                                                                                     ((#((record-marker)
                                                                                         #((record-marker)
                                                                                           #f
                                                                                           (id args
                                                                                               body))
                                                                                         #(291
                                                                                           (r$524)
                                                                                           ((in-port:set-lnum!
                                                                                              k$522
                                                                                              ptbl$196
                                                                                              r$524))))
                                                                                       (+ 1
                                                                                          r$525)))))
                                                                                 ptbl$196)
                                                                               (k$522 #f)))))
                                                                        (equal?
                                                                          c$204
                                                                          #\newline)))))
                                                                  #((record-marker)
                                                                    #((record-marker)
                                                                      #f
                                                                      (id args
                                                                          body))
                                                                    #(290
                                                                      (r$518)
                                                                      ((#((record-marker)
                                                                          #((record-marker)
                                                                            #f
                                                                            (id args
                                                                                body))
                                                                          #(289
                                                                            (k$520)
                                                                            ((#((record-marker)
                                                                                #((record-marker)
                                                                                  #f
                                                                                  (id args
                                                                                      body))
                                                                                #(288
                                                                                  (r$521)
                                                                                  ((if r$521
                                                                                     (in-port:set-cnum!
                                                                                       k$520
                                                                                       ptbl$196
                                                                                       0)
                                                                                     (k$520 #f)))))
                                                                              (equal?
                                                                                c$204
                                                                                #\newline)))))
                                                                        #((record-marker)
                                                                          #((record-marker)
                                                                            #f
                                                                            (id args
                                                                                body))
                                                                          #(287
                                                                            (r$519)
                                                                            ((parse/tok
                                                                               k$497
                                                                               fp$202
                                                                               tok$201
                                                                               toks$200
                                                                               all?$199
                                                                               #f
                                                                               parens$197
                                                                               ptbl$196
                                                                               c$204)))))))))))))
                                                           (#((record-marker)
                                                              #((record-marker)
                                                                #f
                                                                (id args body))
                                                              #(558
                                                                (r$526)
                                                                ((if r$526
                                                                   (#((record-marker)
                                                                      #((record-marker)
                                                                        #f
                                                                        (id args
                                                                            body))
                                                                      #(296
                                                                        ()
                                                                        ((parse/tok
                                                                           k$497
                                                                           fp$202
                                                                           tok$201
                                                                           toks$200
                                                                           all?$199
                                                                           #t
                                                                           parens$197
                                                                           ptbl$196
                                                                           c$204)))))
                                                                   (#((record-marker)
                                                                      #((record-marker)
                                                                        #f
                                                                        (id args
                                                                            body))
                                                                      #(557
                                                                        (r$527)
                                                                        ((if r$527
                                                                           (#((record-marker)
                                                                              #((record-marker)
                                                                                #f
                                                                                (id args
                                                                                    body))
                                                                              #(316
                                                                                ()
                                                                                ((#((record-marker)
                                                                                    #((record-marker)
                                                                                      #f
                                                                                      (id args
                                                                                          body))
                                                                                    #(315
                                                                                      (k$541)
                                                                                      ((if all?$199
                                                                                         (k$541 #f)
                                                                                         (#((record-marker)
                                                                                            #((record-marker)
                                                                                              #f
                                                                                              (id args
                                                                                                  body))
                                                                                            #(314
                                                                                              (r$542)
                                                                                              ((not k$541
                                                                                                    r$542))))
                                                                                          (null? tok$201))))))
                                                                                  #((record-marker)
                                                                                    #((record-marker)
                                                                                      #f
                                                                                      (id args
                                                                                          body))
                                                                                    #(313
                                                                                      (r$528)
                                                                                      ((if r$528
                                                                                         (#((record-marker)
                                                                                            #((record-marker)
                                                                                              #f
                                                                                              (id args
                                                                                                  body))
                                                                                            #(300
                                                                                              ()
                                                                                              ((in-port:set-buf!
                                                                                                 #((record-marker)
                                                                                                   #((record-marker)
                                                                                                     #f
                                                                                                     (id args
                                                                                                         body))
                                                                                                   #(299
                                                                                                     (r$529)
                                                                                                     ((->tok #((record-marker)
                                                                                                               #((record-marker)
                                                                                                                 #f
                                                                                                                 (id args
                                                                                                                     body))
                                                                                                               #(298
                                                                                                                 (r$531)
                                                                                                                 ((add-tok
                                                                                                                    #((record-marker)
                                                                                                                      #((record-marker)
                                                                                                                        #f
                                                                                                                        (id args
                                                                                                                            body))
                                                                                                                      #(297
                                                                                                                        (r$530)
                                                                                                                        ((k$497 (car r$530)))))
                                                                                                                    r$531
                                                                                                                    toks$200))))
                                                                                                             tok$201))))
                                                                                                 ptbl$196
                                                                                                 c$204)))))
                                                                                         (#((record-marker)
                                                                                            #((record-marker)
                                                                                              #f
                                                                                              (id args
                                                                                                  body))
                                                                                            #(312
                                                                                              ()
                                                                                              ((#((record-marker)
                                                                                                  #((record-marker)
                                                                                                    #f
                                                                                                    (id args
                                                                                                        body))
                                                                                                  #(311
                                                                                                    (r$539)
                                                                                                    ((#((record-marker)
                                                                                                        #((record-marker)
                                                                                                          #f
                                                                                                          (id args
                                                                                                              body))
                                                                                                        #(310
                                                                                                          (r$540)
                                                                                                          ((parse #((record-marker)
                                                                                                                    #((record-marker)
                                                                                                                      #f
                                                                                                                      (id args
                                                                                                                          body))
                                                                                                                    #(308
                                                                                                                      (sub$230)
                                                                                                                      ((#((record-marker)
                                                                                                                          #((record-marker)
                                                                                                                            #f
                                                                                                                            (id args
                                                                                                                                body))
                                                                                                                          #(307
                                                                                                                            (new-toks$231)
                                                                                                                            ((#((record-marker)
                                                                                                                                #((record-marker)
                                                                                                                                  #f
                                                                                                                                  (id args
                                                                                                                                      body))
                                                                                                                                #(306
                                                                                                                                  (r$538)
                                                                                                                                  ((list #((record-marker)
                                                                                                                                           #((record-marker)
                                                                                                                                             #f
                                                                                                                                             (id args
                                                                                                                                                 body))
                                                                                                                                           #(305
                                                                                                                                             (r$536)
                                                                                                                                             ((get-toks
                                                                                                                                                #((record-marker)
                                                                                                                                                  #((record-marker)
                                                                                                                                                    #f
                                                                                                                                                    (id args
                                                                                                                                                        body))
                                                                                                                                                  #(304
                                                                                                                                                    (r$537)
                                                                                                                                                    ((add-tok
                                                                                                                                                       #((record-marker)
                                                                                                                                                         #((record-marker)
                                                                                                                                                           #f
                                                                                                                                                           (id args
                                                                                                                                                               body))
                                                                                                                                                         #(303
                                                                                                                                                           (r$535)
                                                                                                                                                           ((#((record-marker)
                                                                                                                                                               #((record-marker)
                                                                                                                                                                 #f
                                                                                                                                                                 (id args
                                                                                                                                                                     body))
                                                                                                                                                               #(302
                                                                                                                                                                 (r$533)
                                                                                                                                                                 ((if all?$199
                                                                                                                                                                    (#((record-marker)
                                                                                                                                                                       #((record-marker)
                                                                                                                                                                         #f
                                                                                                                                                                         (id args
                                                                                                                                                                             body))
                                                                                                                                                                       #(301
                                                                                                                                                                         (r$534)
                                                                                                                                                                         ((parse k$497
                                                                                                                                                                                 fp$202
                                                                                                                                                                                 r$534
                                                                                                                                                                                 new-toks$231
                                                                                                                                                                                 all?$199
                                                                                                                                                                                 #f
                                                                                                                                                                                 parens$197
                                                                                                                                                                                 ptbl$196))))
                                                                                                                                                                     '())
                                                                                                                                                                    (k$497 (car new-toks$231))))))
                                                                                                                                                             (set! new-toks$231
                                                                                                                                                               r$535)))))
                                                                                                                                                       r$536
                                                                                                                                                       r$537))))
                                                                                                                                                tok$201
                                                                                                                                                toks$200))))
                                                                                                                                         r$538
                                                                                                                                         sub$230))))
                                                                                                                              'quote))))
                                                                                                                        #f))))
                                                                                                                  fp$202
                                                                                                                  r$539
                                                                                                                  r$540
                                                                                                                  #f
                                                                                                                  #f
                                                                                                                  0
                                                                                                                  ptbl$196))))
                                                                                                      '()))))
                                                                                                '()))))))))))))))
                                                                           (#((record-marker)
                                                                              #((record-marker)
                                                                                #f
                                                                                (id args
                                                                                    body))
                                                                              #(556
                                                                                (r$543)
                                                                                ((if r$543
                                                                                   (#((record-marker)
                                                                                      #((record-marker)
                                                                                        #f
                                                                                        (id args
                                                                                            body))
                                                                                      #(336
                                                                                        ()
                                                                                        ((#((record-marker)
                                                                                            #((record-marker)
                                                                                              #f
                                                                                              (id args
                                                                                                  body))
                                                                                            #(335
                                                                                              (k$557)
                                                                                              ((if all?$199
                                                                                                 (k$557 #f)
                                                                                                 (#((record-marker)
                                                                                                    #((record-marker)
                                                                                                      #f
                                                                                                      (id args
                                                                                                          body))
                                                                                                    #(334
                                                                                                      (r$558)
                                                                                                      ((not k$557
                                                                                                            r$558))))
                                                                                                  (null? tok$201))))))
                                                                                          #((record-marker)
                                                                                            #((record-marker)
                                                                                              #f
                                                                                              (id args
                                                                                                  body))
                                                                                            #(333
                                                                                              (r$544)
                                                                                              ((if r$544
                                                                                                 (#((record-marker)
                                                                                                    #((record-marker)
                                                                                                      #f
                                                                                                      (id args
                                                                                                          body))
                                                                                                    #(320
                                                                                                      ()
                                                                                                      ((in-port:set-buf!
                                                                                                         #((record-marker)
                                                                                                           #((record-marker)
                                                                                                             #f
                                                                                                             (id args
                                                                                                                 body))
                                                                                                           #(319
                                                                                                             (r$545)
                                                                                                             ((->tok #((record-marker)
                                                                                                                       #((record-marker)
                                                                                                                         #f
                                                                                                                         (id args
                                                                                                                             body))
                                                                                                                       #(318
                                                                                                                         (r$547)
                                                                                                                         ((add-tok
                                                                                                                            #((record-marker)
                                                                                                                              #((record-marker)
                                                                                                                                #f
                                                                                                                                (id args
                                                                                                                                    body))
                                                                                                                              #(317
                                                                                                                                (r$546)
                                                                                                                                ((k$497 (car r$546)))))
                                                                                                                            r$547
                                                                                                                            toks$200))))
                                                                                                                     tok$201))))
                                                                                                         ptbl$196
                                                                                                         c$204)))))
                                                                                                 (#((record-marker)
                                                                                                    #((record-marker)
                                                                                                      #f
                                                                                                      (id args
                                                                                                          body))
                                                                                                    #(332
                                                                                                      ()
                                                                                                      ((#((record-marker)
                                                                                                          #((record-marker)
                                                                                                            #f
                                                                                                            (id args
                                                                                                                body))
                                                                                                          #(331
                                                                                                            (r$555)
                                                                                                            ((#((record-marker)
                                                                                                                #((record-marker)
                                                                                                                  #f
                                                                                                                  (id args
                                                                                                                      body))
                                                                                                                #(330
                                                                                                                  (r$556)
                                                                                                                  ((parse #((record-marker)
                                                                                                                            #((record-marker)
                                                                                                                              #f
                                                                                                                              (id args
                                                                                                                                  body))
                                                                                                                            #(328
                                                                                                                              (sub$228)
                                                                                                                              ((#((record-marker)
                                                                                                                                  #((record-marker)
                                                                                                                                    #f
                                                                                                                                    (id args
                                                                                                                                        body))
                                                                                                                                  #(327
                                                                                                                                    (new-toks$229)
                                                                                                                                    ((#((record-marker)
                                                                                                                                        #((record-marker)
                                                                                                                                          #f
                                                                                                                                          (id args
                                                                                                                                              body))
                                                                                                                                        #(326
                                                                                                                                          (r$554)
                                                                                                                                          ((list #((record-marker)
                                                                                                                                                   #((record-marker)
                                                                                                                                                     #f
                                                                                                                                                     (id args
                                                                                                                                                         body))
                                                                                                                                                   #(325
                                                                                                                                                     (r$552)
                                                                                                                                                     ((get-toks
                                                                                                                                                        #((record-marker)
                                                                                                                                                          #((record-marker)
                                                                                                                                                            #f
                                                                                                                                                            (id args
                                                                                                                                                                body))
                                                                                                                                                          #(324
                                                                                                                                                            (r$553)
                                                                                                                                                            ((add-tok
                                                                                                                                                               #((record-marker)
                                                                                                                                                                 #((record-marker)
                                                                                                                                                                   #f
                                                                                                                                                                   (id args
                                                                                                                                                                       body))
                                                                                                                                                                 #(323
                                                                                                                                                                   (r$551)
                                                                                                                                                                   ((#((record-marker)
                                                                                                                                                                       #((record-marker)
                                                                                                                                                                         #f
                                                                                                                                                                         (id args
                                                                                                                                                                             body))
                                                                                                                                                                       #(322
                                                                                                                                                                         (r$549)
                                                                                                                                                                         ((if all?$199
                                                                                                                                                                            (#((record-marker)
                                                                                                                                                                               #((record-marker)
                                                                                                                                                                                 #f
                                                                                                                                                                                 (id args
                                                                                                                                                                                     body))
                                                                                                                                                                               #(321
                                                                                                                                                                                 (r$550)
                                                                                                                                                                                 ((parse k$497
                                                                                                                                                                                         fp$202
                                                                                                                                                                                         r$550
                                                                                                                                                                                         new-toks$229
                                                                                                                                                                                         all?$199
                                                                                                                                                                                         #f
                                                                                                                                                                                         parens$197
                                                                                                                                                                                         ptbl$196))))
                                                                                                                                                                             '())
                                                                                                                                                                            (k$497 (car new-toks$229))))))
                                                                                                                                                                     (set! new-toks$229
                                                                                                                                                                       r$551)))))
                                                                                                                                                               r$552
                                                                                                                                                               r$553))))
                                                                                                                                                        tok$201
                                                                                                                                                        toks$200))))
                                                                                                                                                 r$554
                                                                                                                                                 sub$228))))
                                                                                                                                      'quasiquote))))
                                                                                                                                #f))))
                                                                                                                          fp$202
                                                                                                                          r$555
                                                                                                                          r$556
                                                                                                                          #f
                                                                                                                          #f
                                                                                                                          0
                                                                                                                          ptbl$196))))
                                                                                                              '()))))
                                                                                                        '()))))))))))))))
                                                                                   (#((record-marker)
                                                                                      #((record-marker)
                                                                                        #f
                                                                                        (id args
                                                                                            body))
                                                                                      #(555
                                                                                        (r$559)
                                                                                        ((if r$559
                                                                                           (#((record-marker)
                                                                                              #((record-marker)
                                                                                                #f
                                                                                                (id args
                                                                                                    body))
                                                                                              #(372
                                                                                                ()
                                                                                                ((#((record-marker)
                                                                                                    #((record-marker)
                                                                                                      #f
                                                                                                      (id args
                                                                                                          body))
                                                                                                    #(371
                                                                                                      (k$587)
                                                                                                      ((if all?$199
                                                                                                         (k$587 #f)
                                                                                                         (#((record-marker)
                                                                                                            #((record-marker)
                                                                                                              #f
                                                                                                              (id args
                                                                                                                  body))
                                                                                                            #(370
                                                                                                              (r$588)
                                                                                                              ((not k$587
                                                                                                                    r$588))))
                                                                                                          (null? tok$201))))))
                                                                                                  #((record-marker)
                                                                                                    #((record-marker)
                                                                                                      #f
                                                                                                      (id args
                                                                                                          body))
                                                                                                    #(369
                                                                                                      (r$560)
                                                                                                      ((if r$560
                                                                                                         (#((record-marker)
                                                                                                            #((record-marker)
                                                                                                              #f
                                                                                                              (id args
                                                                                                                  body))
                                                                                                            #(340
                                                                                                              ()
                                                                                                              ((in-port:set-buf!
                                                                                                                 #((record-marker)
                                                                                                                   #((record-marker)
                                                                                                                     #f
                                                                                                                     (id args
                                                                                                                         body))
                                                                                                                   #(339
                                                                                                                     (r$561)
                                                                                                                     ((->tok #((record-marker)
                                                                                                                               #((record-marker)
                                                                                                                                 #f
                                                                                                                                 (id args
                                                                                                                                     body))
                                                                                                                               #(338
                                                                                                                                 (r$563)
                                                                                                                                 ((add-tok
                                                                                                                                    #((record-marker)
                                                                                                                                      #((record-marker)
                                                                                                                                        #f
                                                                                                                                        (id args
                                                                                                                                            body))
                                                                                                                                      #(337
                                                                                                                                        (r$562)
                                                                                                                                        ((k$497 (car r$562)))))
                                                                                                                                    r$563
                                                                                                                                    toks$200))))
                                                                                                                             tok$201))))
                                                                                                                 ptbl$196
                                                                                                                 c$204)))))
                                                                                                         (#((record-marker)
                                                                                                            #((record-marker)
                                                                                                              #f
                                                                                                              (id args
                                                                                                                  body))
                                                                                                            #(368
                                                                                                              ()
                                                                                                              ((#((record-marker)
                                                                                                                  #((record-marker)
                                                                                                                    #f
                                                                                                                    (id args
                                                                                                                        body))
                                                                                                                  #(367
                                                                                                                    (sub$227
                                                                                                                      next-c$226
                                                                                                                      unquote-sym$225
                                                                                                                      new-toks$224)
                                                                                                                    ((#((record-marker)
                                                                                                                        #((record-marker)
                                                                                                                          #f
                                                                                                                          (id args
                                                                                                                              body))
                                                                                                                        #(366
                                                                                                                          (r$564)
                                                                                                                          ((#((record-marker)
                                                                                                                              #((record-marker)
                                                                                                                                #f
                                                                                                                                (id args
                                                                                                                                    body))
                                                                                                                              #(365
                                                                                                                                (r$586)
                                                                                                                                ((#((record-marker)
                                                                                                                                    #((record-marker)
                                                                                                                                      #f
                                                                                                                                      (id args
                                                                                                                                          body))
                                                                                                                                    #(364
                                                                                                                                      (r$565)
                                                                                                                                      ((#((record-marker)
                                                                                                                                          #((record-marker)
                                                                                                                                            #f
                                                                                                                                            (id args
                                                                                                                                                body))
                                                                                                                                          #(363
                                                                                                                                            (k$584)
                                                                                                                                            ((#((record-marker)
                                                                                                                                                #((record-marker)
                                                                                                                                                  #f
                                                                                                                                                  (id args
                                                                                                                                                      body))
                                                                                                                                                #(362
                                                                                                                                                  (r$585)
                                                                                                                                                  ((if r$585
                                                                                                                                                     (k$584 'unquote-splicing)
                                                                                                                                                     (k$584 'unquote)))))
                                                                                                                                              (equal?
                                                                                                                                                next-c$226
                                                                                                                                                #\@)))))
                                                                                                                                        #((record-marker)
                                                                                                                                          #((record-marker)
                                                                                                                                            #f
                                                                                                                                            (id args
                                                                                                                                                body))
                                                                                                                                          #(361
                                                                                                                                            (r$583)
                                                                                                                                            ((#((record-marker)
                                                                                                                                                #((record-marker)
                                                                                                                                                  #f
                                                                                                                                                  (id args
                                                                                                                                                      body))
                                                                                                                                                #(360
                                                                                                                                                  (r$566)
                                                                                                                                                  ((#((record-marker)
                                                                                                                                                      #((record-marker)
                                                                                                                                                        #f
                                                                                                                                                        (id args
                                                                                                                                                            body))
                                                                                                                                                      #(359
                                                                                                                                                        (r$567)
                                                                                                                                                        ((#((record-marker)
                                                                                                                                                            #((record-marker)
                                                                                                                                                              #f
                                                                                                                                                              (id args
                                                                                                                                                                  body))
                                                                                                                                                            #(358
                                                                                                                                                              (k$578)
                                                                                                                                                              ((#((record-marker)
                                                                                                                                                                  #((record-marker)
                                                                                                                                                                    #f
                                                                                                                                                                    (id args
                                                                                                                                                                        body))
                                                                                                                                                                  #(357
                                                                                                                                                                    (r$579)
                                                                                                                                                                    ((if r$579
                                                                                                                                                                       (#((record-marker)
                                                                                                                                                                          #((record-marker)
                                                                                                                                                                            #f
                                                                                                                                                                            (id args
                                                                                                                                                                                body))
                                                                                                                                                                          #(353
                                                                                                                                                                            ()
                                                                                                                                                                            ((in-port:get-lnum
                                                                                                                                                                               #((record-marker)
                                                                                                                                                                                 #((record-marker)
                                                                                                                                                                                   #f
                                                                                                                                                                                   (id args
                                                                                                                                                                                       body))
                                                                                                                                                                                 #(352
                                                                                                                                                                                   (r$580)
                                                                                                                                                                                   ((in-port:get-cnum
                                                                                                                                                                                      #((record-marker)
                                                                                                                                                                                        #((record-marker)
                                                                                                                                                                                          #f
                                                                                                                                                                                          (id args
                                                                                                                                                                                              body))
                                                                                                                                                                                        #(351
                                                                                                                                                                                          (r$581)
                                                                                                                                                                                          ((parse-error
                                                                                                                                                                                             k$578
                                                                                                                                                                                             "unexpected end of file"
                                                                                                                                                                                             r$580
                                                                                                                                                                                             r$581))))
                                                                                                                                                                                      ptbl$196))))
                                                                                                                                                                               ptbl$196)))))
                                                                                                                                                                       (#((record-marker)
                                                                                                                                                                          #((record-marker)
                                                                                                                                                                            #f
                                                                                                                                                                            (id args
                                                                                                                                                                                body))
                                                                                                                                                                          #(356
                                                                                                                                                                            (r$582)
                                                                                                                                                                            ((if r$582
                                                                                                                                                                               (#((record-marker)
                                                                                                                                                                                  #((record-marker)
                                                                                                                                                                                    #f
                                                                                                                                                                                    (id args
                                                                                                                                                                                        body))
                                                                                                                                                                                  #(354
                                                                                                                                                                                    ()
                                                                                                                                                                                    ((k$578 #f)))))
                                                                                                                                                                               (#((record-marker)
                                                                                                                                                                                  #((record-marker)
                                                                                                                                                                                    #f
                                                                                                                                                                                    (id args
                                                                                                                                                                                        body))
                                                                                                                                                                                  #(355
                                                                                                                                                                                    ()
                                                                                                                                                                                    ((in-port:set-buf!
                                                                                                                                                                                       k$578
                                                                                                                                                                                       ptbl$196
                                                                                                                                                                                       next-c$226)))))))))
                                                                                                                                                                        (equal?
                                                                                                                                                                          next-c$226
                                                                                                                                                                          #\@))))))
                                                                                                                                                                (eof-object?
                                                                                                                                                                  next-c$226)))))
                                                                                                                                                          #((record-marker)
                                                                                                                                                            #((record-marker)
                                                                                                                                                              #f
                                                                                                                                                              (id args
                                                                                                                                                                  body))
                                                                                                                                                            #(350
                                                                                                                                                              (r$568)
                                                                                                                                                              ((#((record-marker)
                                                                                                                                                                  #((record-marker)
                                                                                                                                                                    #f
                                                                                                                                                                    (id args
                                                                                                                                                                        body))
                                                                                                                                                                  #(349
                                                                                                                                                                    (r$576)
                                                                                                                                                                    ((#((record-marker)
                                                                                                                                                                        #((record-marker)
                                                                                                                                                                          #f
                                                                                                                                                                          (id args
                                                                                                                                                                              body))
                                                                                                                                                                        #(348
                                                                                                                                                                          (r$577)
                                                                                                                                                                          ((parse #((record-marker)
                                                                                                                                                                                    #((record-marker)
                                                                                                                                                                                      #f
                                                                                                                                                                                      (id args
                                                                                                                                                                                          body))
                                                                                                                                                                                    #(347
                                                                                                                                                                                      (r$575)
                                                                                                                                                                                      ((#((record-marker)
                                                                                                                                                                                          #((record-marker)
                                                                                                                                                                                            #f
                                                                                                                                                                                            (id args
                                                                                                                                                                                                body))
                                                                                                                                                                                          #(346
                                                                                                                                                                                            (r$569)
                                                                                                                                                                                            ((list #((record-marker)
                                                                                                                                                                                                     #((record-marker)
                                                                                                                                                                                                       #f
                                                                                                                                                                                                       (id args
                                                                                                                                                                                                           body))
                                                                                                                                                                                                     #(345
                                                                                                                                                                                                       (r$573)
                                                                                                                                                                                                       ((get-toks
                                                                                                                                                                                                          #((record-marker)
                                                                                                                                                                                                            #((record-marker)
                                                                                                                                                                                                              #f
                                                                                                                                                                                                              (id args
                                                                                                                                                                                                                  body))
                                                                                                                                                                                                            #(344
                                                                                                                                                                                                              (r$574)
                                                                                                                                                                                                              ((add-tok
                                                                                                                                                                                                                 #((record-marker)
                                                                                                                                                                                                                   #((record-marker)
                                                                                                                                                                                                                     #f
                                                                                                                                                                                                                     (id args
                                                                                                                                                                                                                         body))
                                                                                                                                                                                                                   #(343
                                                                                                                                                                                                                     (r$572)
                                                                                                                                                                                                                     ((#((record-marker)
                                                                                                                                                                                                                         #((record-marker)
                                                                                                                                                                                                                           #f
                                                                                                                                                                                                                           (id args
                                                                                                                                                                                                                               body))
                                                                                                                                                                                                                         #(342
                                                                                                                                                                                                                           (r$570)
                                                                                                                                                                                                                           ((if all?$199
                                                                                                                                                                                                                              (#((record-marker)
                                                                                                                                                                                                                                 #((record-marker)
                                                                                                                                                                                                                                   #f
                                                                                                                                                                                                                                   (id args
                                                                                                                                                                                                                                       body))
                                                                                                                                                                                                                                 #(341
                                                                                                                                                                                                                                   (r$571)
                                                                                                                                                                                                                                   ((parse k$497
                                                                                                                                                                                                                                           fp$202
                                                                                                                                                                                                                                           r$571
                                                                                                                                                                                                                                           new-toks$224
                                                                                                                                                                                                                                           all?$199
                                                                                                                                                                                                                                           #f
                                                                                                                                                                                                                                           parens$197
                                                                                                                                                                                                                                           ptbl$196))))
                                                                                                                                                                                                                               '())
                                                                                                                                                                                                                              (k$497 (car new-toks$224))))))
                                                                                                                                                                                                                       (set! new-toks$224
                                                                                                                                                                                                                         r$572)))))
                                                                                                                                                                                                                 r$573
                                                                                                                                                                                                                 r$574))))
                                                                                                                                                                                                          tok$201
                                                                                                                                                                                                          toks$200))))
                                                                                                                                                                                                   unquote-sym$225
                                                                                                                                                                                                   sub$227))))
                                                                                                                                                                                        (set! sub$227
                                                                                                                                                                                          r$575)))))
                                                                                                                                                                                  fp$202
                                                                                                                                                                                  r$576
                                                                                                                                                                                  r$577
                                                                                                                                                                                  #f
                                                                                                                                                                                  #f
                                                                                                                                                                                  0
                                                                                                                                                                                  ptbl$196))))
                                                                                                                                                                      '()))))
                                                                                                                                                                '()))))))))
                                                                                                                                                    (set! new-toks$224
                                                                                                                                                      #f)))))
                                                                                                                                              (set! unquote-sym$225
                                                                                                                                                r$583)))))))))
                                                                                                                                  (set! next-c$226
                                                                                                                                    r$586)))))
                                                                                                                            (read-char
                                                                                                                              fp$202)))))
                                                                                                                      (set! sub$227
                                                                                                                        #f)))))
                                                                                                                #f
                                                                                                                #f
                                                                                                                #f
                                                                                                                #f))))))))))))))
                                                                                           (#((record-marker)
                                                                                              #((record-marker)
                                                                                                #f
                                                                                                (id args
                                                                                                    body))
                                                                                              #(554
                                                                                                (r$589)
                                                                                                ((if r$589
                                                                                                   (#((record-marker)
                                                                                                      #((record-marker)
                                                                                                        #f
                                                                                                        (id args
                                                                                                            body))
                                                                                                      #(396
                                                                                                        ()
                                                                                                        ((#((record-marker)
                                                                                                            #((record-marker)
                                                                                                              #f
                                                                                                              (id args
                                                                                                                  body))
                                                                                                            #(395
                                                                                                              (k$607)
                                                                                                              ((if all?$199
                                                                                                                 (k$607 #f)
                                                                                                                 (#((record-marker)
                                                                                                                    #((record-marker)
                                                                                                                      #f
                                                                                                                      (id args
                                                                                                                          body))
                                                                                                                    #(394
                                                                                                                      (r$608)
                                                                                                                      ((not k$607
                                                                                                                            r$608))))
                                                                                                                  (null? tok$201))))))
                                                                                                          #((record-marker)
                                                                                                            #((record-marker)
                                                                                                              #f
                                                                                                              (id args
                                                                                                                  body))
                                                                                                            #(393
                                                                                                              (r$590)
                                                                                                              ((if r$590
                                                                                                                 (#((record-marker)
                                                                                                                    #((record-marker)
                                                                                                                      #f
                                                                                                                      (id args
                                                                                                                          body))
                                                                                                                    #(376
                                                                                                                      ()
                                                                                                                      ((in-port:set-buf!
                                                                                                                         #((record-marker)
                                                                                                                           #((record-marker)
                                                                                                                             #f
                                                                                                                             (id args
                                                                                                                                 body))
                                                                                                                           #(375
                                                                                                                             (r$591)
                                                                                                                             ((->tok #((record-marker)
                                                                                                                                       #((record-marker)
                                                                                                                                         #f
                                                                                                                                         (id args
                                                                                                                                             body))
                                                                                                                                       #(374
                                                                                                                                         (r$593)
                                                                                                                                         ((add-tok
                                                                                                                                            #((record-marker)
                                                                                                                                              #((record-marker)
                                                                                                                                                #f
                                                                                                                                                (id args
                                                                                                                                                    body))
                                                                                                                                              #(373
                                                                                                                                                (r$592)
                                                                                                                                                ((k$497 (car r$592)))))
                                                                                                                                            r$593
                                                                                                                                            toks$200))))
                                                                                                                                     tok$201))))
                                                                                                                         ptbl$196
                                                                                                                         c$204)))))
                                                                                                                 (#((record-marker)
                                                                                                                    #((record-marker)
                                                                                                                      #f
                                                                                                                      (id args
                                                                                                                          body))
                                                                                                                    #(392
                                                                                                                      ()
                                                                                                                      ((#((record-marker)
                                                                                                                          #((record-marker)
                                                                                                                            #f
                                                                                                                            (id args
                                                                                                                                body))
                                                                                                                          #(391
                                                                                                                            (r$604)
                                                                                                                            ((#((record-marker)
                                                                                                                                #((record-marker)
                                                                                                                                  #f
                                                                                                                                  (id args
                                                                                                                                      body))
                                                                                                                                #(390
                                                                                                                                  (r$605)
                                                                                                                                  ((#((record-marker)
                                                                                                                                      #((record-marker)
                                                                                                                                        #f
                                                                                                                                        (id args
                                                                                                                                            body))
                                                                                                                                      #(389
                                                                                                                                        (r$606)
                                                                                                                                        ((parse #((record-marker)
                                                                                                                                                  #((record-marker)
                                                                                                                                                    #f
                                                                                                                                                    (id args
                                                                                                                                                        body))
                                                                                                                                                  #(388
                                                                                                                                                    (r$594)
                                                                                                                                                    ((get-toks
                                                                                                                                                       #((record-marker)
                                                                                                                                                         #((record-marker)
                                                                                                                                                           #f
                                                                                                                                                           (id args
                                                                                                                                                               body))
                                                                                                                                                         #(387
                                                                                                                                                           (r$595)
                                                                                                                                                           ((#((record-marker)
                                                                                                                                                               #((record-marker)
                                                                                                                                                                 #f
                                                                                                                                                                 (id args
                                                                                                                                                                     body))
                                                                                                                                                               #(386
                                                                                                                                                                 (sub$222
                                                                                                                                                                   toks*$221)
                                                                                                                                                                 ((#((record-marker)
                                                                                                                                                                     #((record-marker)
                                                                                                                                                                       #f
                                                                                                                                                                       (id args
                                                                                                                                                                           body))
                                                                                                                                                                     #(385
                                                                                                                                                                       (new-toks$223)
                                                                                                                                                                       ((#((record-marker)
                                                                                                                                                                           #((record-marker)
                                                                                                                                                                             #f
                                                                                                                                                                             (id args
                                                                                                                                                                                 body))
                                                                                                                                                                           #(384
                                                                                                                                                                             (k$600)
                                                                                                                                                                             ((#((record-marker)
                                                                                                                                                                                 #((record-marker)
                                                                                                                                                                                   #f
                                                                                                                                                                                   (id args
                                                                                                                                                                                       body))
                                                                                                                                                                                 #(383
                                                                                                                                                                                   (k$602)
                                                                                                                                                                                   ((#((record-marker)
                                                                                                                                                                                       #((record-marker)
                                                                                                                                                                                         #f
                                                                                                                                                                                         (id args
                                                                                                                                                                                             body))
                                                                                                                                                                                       #(382
                                                                                                                                                                                         (r$603)
                                                                                                                                                                                         ((if r$603
                                                                                                                                                                                            (dotted?
                                                                                                                                                                                              k$602
                                                                                                                                                                                              sub$222)
                                                                                                                                                                                            (k$602 #f)))))
                                                                                                                                                                                     (pair? sub$222)))))
                                                                                                                                                                               #((record-marker)
                                                                                                                                                                                 #((record-marker)
                                                                                                                                                                                   #f
                                                                                                                                                                                   (id args
                                                                                                                                                                                       body))
                                                                                                                                                                                 #(381
                                                                                                                                                                                   (r$601)
                                                                                                                                                                                   ((if r$601
                                                                                                                                                                                      (->dotted-list
                                                                                                                                                                                        k$600
                                                                                                                                                                                        sub$222)
                                                                                                                                                                                      (k$600 sub$222)))))))))
                                                                                                                                                                         #((record-marker)
                                                                                                                                                                           #((record-marker)
                                                                                                                                                                             #f
                                                                                                                                                                             (id args
                                                                                                                                                                                 body))
                                                                                                                                                                           #(380
                                                                                                                                                                             (r$599)
                                                                                                                                                                             ((add-tok
                                                                                                                                                                                #((record-marker)
                                                                                                                                                                                  #((record-marker)
                                                                                                                                                                                    #f
                                                                                                                                                                                    (id args
                                                                                                                                                                                        body))
                                                                                                                                                                                  #(379
                                                                                                                                                                                    (r$598)
                                                                                                                                                                                    ((#((record-marker)
                                                                                                                                                                                        #((record-marker)
                                                                                                                                                                                          #f
                                                                                                                                                                                          (id args
                                                                                                                                                                                              body))
                                                                                                                                                                                        #(378
                                                                                                                                                                                          (r$596)
                                                                                                                                                                                          ((if all?$199
                                                                                                                                                                                             (#((record-marker)
                                                                                                                                                                                                #((record-marker)
                                                                                                                                                                                                  #f
                                                                                                                                                                                                  (id args
                                                                                                                                                                                                      body))
                                                                                                                                                                                                #(377
                                                                                                                                                                                                  (r$597)
                                                                                                                                                                                                  ((parse k$497
                                                                                                                                                                                                          fp$202
                                                                                                                                                                                                          r$597
                                                                                                                                                                                                          new-toks$223
                                                                                                                                                                                                          all?$199
                                                                                                                                                                                                          #f
                                                                                                                                                                                                          parens$197
                                                                                                                                                                                                          ptbl$196))))
                                                                                                                                                                                              '())
                                                                                                                                                                                             (k$497 (car new-toks$223))))))
                                                                                                                                                                                      (set! new-toks$223
                                                                                                                                                                                        r$598)))))
                                                                                                                                                                                r$599
                                                                                                                                                                                toks*$221))))))))
                                                                                                                                                                   #f))))
                                                                                                                                                             r$594
                                                                                                                                                             r$595))))
                                                                                                                                                       tok$201
                                                                                                                                                       toks$200))))
                                                                                                                                                fp$202
                                                                                                                                                r$604
                                                                                                                                                r$605
                                                                                                                                                #t
                                                                                                                                                #f
                                                                                                                                                r$606
                                                                                                                                                ptbl$196))))
                                                                                                                                    (+ parens$197
                                                                                                                                       1)))))
                                                                                                                              '()))))
                                                                                                                        '()))))))))))))))
                                                                                                   (#((record-marker)
                                                                                                      #((record-marker)
                                                                                                        #f
                                                                                                        (id args
                                                                                                            body))
                                                                                                      #(553
                                                                                                        (r$609)
                                                                                                        ((if r$609
                                                                                                           (#((record-marker)
                                                                                                              #((record-marker)
                                                                                                                #f
                                                                                                                (id args
                                                                                                                    body))
                                                                                                              #(410
                                                                                                                ()
                                                                                                                ((#((record-marker)
                                                                                                                    #((record-marker)
                                                                                                                      #f
                                                                                                                      (id args
                                                                                                                          body))
                                                                                                                    #(409
                                                                                                                      (k$618)
                                                                                                                      ((if all?$199
                                                                                                                         (k$618 #f)
                                                                                                                         (#((record-marker)
                                                                                                                            #((record-marker)
                                                                                                                              #f
                                                                                                                              (id args
                                                                                                                                  body))
                                                                                                                            #(408
                                                                                                                              (r$619)
                                                                                                                              ((not k$618
                                                                                                                                    r$619))))
                                                                                                                          (null? tok$201))))))
                                                                                                                  #((record-marker)
                                                                                                                    #((record-marker)
                                                                                                                      #f
                                                                                                                      (id args
                                                                                                                          body))
                                                                                                                    #(407
                                                                                                                      (r$610)
                                                                                                                      ((if r$610
                                                                                                                         (#((record-marker)
                                                                                                                            #((record-marker)
                                                                                                                              #f
                                                                                                                              (id args
                                                                                                                                  body))
                                                                                                                            #(400
                                                                                                                              ()
                                                                                                                              ((in-port:set-buf!
                                                                                                                                 #((record-marker)
                                                                                                                                   #((record-marker)
                                                                                                                                     #f
                                                                                                                                     (id args
                                                                                                                                         body))
                                                                                                                                   #(399
                                                                                                                                     (r$611)
                                                                                                                                     ((->tok #((record-marker)
                                                                                                                                               #((record-marker)
                                                                                                                                                 #f
                                                                                                                                                 (id args
                                                                                                                                                     body))
                                                                                                                                               #(398
                                                                                                                                                 (r$613)
                                                                                                                                                 ((add-tok
                                                                                                                                                    #((record-marker)
                                                                                                                                                      #((record-marker)
                                                                                                                                                        #f
                                                                                                                                                        (id args
                                                                                                                                                            body))
                                                                                                                                                      #(397
                                                                                                                                                        (r$612)
                                                                                                                                                        ((k$497 (car r$612)))))
                                                                                                                                                    r$613
                                                                                                                                                    toks$200))))
                                                                                                                                             tok$201))))
                                                                                                                                 ptbl$196
                                                                                                                                 c$204)))))
                                                                                                                         (#((record-marker)
                                                                                                                            #((record-marker)
                                                                                                                              #f
                                                                                                                              (id args
                                                                                                                                  body))
                                                                                                                            #(406
                                                                                                                              (r$614)
                                                                                                                              ((if r$614
                                                                                                                                 (#((record-marker)
                                                                                                                                    #((record-marker)
                                                                                                                                      #f
                                                                                                                                      (id args
                                                                                                                                          body))
                                                                                                                                    #(403
                                                                                                                                      ()
                                                                                                                                      ((in-port:get-lnum
                                                                                                                                         #((record-marker)
                                                                                                                                           #((record-marker)
                                                                                                                                             #f
                                                                                                                                             (id args
                                                                                                                                                 body))
                                                                                                                                           #(402
                                                                                                                                             (r$615)
                                                                                                                                             ((in-port:get-cnum
                                                                                                                                                #((record-marker)
                                                                                                                                                  #((record-marker)
                                                                                                                                                    #f
                                                                                                                                                    (id args
                                                                                                                                                        body))
                                                                                                                                                  #(401
                                                                                                                                                    (r$616)
                                                                                                                                                    ((parse-error
                                                                                                                                                       k$497
                                                                                                                                                       "unexpected closing parenthesis"
                                                                                                                                                       r$615
                                                                                                                                                       r$616))))
                                                                                                                                                ptbl$196))))
                                                                                                                                         ptbl$196)))))
                                                                                                                                 (#((record-marker)
                                                                                                                                    #((record-marker)
                                                                                                                                      #f
                                                                                                                                      (id args
                                                                                                                                          body))
                                                                                                                                    #(405
                                                                                                                                      ()
                                                                                                                                      ((get-toks
                                                                                                                                         #((record-marker)
                                                                                                                                           #((record-marker)
                                                                                                                                             #f
                                                                                                                                             (id args
                                                                                                                                                 body))
                                                                                                                                           #(404
                                                                                                                                             (r$617)
                                                                                                                                             ((reverse
                                                                                                                                                k$497
                                                                                                                                                r$617))))
                                                                                                                                         tok$201
                                                                                                                                         toks$200)))))))))
                                                                                                                          (= parens$197
                                                                                                                             0)))))))))))
                                                                                                           (#((record-marker)
                                                                                                              #((record-marker)
                                                                                                                #f
                                                                                                                (id args
                                                                                                                    body))
                                                                                                              #(552
                                                                                                                (r$620)
                                                                                                                ((if r$620
                                                                                                                   (#((record-marker)
                                                                                                                      #((record-marker)
                                                                                                                        #f
                                                                                                                        (id args
                                                                                                                            body))
                                                                                                                      #(427
                                                                                                                        ()
                                                                                                                        ((#((record-marker)
                                                                                                                            #((record-marker)
                                                                                                                              #f
                                                                                                                              (id args
                                                                                                                                  body))
                                                                                                                            #(426
                                                                                                                              (k$631)
                                                                                                                              ((if all?$199
                                                                                                                                 (k$631 #f)
                                                                                                                                 (#((record-marker)
                                                                                                                                    #((record-marker)
                                                                                                                                      #f
                                                                                                                                      (id args
                                                                                                                                          body))
                                                                                                                                    #(425
                                                                                                                                      (r$632)
                                                                                                                                      ((not k$631
                                                                                                                                            r$632))))
                                                                                                                                  (null? tok$201))))))
                                                                                                                          #((record-marker)
                                                                                                                            #((record-marker)
                                                                                                                              #f
                                                                                                                              (id args
                                                                                                                                  body))
                                                                                                                            #(424
                                                                                                                              (r$621)
                                                                                                                              ((if r$621
                                                                                                                                 (#((record-marker)
                                                                                                                                    #((record-marker)
                                                                                                                                      #f
                                                                                                                                      (id args
                                                                                                                                          body))
                                                                                                                                    #(414
                                                                                                                                      ()
                                                                                                                                      ((in-port:set-buf!
                                                                                                                                         #((record-marker)
                                                                                                                                           #((record-marker)
                                                                                                                                             #f
                                                                                                                                             (id args
                                                                                                                                                 body))
                                                                                                                                           #(413
                                                                                                                                             (r$622)
                                                                                                                                             ((->tok #((record-marker)
                                                                                                                                                       #((record-marker)
                                                                                                                                                         #f
                                                                                                                                                         (id args
                                                                                                                                                             body))
                                                                                                                                                       #(412
                                                                                                                                                         (r$624)
                                                                                                                                                         ((add-tok
                                                                                                                                                            #((record-marker)
                                                                                                                                                              #((record-marker)
                                                                                                                                                                #f
                                                                                                                                                                (id args
                                                                                                                                                                    body))
                                                                                                                                                              #(411
                                                                                                                                                                (r$623)
                                                                                                                                                                ((k$497 (car r$623)))))
                                                                                                                                                            r$624
                                                                                                                                                            toks$200))))
                                                                                                                                                     tok$201))))
                                                                                                                                         ptbl$196
                                                                                                                                         c$204)))))
                                                                                                                                 (#((record-marker)
                                                                                                                                    #((record-marker)
                                                                                                                                      #f
                                                                                                                                      (id args
                                                                                                                                          body))
                                                                                                                                    #(423
                                                                                                                                      ()
                                                                                                                                      ((#((record-marker)
                                                                                                                                          #((record-marker)
                                                                                                                                            #f
                                                                                                                                            (id args
                                                                                                                                                body))
                                                                                                                                          #(422
                                                                                                                                            (r$630)
                                                                                                                                            ((read-str
                                                                                                                                               #((record-marker)
                                                                                                                                                 #((record-marker)
                                                                                                                                                   #f
                                                                                                                                                   (id args
                                                                                                                                                       body))
                                                                                                                                                 #(421
                                                                                                                                                   (r$625)
                                                                                                                                                   ((get-toks
                                                                                                                                                      #((record-marker)
                                                                                                                                                        #((record-marker)
                                                                                                                                                          #f
                                                                                                                                                          (id args
                                                                                                                                                              body))
                                                                                                                                                        #(420
                                                                                                                                                          (r$626)
                                                                                                                                                          ((#((record-marker)
                                                                                                                                                              #((record-marker)
                                                                                                                                                                #f
                                                                                                                                                                (id args
                                                                                                                                                                    body))
                                                                                                                                                              #(419
                                                                                                                                                                (str$219
                                                                                                                                                                  toks*$218)
                                                                                                                                                                ((#((record-marker)
                                                                                                                                                                    #((record-marker)
                                                                                                                                                                      #f
                                                                                                                                                                      (id args
                                                                                                                                                                          body))
                                                                                                                                                                    #(418
                                                                                                                                                                      (new-toks$220)
                                                                                                                                                                      ((add-tok
                                                                                                                                                                         #((record-marker)
                                                                                                                                                                           #((record-marker)
                                                                                                                                                                             #f
                                                                                                                                                                             (id args
                                                                                                                                                                                 body))
                                                                                                                                                                           #(417
                                                                                                                                                                             (r$629)
                                                                                                                                                                             ((#((record-marker)
                                                                                                                                                                                 #((record-marker)
                                                                                                                                                                                   #f
                                                                                                                                                                                   (id args
                                                                                                                                                                                       body))
                                                                                                                                                                                 #(416
                                                                                                                                                                                   (r$627)
                                                                                                                                                                                   ((if all?$199
                                                                                                                                                                                      (#((record-marker)
                                                                                                                                                                                         #((record-marker)
                                                                                                                                                                                           #f
                                                                                                                                                                                           (id args
                                                                                                                                                                                               body))
                                                                                                                                                                                         #(415
                                                                                                                                                                                           (r$628)
                                                                                                                                                                                           ((parse k$497
                                                                                                                                                                                                   fp$202
                                                                                                                                                                                                   r$628
                                                                                                                                                                                                   new-toks$220
                                                                                                                                                                                                   all?$199
                                                                                                                                                                                                   #f
                                                                                                                                                                                                   parens$197
                                                                                                                                                                                                   ptbl$196))))
                                                                                                                                                                                       '())
                                                                                                                                                                                      (k$497 (car new-toks$220))))))
                                                                                                                                                                               (set! new-toks$220
                                                                                                                                                                                 r$629)))))
                                                                                                                                                                         str$219
                                                                                                                                                                         toks*$218))))
                                                                                                                                                                  #f))))
                                                                                                                                                            r$625
                                                                                                                                                            r$626))))
                                                                                                                                                      tok$201
                                                                                                                                                      toks$200))))
                                                                                                                                               fp$202
                                                                                                                                               r$630
                                                                                                                                               ptbl$196))))
                                                                                                                                        '()))))))))))))))
                                                                                                                   (#((record-marker)
                                                                                                                      #((record-marker)
                                                                                                                        #f
                                                                                                                        (id args
                                                                                                                            body))
                                                                                                                      #(551
                                                                                                                        (r$633)
                                                                                                                        ((if r$633
                                                                                                                           (#((record-marker)
                                                                                                                              #((record-marker)
                                                                                                                                #f
                                                                                                                                (id args
                                                                                                                                    body))
                                                                                                                              #(546
                                                                                                                                ()
                                                                                                                                ((#((record-marker)
                                                                                                                                    #((record-marker)
                                                                                                                                      #f
                                                                                                                                      (id args
                                                                                                                                          body))
                                                                                                                                    #(545
                                                                                                                                      (r$634)
                                                                                                                                      ((if r$634
                                                                                                                                         (#((record-marker)
                                                                                                                                            #((record-marker)
                                                                                                                                              #f
                                                                                                                                              (id args
                                                                                                                                                  body))
                                                                                                                                            #(542
                                                                                                                                              (next-c$205)
                                                                                                                                              ((in-port:get-cnum
                                                                                                                                                 #((record-marker)
                                                                                                                                                   #((record-marker)
                                                                                                                                                     #f
                                                                                                                                                     (id args
                                                                                                                                                         body))
                                                                                                                                                   #(541
                                                                                                                                                     (r$729)
                                                                                                                                                     ((#((record-marker)
                                                                                                                                                         #((record-marker)
                                                                                                                                                           #f
                                                                                                                                                           (id args
                                                                                                                                                               body))
                                                                                                                                                         #(540
                                                                                                                                                           (r$728)
                                                                                                                                                           ((in-port:set-cnum!
                                                                                                                                                              #((record-marker)
                                                                                                                                                                #((record-marker)
                                                                                                                                                                  #f
                                                                                                                                                                  (id args
                                                                                                                                                                      body))
                                                                                                                                                                #(539
                                                                                                                                                                  (r$636)
                                                                                                                                                                  ((#((record-marker)
                                                                                                                                                                      #((record-marker)
                                                                                                                                                                        #f
                                                                                                                                                                        (id args
                                                                                                                                                                            body))
                                                                                                                                                                      #(538
                                                                                                                                                                        (r$637)
                                                                                                                                                                        ((if r$637
                                                                                                                                                                           (#((record-marker)
                                                                                                                                                                              #((record-marker)
                                                                                                                                                                                #f
                                                                                                                                                                                (id args
                                                                                                                                                                                    body))
                                                                                                                                                                              #(430
                                                                                                                                                                                ()
                                                                                                                                                                                ((read-block-comment
                                                                                                                                                                                   #((record-marker)
                                                                                                                                                                                     #((record-marker)
                                                                                                                                                                                       #f
                                                                                                                                                                                       (id args
                                                                                                                                                                                           body))
                                                                                                                                                                                     #(429
                                                                                                                                                                                       (r$638)
                                                                                                                                                                                       ((#((record-marker)
                                                                                                                                                                                           #((record-marker)
                                                                                                                                                                                             #f
                                                                                                                                                                                             (id args
                                                                                                                                                                                                 body))
                                                                                                                                                                                           #(428
                                                                                                                                                                                             (r$639)
                                                                                                                                                                                             ((parse k$497
                                                                                                                                                                                                     fp$202
                                                                                                                                                                                                     r$639
                                                                                                                                                                                                     toks$200
                                                                                                                                                                                                     all?$199
                                                                                                                                                                                                     #f
                                                                                                                                                                                                     parens$197
                                                                                                                                                                                                     ptbl$196))))
                                                                                                                                                                                         '()))))
                                                                                                                                                                                   fp$202
                                                                                                                                                                                   ptbl$196)))))
                                                                                                                                                                           (#((record-marker)
                                                                                                                                                                              #((record-marker)
                                                                                                                                                                                #f
                                                                                                                                                                                (id args
                                                                                                                                                                                    body))
                                                                                                                                                                              #(537
                                                                                                                                                                                (r$640)
                                                                                                                                                                                ((if r$640
                                                                                                                                                                                   (#((record-marker)
                                                                                                                                                                                      #((record-marker)
                                                                                                                                                                                        #f
                                                                                                                                                                                        (id args
                                                                                                                                                                                            body))
                                                                                                                                                                                      #(433
                                                                                                                                                                                        ()
                                                                                                                                                                                        ((if all?$199
                                                                                                                                                                                           (#((record-marker)
                                                                                                                                                                                              #((record-marker)
                                                                                                                                                                                                #f
                                                                                                                                                                                                (id args
                                                                                                                                                                                                    body))
                                                                                                                                                                                              #(432
                                                                                                                                                                                                (r$641)
                                                                                                                                                                                                ((#((record-marker)
                                                                                                                                                                                                    #((record-marker)
                                                                                                                                                                                                      #f
                                                                                                                                                                                                      (id args
                                                                                                                                                                                                          body))
                                                                                                                                                                                                    #(431
                                                                                                                                                                                                      (r$642)
                                                                                                                                                                                                      ((parse k$497
                                                                                                                                                                                                              fp$202
                                                                                                                                                                                                              r$641
                                                                                                                                                                                                              r$642
                                                                                                                                                                                                              all?$199
                                                                                                                                                                                                              #f
                                                                                                                                                                                                              parens$197
                                                                                                                                                                                                              ptbl$196))))
                                                                                                                                                                                                  (cons #t
                                                                                                                                                                                                        toks$200)))))
                                                                                                                                                                                            '())
                                                                                                                                                                                           (k$497 #t))))))
                                                                                                                                                                                   (#((record-marker)
                                                                                                                                                                                      #((record-marker)
                                                                                                                                                                                        #f
                                                                                                                                                                                        (id args
                                                                                                                                                                                            body))
                                                                                                                                                                                      #(536
                                                                                                                                                                                        (r$643)
                                                                                                                                                                                        ((if r$643
                                                                                                                                                                                           (#((record-marker)
                                                                                                                                                                                              #((record-marker)
                                                                                                                                                                                                #f
                                                                                                                                                                                                (id args
                                                                                                                                                                                                    body))
                                                                                                                                                                                              #(436
                                                                                                                                                                                                ()
                                                                                                                                                                                                ((if all?$199
                                                                                                                                                                                                   (#((record-marker)
                                                                                                                                                                                                      #((record-marker)
                                                                                                                                                                                                        #f
                                                                                                                                                                                                        (id args
                                                                                                                                                                                                            body))
                                                                                                                                                                                                      #(435
                                                                                                                                                                                                        (r$644)
                                                                                                                                                                                                        ((#((record-marker)
                                                                                                                                                                                                            #((record-marker)
                                                                                                                                                                                                              #f
                                                                                                                                                                                                              (id args
                                                                                                                                                                                                                  body))
                                                                                                                                                                                                            #(434
                                                                                                                                                                                                              (r$645)
                                                                                                                                                                                                              ((parse k$497
                                                                                                                                                                                                                      fp$202
                                                                                                                                                                                                                      r$644
                                                                                                                                                                                                                      r$645
                                                                                                                                                                                                                      all?$199
                                                                                                                                                                                                                      #f
                                                                                                                                                                                                                      parens$197
                                                                                                                                                                                                                      ptbl$196))))
                                                                                                                                                                                                          (cons #f
                                                                                                                                                                                                                toks$200)))))
                                                                                                                                                                                                    '())
                                                                                                                                                                                                   (k$497 #f))))))
                                                                                                                                                                                           (#((record-marker)
                                                                                                                                                                                              #((record-marker)
                                                                                                                                                                                                #f
                                                                                                                                                                                                (id args
                                                                                                                                                                                                    body))
                                                                                                                                                                                              #(535
                                                                                                                                                                                                (r$646)
                                                                                                                                                                                                ((if r$646
                                                                                                                                                                                                   (#((record-marker)
                                                                                                                                                                                                      #((record-marker)
                                                                                                                                                                                                        #f
                                                                                                                                                                                                        (id args
                                                                                                                                                                                                            body))
                                                                                                                                                                                                      #(440
                                                                                                                                                                                                        ()
                                                                                                                                                                                                        ((#((record-marker)
                                                                                                                                                                                                            #((record-marker)
                                                                                                                                                                                                              #f
                                                                                                                                                                                                              (id args
                                                                                                                                                                                                                  body))
                                                                                                                                                                                                            #(437
                                                                                                                                                                                                              (r$647)
                                                                                                                                                                                                              ((parse-number
                                                                                                                                                                                                                 k$497
                                                                                                                                                                                                                 fp$202
                                                                                                                                                                                                                 toks$200
                                                                                                                                                                                                                 all?$199
                                                                                                                                                                                                                 parens$197
                                                                                                                                                                                                                 ptbl$196
                                                                                                                                                                                                                 10
                                                                                                                                                                                                                 r$647))))
                                                                                                                                                                                                          #((record-marker)
                                                                                                                                                                                                            #((record-marker)
                                                                                                                                                                                                              #f
                                                                                                                                                                                                              (id args
                                                                                                                                                                                                                  body))
                                                                                                                                                                                                            #(439
                                                                                                                                                                                                              (k$648 num$217)
                                                                                                                                                                                                              ((parse-atom
                                                                                                                                                                                                                 #((record-marker)
                                                                                                                                                                                                                   #((record-marker)
                                                                                                                                                                                                                     #f
                                                                                                                                                                                                                     (id args
                                                                                                                                                                                                                         body))
                                                                                                                                                                                                                   #(438
                                                                                                                                                                                                                     (r$649)
                                                                                                                                                                                                                     ((exact k$648
                                                                                                                                                                                                                             r$649))))
                                                                                                                                                                                                                 num$217)))))))))
                                                                                                                                                                                                   (#((record-marker)
                                                                                                                                                                                                      #((record-marker)
                                                                                                                                                                                                        #f
                                                                                                                                                                                                        (id args
                                                                                                                                                                                                            body))
                                                                                                                                                                                                      #(534
                                                                                                                                                                                                        (r$650)
                                                                                                                                                                                                        ((if r$650
                                                                                                                                                                                                           (#((record-marker)
                                                                                                                                                                                                              #((record-marker)
                                                                                                                                                                                                                #f
                                                                                                                                                                                                                (id args
                                                                                                                                                                                                                    body))
                                                                                                                                                                                                              #(444
                                                                                                                                                                                                                ()
                                                                                                                                                                                                                ((#((record-marker)
                                                                                                                                                                                                                    #((record-marker)
                                                                                                                                                                                                                      #f
                                                                                                                                                                                                                      (id args
                                                                                                                                                                                                                          body))
                                                                                                                                                                                                                    #(441
                                                                                                                                                                                                                      (r$651)
                                                                                                                                                                                                                      ((parse-number
                                                                                                                                                                                                                         k$497
                                                                                                                                                                                                                         fp$202
                                                                                                                                                                                                                         toks$200
                                                                                                                                                                                                                         all?$199
                                                                                                                                                                                                                         parens$197
                                                                                                                                                                                                                         ptbl$196
                                                                                                                                                                                                                         10
                                                                                                                                                                                                                         r$651))))
                                                                                                                                                                                                                  #((record-marker)
                                                                                                                                                                                                                    #((record-marker)
                                                                                                                                                                                                                      #f
                                                                                                                                                                                                                      (id args
                                                                                                                                                                                                                          body))
                                                                                                                                                                                                                    #(443
                                                                                                                                                                                                                      (k$652 num$216)
                                                                                                                                                                                                                      ((parse-atom
                                                                                                                                                                                                                         #((record-marker)
                                                                                                                                                                                                                           #((record-marker)
                                                                                                                                                                                                                             #f
                                                                                                                                                                                                                             (id args
                                                                                                                                                                                                                                 body))
                                                                                                                                                                                                                           #(442
                                                                                                                                                                                                                             (r$653)
                                                                                                                                                                                                                             ((inexact
                                                                                                                                                                                                                                k$652
                                                                                                                                                                                                                                r$653))))
                                                                                                                                                                                                                         num$216)))))))))
                                                                                                                                                                                                           (#((record-marker)
                                                                                                                                                                                                              #((record-marker)
                                                                                                                                                                                                                #f
                                                                                                                                                                                                                (id args
                                                                                                                                                                                                                    body))
                                                                                                                                                                                                              #(533
                                                                                                                                                                                                                (r$654)
                                                                                                                                                                                                                ((if r$654
                                                                                                                                                                                                                   (#((record-marker)
                                                                                                                                                                                                                      #((record-marker)
                                                                                                                                                                                                                        #f
                                                                                                                                                                                                                        (id args
                                                                                                                                                                                                                            body))
                                                                                                                                                                                                                      #(448
                                                                                                                                                                                                                        ()
                                                                                                                                                                                                                        ((#((record-marker)
                                                                                                                                                                                                                            #((record-marker)
                                                                                                                                                                                                                              #f
                                                                                                                                                                                                                              (id args
                                                                                                                                                                                                                                  body))
                                                                                                                                                                                                                            #(445
                                                                                                                                                                                                                              (r$655)
                                                                                                                                                                                                                              ((parse-number
                                                                                                                                                                                                                                 k$497
                                                                                                                                                                                                                                 fp$202
                                                                                                                                                                                                                                 toks$200
                                                                                                                                                                                                                                 all?$199
                                                                                                                                                                                                                                 parens$197
                                                                                                                                                                                                                                 ptbl$196
                                                                                                                                                                                                                                 2
                                                                                                                                                                                                                                 r$655))))
                                                                                                                                                                                                                          #((record-marker)
                                                                                                                                                                                                                            #((record-marker)
                                                                                                                                                                                                                              #f
                                                                                                                                                                                                                              (id args
                                                                                                                                                                                                                                  body))
                                                                                                                                                                                                                            #(447
                                                                                                                                                                                                                              (k$656 num$215)
                                                                                                                                                                                                                              ((#((record-marker)
                                                                                                                                                                                                                                  #((record-marker)
                                                                                                                                                                                                                                    #f
                                                                                                                                                                                                                                    (id args
                                                                                                                                                                                                                                        body))
                                                                                                                                                                                                                                  #(446
                                                                                                                                                                                                                                    (r$657)
                                                                                                                                                                                                                                    ((k$656 (string->number
                                                                                                                                                                                                                                              r$657
                                                                                                                                                                                                                                              2)))))
                                                                                                                                                                                                                                (list->string
                                                                                                                                                                                                                                  num$215))))))))))
                                                                                                                                                                                                                   (#((record-marker)
                                                                                                                                                                                                                      #((record-marker)
                                                                                                                                                                                                                        #f
                                                                                                                                                                                                                        (id args
                                                                                                                                                                                                                            body))
                                                                                                                                                                                                                      #(532
                                                                                                                                                                                                                        (r$658)
                                                                                                                                                                                                                        ((if r$658
                                                                                                                                                                                                                           (#((record-marker)
                                                                                                                                                                                                                              #((record-marker)
                                                                                                                                                                                                                                #f
                                                                                                                                                                                                                                (id args
                                                                                                                                                                                                                                    body))
                                                                                                                                                                                                                              #(452
                                                                                                                                                                                                                                ()
                                                                                                                                                                                                                                ((#((record-marker)
                                                                                                                                                                                                                                    #((record-marker)
                                                                                                                                                                                                                                      #f
                                                                                                                                                                                                                                      (id args
                                                                                                                                                                                                                                          body))
                                                                                                                                                                                                                                    #(449
                                                                                                                                                                                                                                      (r$659)
                                                                                                                                                                                                                                      ((parse-number
                                                                                                                                                                                                                                         k$497
                                                                                                                                                                                                                                         fp$202
                                                                                                                                                                                                                                         toks$200
                                                                                                                                                                                                                                         all?$199
                                                                                                                                                                                                                                         parens$197
                                                                                                                                                                                                                                         ptbl$196
                                                                                                                                                                                                                                         8
                                                                                                                                                                                                                                         r$659))))
                                                                                                                                                                                                                                  #((record-marker)
                                                                                                                                                                                                                                    #((record-marker)
                                                                                                                                                                                                                                      #f
                                                                                                                                                                                                                                      (id args
                                                                                                                                                                                                                                          body))
                                                                                                                                                                                                                                    #(451
                                                                                                                                                                                                                                      (k$660 num$214)
                                                                                                                                                                                                                                      ((#((record-marker)
                                                                                                                                                                                                                                          #((record-marker)
                                                                                                                                                                                                                                            #f
                                                                                                                                                                                                                                            (id args
                                                                                                                                                                                                                                                body))
                                                                                                                                                                                                                                          #(450
                                                                                                                                                                                                                                            (r$661)
                                                                                                                                                                                                                                            ((k$660 (string->number
                                                                                                                                                                                                                                                      r$661
                                                                                                                                                                                                                                                      8)))))
                                                                                                                                                                                                                                        (list->string
                                                                                                                                                                                                                                          num$214))))))))))
                                                                                                                                                                                                                           (#((record-marker)
                                                                                                                                                                                                                              #((record-marker)
                                                                                                                                                                                                                                #f
                                                                                                                                                                                                                                (id args
                                                                                                                                                                                                                                    body))
                                                                                                                                                                                                                              #(531
                                                                                                                                                                                                                                (r$662)
                                                                                                                                                                                                                                ((if r$662
                                                                                                                                                                                                                                   (#((record-marker)
                                                                                                                                                                                                                                      #((record-marker)
                                                                                                                                                                                                                                        #f
                                                                                                                                                                                                                                        (id args
                                                                                                                                                                                                                                            body))
                                                                                                                                                                                                                                      #(456
                                                                                                                                                                                                                                        ()
                                                                                                                                                                                                                                        ((#((record-marker)
                                                                                                                                                                                                                                            #((record-marker)
                                                                                                                                                                                                                                              #f
                                                                                                                                                                                                                                              (id args
                                                                                                                                                                                                                                                  body))
                                                                                                                                                                                                                                            #(453
                                                                                                                                                                                                                                              (r$663)
                                                                                                                                                                                                                                              ((parse-number
                                                                                                                                                                                                                                                 k$497
                                                                                                                                                                                                                                                 fp$202
                                                                                                                                                                                                                                                 toks$200
                                                                                                                                                                                                                                                 all?$199
                                                                                                                                                                                                                                                 parens$197
                                                                                                                                                                                                                                                 ptbl$196
                                                                                                                                                                                                                                                 16
                                                                                                                                                                                                                                                 r$663))))
                                                                                                                                                                                                                                          #((record-marker)
                                                                                                                                                                                                                                            #((record-marker)
                                                                                                                                                                                                                                              #f
                                                                                                                                                                                                                                              (id args
                                                                                                                                                                                                                                                  body))
                                                                                                                                                                                                                                            #(455
                                                                                                                                                                                                                                              (k$664 num$213)
                                                                                                                                                                                                                                              ((#((record-marker)
                                                                                                                                                                                                                                                  #((record-marker)
                                                                                                                                                                                                                                                    #f
                                                                                                                                                                                                                                                    (id args
                                                                                                                                                                                                                                                        body))
                                                                                                                                                                                                                                                  #(454
                                                                                                                                                                                                                                                    (r$665)
                                                                                                                                                                                                                                                    ((k$664 (string->number
                                                                                                                                                                                                                                                              r$665
                                                                                                                                                                                                                                                              16)))))
                                                                                                                                                                                                                                                (list->string
                                                                                                                                                                                                                                                  num$213))))))))))
                                                                                                                                                                                                                                   (#((record-marker)
                                                                                                                                                                                                                                      #((record-marker)
                                                                                                                                                                                                                                        #f
                                                                                                                                                                                                                                        (id args
                                                                                                                                                                                                                                            body))
                                                                                                                                                                                                                                      #(530
                                                                                                                                                                                                                                        (r$666)
                                                                                                                                                                                                                                        ((if r$666
                                                                                                                                                                                                                                           (#((record-marker)
                                                                                                                                                                                                                                              #((record-marker)
                                                                                                                                                                                                                                                #f
                                                                                                                                                                                                                                                (id args
                                                                                                                                                                                                                                                    body))
                                                                                                                                                                                                                                              #(488
                                                                                                                                                                                                                                                ()
                                                                                                                                                                                                                                                ((#((record-marker)
                                                                                                                                                                                                                                                    #((record-marker)
                                                                                                                                                                                                                                                      #f
                                                                                                                                                                                                                                                      (id args
                                                                                                                                                                                                                                                          body))
                                                                                                                                                                                                                                                    #(487
                                                                                                                                                                                                                                                      (r$695)
                                                                                                                                                                                                                                                      ((#((record-marker)
                                                                                                                                                                                                                                                          #((record-marker)
                                                                                                                                                                                                                                                            #f
                                                                                                                                                                                                                                                            (id args
                                                                                                                                                                                                                                                                body))
                                                                                                                                                                                                                                                          #(486
                                                                                                                                                                                                                                                            (r$667)
                                                                                                                                                                                                                                                            ((#((record-marker)
                                                                                                                                                                                                                                                                #((record-marker)
                                                                                                                                                                                                                                                                  #f
                                                                                                                                                                                                                                                                  (id args
                                                                                                                                                                                                                                                                      body))
                                                                                                                                                                                                                                                                #(485
                                                                                                                                                                                                                                                                  (k$691)
                                                                                                                                                                                                                                                                  ((#((record-marker)
                                                                                                                                                                                                                                                                      #((record-marker)
                                                                                                                                                                                                                                                                        #f
                                                                                                                                                                                                                                                                        (id args
                                                                                                                                                                                                                                                                            body))
                                                                                                                                                                                                                                                                      #(484
                                                                                                                                                                                                                                                                        (r$692)
                                                                                                                                                                                                                                                                        ((if r$692
                                                                                                                                                                                                                                                                           (k$691 #f)
                                                                                                                                                                                                                                                                           (in-port:get-lnum
                                                                                                                                                                                                                                                                             #((record-marker)
                                                                                                                                                                                                                                                                               #((record-marker)
                                                                                                                                                                                                                                                                                 #f
                                                                                                                                                                                                                                                                                 (id args
                                                                                                                                                                                                                                                                                     body))
                                                                                                                                                                                                                                                                               #(483
                                                                                                                                                                                                                                                                                 (r$693)
                                                                                                                                                                                                                                                                                 ((in-port:get-cnum
                                                                                                                                                                                                                                                                                    #((record-marker)
                                                                                                                                                                                                                                                                                      #((record-marker)
                                                                                                                                                                                                                                                                                        #f
                                                                                                                                                                                                                                                                                        (id args
                                                                                                                                                                                                                                                                                            body))
                                                                                                                                                                                                                                                                                      #(482
                                                                                                                                                                                                                                                                                        (r$694)
                                                                                                                                                                                                                                                                                        ((parse-error
                                                                                                                                                                                                                                                                                           k$691
                                                                                                                                                                                                                                                                                           "Unhandled input sequence"
                                                                                                                                                                                                                                                                                           r$693
                                                                                                                                                                                                                                                                                           r$694))))
                                                                                                                                                                                                                                                                                    ptbl$196))))
                                                                                                                                                                                                                                                                             ptbl$196)))))
                                                                                                                                                                                                                                                                    (eq? #\8
                                                                                                                                                                                                                                                                         next-c$205)))))
                                                                                                                                                                                                                                                              #((record-marker)
                                                                                                                                                                                                                                                                #((record-marker)
                                                                                                                                                                                                                                                                  #f
                                                                                                                                                                                                                                                                  (id args
                                                                                                                                                                                                                                                                      body))
                                                                                                                                                                                                                                                                #(481
                                                                                                                                                                                                                                                                  (r$668)
                                                                                                                                                                                                                                                                  ((#((record-marker)
                                                                                                                                                                                                                                                                      #((record-marker)
                                                                                                                                                                                                                                                                        #f
                                                                                                                                                                                                                                                                        (id args
                                                                                                                                                                                                                                                                            body))
                                                                                                                                                                                                                                                                      #(480
                                                                                                                                                                                                                                                                        (r$690)
                                                                                                                                                                                                                                                                        ((#((record-marker)
                                                                                                                                                                                                                                                                            #((record-marker)
                                                                                                                                                                                                                                                                              #f
                                                                                                                                                                                                                                                                              (id args
                                                                                                                                                                                                                                                                                  body))
                                                                                                                                                                                                                                                                            #(479
                                                                                                                                                                                                                                                                              (r$669)
                                                                                                                                                                                                                                                                              ((#((record-marker)
                                                                                                                                                                                                                                                                                  #((record-marker)
                                                                                                                                                                                                                                                                                    #f
                                                                                                                                                                                                                                                                                    (id args
                                                                                                                                                                                                                                                                                        body))
                                                                                                                                                                                                                                                                                  #(478
                                                                                                                                                                                                                                                                                    (k$686)
                                                                                                                                                                                                                                                                                    ((#((record-marker)
                                                                                                                                                                                                                                                                                        #((record-marker)
                                                                                                                                                                                                                                                                                          #f
                                                                                                                                                                                                                                                                                          (id args
                                                                                                                                                                                                                                                                                              body))
                                                                                                                                                                                                                                                                                        #(477
                                                                                                                                                                                                                                                                                          (r$687)
                                                                                                                                                                                                                                                                                          ((if r$687
                                                                                                                                                                                                                                                                                             (k$686 #f)
                                                                                                                                                                                                                                                                                             (in-port:get-lnum
                                                                                                                                                                                                                                                                                               #((record-marker)
                                                                                                                                                                                                                                                                                                 #((record-marker)
                                                                                                                                                                                                                                                                                                   #f
                                                                                                                                                                                                                                                                                                   (id args
                                                                                                                                                                                                                                                                                                       body))
                                                                                                                                                                                                                                                                                                 #(476
                                                                                                                                                                                                                                                                                                   (r$688)
                                                                                                                                                                                                                                                                                                   ((in-port:get-cnum
                                                                                                                                                                                                                                                                                                      #((record-marker)
                                                                                                                                                                                                                                                                                                        #((record-marker)
                                                                                                                                                                                                                                                                                                          #f
                                                                                                                                                                                                                                                                                                          (id args
                                                                                                                                                                                                                                                                                                              body))
                                                                                                                                                                                                                                                                                                        #(475
                                                                                                                                                                                                                                                                                                          (r$689)
                                                                                                                                                                                                                                                                                                          ((parse-error
                                                                                                                                                                                                                                                                                                             k$686
                                                                                                                                                                                                                                                                                                             "Unhandled input sequence"
                                                                                                                                                                                                                                                                                                             r$688
                                                                                                                                                                                                                                                                                                             r$689))))
                                                                                                                                                                                                                                                                                                      ptbl$196))))
                                                                                                                                                                                                                                                                                               ptbl$196)))))
                                                                                                                                                                                                                                                                                      (eq? #\(
                                                                                                                                                                                                                                                                                           next-c$205)))))
                                                                                                                                                                                                                                                                                #((record-marker)
                                                                                                                                                                                                                                                                                  #((record-marker)
                                                                                                                                                                                                                                                                                    #f
                                                                                                                                                                                                                                                                                    (id args
                                                                                                                                                                                                                                                                                        body))
                                                                                                                                                                                                                                                                                  #(474
                                                                                                                                                                                                                                                                                    (r$670)
                                                                                                                                                                                                                                                                                    ((#((record-marker)
                                                                                                                                                                                                                                                                                        #((record-marker)
                                                                                                                                                                                                                                                                                          #f
                                                                                                                                                                                                                                                                                          (id args
                                                                                                                                                                                                                                                                                              body))
                                                                                                                                                                                                                                                                                        #(473
                                                                                                                                                                                                                                                                                          (r$683)
                                                                                                                                                                                                                                                                                          ((#((record-marker)
                                                                                                                                                                                                                                                                                              #((record-marker)
                                                                                                                                                                                                                                                                                                #f
                                                                                                                                                                                                                                                                                                (id args
                                                                                                                                                                                                                                                                                                    body))
                                                                                                                                                                                                                                                                                              #(472
                                                                                                                                                                                                                                                                                                (r$684)
                                                                                                                                                                                                                                                                                                ((#((record-marker)
                                                                                                                                                                                                                                                                                                    #((record-marker)
                                                                                                                                                                                                                                                                                                      #f
                                                                                                                                                                                                                                                                                                      (id args
                                                                                                                                                                                                                                                                                                          body))
                                                                                                                                                                                                                                                                                                    #(471
                                                                                                                                                                                                                                                                                                      (r$685)
                                                                                                                                                                                                                                                                                                      ((parse #((record-marker)
                                                                                                                                                                                                                                                                                                                #((record-marker)
                                                                                                                                                                                                                                                                                                                  #f
                                                                                                                                                                                                                                                                                                                  (id args
                                                                                                                                                                                                                                                                                                                      body))
                                                                                                                                                                                                                                                                                                                #(470
                                                                                                                                                                                                                                                                                                                  (r$671)
                                                                                                                                                                                                                                                                                                                  ((get-toks
                                                                                                                                                                                                                                                                                                                     #((record-marker)
                                                                                                                                                                                                                                                                                                                       #((record-marker)
                                                                                                                                                                                                                                                                                                                         #f
                                                                                                                                                                                                                                                                                                                         (id args
                                                                                                                                                                                                                                                                                                                             body))
                                                                                                                                                                                                                                                                                                                       #(469
                                                                                                                                                                                                                                                                                                                         (r$672)
                                                                                                                                                                                                                                                                                                                         ((#((record-marker)
                                                                                                                                                                                                                                                                                                                             #((record-marker)
                                                                                                                                                                                                                                                                                                                               #f
                                                                                                                                                                                                                                                                                                                               (id args
                                                                                                                                                                                                                                                                                                                                   body))
                                                                                                                                                                                                                                                                                                                             #(468
                                                                                                                                                                                                                                                                                                                               (sub$211
                                                                                                                                                                                                                                                                                                                                 toks*$210)
                                                                                                                                                                                                                                                                                                                               ((#((record-marker)
                                                                                                                                                                                                                                                                                                                                   #((record-marker)
                                                                                                                                                                                                                                                                                                                                     #f
                                                                                                                                                                                                                                                                                                                                     (id args
                                                                                                                                                                                                                                                                                                                                         body))
                                                                                                                                                                                                                                                                                                                                   #(467
                                                                                                                                                                                                                                                                                                                                     (new-toks$212)
                                                                                                                                                                                                                                                                                                                                     ((#((record-marker)
                                                                                                                                                                                                                                                                                                                                         #((record-marker)
                                                                                                                                                                                                                                                                                                                                           #f
                                                                                                                                                                                                                                                                                                                                           (id args
                                                                                                                                                                                                                                                                                                                                               body))
                                                                                                                                                                                                                                                                                                                                         #(466
                                                                                                                                                                                                                                                                                                                                           (k$677)
                                                                                                                                                                                                                                                                                                                                           ((#((record-marker)
                                                                                                                                                                                                                                                                                                                                               #((record-marker)
                                                                                                                                                                                                                                                                                                                                                 #f
                                                                                                                                                                                                                                                                                                                                                 (id args
                                                                                                                                                                                                                                                                                                                                                     body))
                                                                                                                                                                                                                                                                                                                                               #(465
                                                                                                                                                                                                                                                                                                                                                 (k$681)
                                                                                                                                                                                                                                                                                                                                                 ((#((record-marker)
                                                                                                                                                                                                                                                                                                                                                     #((record-marker)
                                                                                                                                                                                                                                                                                                                                                       #f
                                                                                                                                                                                                                                                                                                                                                       (id args
                                                                                                                                                                                                                                                                                                                                                           body))
                                                                                                                                                                                                                                                                                                                                                     #(464
                                                                                                                                                                                                                                                                                                                                                       (r$682)
                                                                                                                                                                                                                                                                                                                                                       ((if r$682
                                                                                                                                                                                                                                                                                                                                                          (dotted?
                                                                                                                                                                                                                                                                                                                                                            k$681
                                                                                                                                                                                                                                                                                                                                                            sub$211)
                                                                                                                                                                                                                                                                                                                                                          (k$681 #f)))))
                                                                                                                                                                                                                                                                                                                                                   (pair? sub$211)))))
                                                                                                                                                                                                                                                                                                                                             #((record-marker)
                                                                                                                                                                                                                                                                                                                                               #((record-marker)
                                                                                                                                                                                                                                                                                                                                                 #f
                                                                                                                                                                                                                                                                                                                                                 (id args
                                                                                                                                                                                                                                                                                                                                                     body))
                                                                                                                                                                                                                                                                                                                                               #(463
                                                                                                                                                                                                                                                                                                                                                 (r$678)
                                                                                                                                                                                                                                                                                                                                                 ((if r$678
                                                                                                                                                                                                                                                                                                                                                    (in-port:get-lnum
                                                                                                                                                                                                                                                                                                                                                      #((record-marker)
                                                                                                                                                                                                                                                                                                                                                        #((record-marker)
                                                                                                                                                                                                                                                                                                                                                          #f
                                                                                                                                                                                                                                                                                                                                                          (id args
                                                                                                                                                                                                                                                                                                                                                              body))
                                                                                                                                                                                                                                                                                                                                                        #(462
                                                                                                                                                                                                                                                                                                                                                          (r$679)
                                                                                                                                                                                                                                                                                                                                                          ((in-port:get-cnum
                                                                                                                                                                                                                                                                                                                                                             #((record-marker)
                                                                                                                                                                                                                                                                                                                                                               #((record-marker)
                                                                                                                                                                                                                                                                                                                                                                 #f
                                                                                                                                                                                                                                                                                                                                                                 (id args
                                                                                                                                                                                                                                                                                                                                                                     body))
                                                                                                                                                                                                                                                                                                                                                               #(461
                                                                                                                                                                                                                                                                                                                                                                 (r$680)
                                                                                                                                                                                                                                                                                                                                                                 ((parse-error
                                                                                                                                                                                                                                                                                                                                                                    k$677
                                                                                                                                                                                                                                                                                                                                                                    "Invalid vector syntax"
                                                                                                                                                                                                                                                                                                                                                                    r$679
                                                                                                                                                                                                                                                                                                                                                                    r$680))))
                                                                                                                                                                                                                                                                                                                                                             ptbl$196))))
                                                                                                                                                                                                                                                                                                                                                      ptbl$196)
                                                                                                                                                                                                                                                                                                                                                    (k$677 (apply bytevector
                                                                                                                                                                                                                                                                                                                                                                  sub$211))))))))))
                                                                                                                                                                                                                                                                                                                                       #((record-marker)
                                                                                                                                                                                                                                                                                                                                         #((record-marker)
                                                                                                                                                                                                                                                                                                                                           #f
                                                                                                                                                                                                                                                                                                                                           (id args
                                                                                                                                                                                                                                                                                                                                               body))
                                                                                                                                                                                                                                                                                                                                         #(460
                                                                                                                                                                                                                                                                                                                                           (r$676)
                                                                                                                                                                                                                                                                                                                                           ((add-tok
                                                                                                                                                                                                                                                                                                                                              #((record-marker)
                                                                                                                                                                                                                                                                                                                                                #((record-marker)
                                                                                                                                                                                                                                                                                                                                                  #f
                                                                                                                                                                                                                                                                                                                                                  (id args
                                                                                                                                                                                                                                                                                                                                                      body))
                                                                                                                                                                                                                                                                                                                                                #(459
                                                                                                                                                                                                                                                                                                                                                  (r$675)
                                                                                                                                                                                                                                                                                                                                                  ((#((record-marker)
                                                                                                                                                                                                                                                                                                                                                      #((record-marker)
                                                                                                                                                                                                                                                                                                                                                        #f
                                                                                                                                                                                                                                                                                                                                                        (id args
                                                                                                                                                                                                                                                                                                                                                            body))
                                                                                                                                                                                                                                                                                                                                                      #(458
                                                                                                                                                                                                                                                                                                                                                        (r$673)
                                                                                                                                                                                                                                                                                                                                                        ((if all?$199
                                                                                                                                                                                                                                                                                                                                                           (#((record-marker)
                                                                                                                                                                                                                                                                                                                                                              #((record-marker)
                                                                                                                                                                                                                                                                                                                                                                #f
                                                                                                                                                                                                                                                                                                                                                                (id args
                                                                                                                                                                                                                                                                                                                                                                    body))
                                                                                                                                                                                                                                                                                                                                                              #(457
                                                                                                                                                                                                                                                                                                                                                                (r$674)
                                                                                                                                                                                                                                                                                                                                                                ((parse k$497
                                                                                                                                                                                                                                                                                                                                                                        fp$202
                                                                                                                                                                                                                                                                                                                                                                        r$674
                                                                                                                                                                                                                                                                                                                                                                        new-toks$212
                                                                                                                                                                                                                                                                                                                                                                        all?$199
                                                                                                                                                                                                                                                                                                                                                                        #f
                                                                                                                                                                                                                                                                                                                                                                        parens$197
                                                                                                                                                                                                                                                                                                                                                                        ptbl$196))))
                                                                                                                                                                                                                                                                                                                                                            '())
                                                                                                                                                                                                                                                                                                                                                           (k$497 (car new-toks$212))))))
                                                                                                                                                                                                                                                                                                                                                    (set! new-toks$212
                                                                                                                                                                                                                                                                                                                                                      r$675)))))
                                                                                                                                                                                                                                                                                                                                              r$676
                                                                                                                                                                                                                                                                                                                                              toks*$210))))))))
                                                                                                                                                                                                                                                                                                                                 #f))))
                                                                                                                                                                                                                                                                                                                           r$671
                                                                                                                                                                                                                                                                                                                           r$672))))
                                                                                                                                                                                                                                                                                                                     tok$201
                                                                                                                                                                                                                                                                                                                     toks$200))))
                                                                                                                                                                                                                                                                                                              fp$202
                                                                                                                                                                                                                                                                                                              r$683
                                                                                                                                                                                                                                                                                                              r$684
                                                                                                                                                                                                                                                                                                              #t
                                                                                                                                                                                                                                                                                                              #f
                                                                                                                                                                                                                                                                                                              r$685
                                                                                                                                                                                                                                                                                                              ptbl$196))))
                                                                                                                                                                                                                                                                                                  (+ parens$197
                                                                                                                                                                                                                                                                                                     1)))))
                                                                                                                                                                                                                                                                                            '()))))
                                                                                                                                                                                                                                                                                      '()))))))))
                                                                                                                                                                                                                                                                          (set! next-c$205
                                                                                                                                                                                                                                                                            r$690)))))
                                                                                                                                                                                                                                                                    (read-char
                                                                                                                                                                                                                                                                      fp$202)))))))))
                                                                                                                                                                                                                                                        (set! next-c$205
                                                                                                                                                                                                                                                          r$695)))))
                                                                                                                                                                                                                                                  (read-char
                                                                                                                                                                                                                                                    fp$202))))))
                                                                                                                                                                                                                                           (#((record-marker)
                                                                                                                                                                                                                                              #((record-marker)
                                                                                                                                                                                                                                                #f
                                                                                                                                                                                                                                                (id args
                                                                                                                                                                                                                                                    body))
                                                                                                                                                                                                                                              #(529
                                                                                                                                                                                                                                                (r$696)
                                                                                                                                                                                                                                                ((if r$696
                                                                                                                                                                                                                                                   (#((record-marker)
                                                                                                                                                                                                                                                      #((record-marker)
                                                                                                                                                                                                                                                        #f
                                                                                                                                                                                                                                                        (id args
                                                                                                                                                                                                                                                            body))
                                                                                                                                                                                                                                                      #(506
                                                                                                                                                                                                                                                        ()
                                                                                                                                                                                                                                                        ((#((record-marker)
                                                                                                                                                                                                                                                            #((record-marker)
                                                                                                                                                                                                                                                              #f
                                                                                                                                                                                                                                                              (id args
                                                                                                                                                                                                                                                                  body))
                                                                                                                                                                                                                                                            #(505
                                                                                                                                                                                                                                                              (r$709)
                                                                                                                                                                                                                                                              ((#((record-marker)
                                                                                                                                                                                                                                                                  #((record-marker)
                                                                                                                                                                                                                                                                    #f
                                                                                                                                                                                                                                                                    (id args
                                                                                                                                                                                                                                                                        body))
                                                                                                                                                                                                                                                                  #(504
                                                                                                                                                                                                                                                                    (r$710)
                                                                                                                                                                                                                                                                    ((#((record-marker)
                                                                                                                                                                                                                                                                        #((record-marker)
                                                                                                                                                                                                                                                                          #f
                                                                                                                                                                                                                                                                          (id args
                                                                                                                                                                                                                                                                              body))
                                                                                                                                                                                                                                                                        #(503
                                                                                                                                                                                                                                                                          (r$711)
                                                                                                                                                                                                                                                                          ((parse #((record-marker)
                                                                                                                                                                                                                                                                                    #((record-marker)
                                                                                                                                                                                                                                                                                      #f
                                                                                                                                                                                                                                                                                      (id args
                                                                                                                                                                                                                                                                                          body))
                                                                                                                                                                                                                                                                                    #(502
                                                                                                                                                                                                                                                                                      (r$697)
                                                                                                                                                                                                                                                                                      ((get-toks
                                                                                                                                                                                                                                                                                         #((record-marker)
                                                                                                                                                                                                                                                                                           #((record-marker)
                                                                                                                                                                                                                                                                                             #f
                                                                                                                                                                                                                                                                                             (id args
                                                                                                                                                                                                                                                                                                 body))
                                                                                                                                                                                                                                                                                           #(501
                                                                                                                                                                                                                                                                                             (r$698)
                                                                                                                                                                                                                                                                                             ((#((record-marker)
                                                                                                                                                                                                                                                                                                 #((record-marker)
                                                                                                                                                                                                                                                                                                   #f
                                                                                                                                                                                                                                                                                                   (id args
                                                                                                                                                                                                                                                                                                       body))
                                                                                                                                                                                                                                                                                                 #(500
                                                                                                                                                                                                                                                                                                   (sub$208
                                                                                                                                                                                                                                                                                                     toks*$207)
                                                                                                                                                                                                                                                                                                   ((#((record-marker)
                                                                                                                                                                                                                                                                                                       #((record-marker)
                                                                                                                                                                                                                                                                                                         #f
                                                                                                                                                                                                                                                                                                         (id args
                                                                                                                                                                                                                                                                                                             body))
                                                                                                                                                                                                                                                                                                       #(499
                                                                                                                                                                                                                                                                                                         (new-toks$209)
                                                                                                                                                                                                                                                                                                         ((#((record-marker)
                                                                                                                                                                                                                                                                                                             #((record-marker)
                                                                                                                                                                                                                                                                                                               #f
                                                                                                                                                                                                                                                                                                               (id args
                                                                                                                                                                                                                                                                                                                   body))
                                                                                                                                                                                                                                                                                                             #(498
                                                                                                                                                                                                                                                                                                               (k$703)
                                                                                                                                                                                                                                                                                                               ((#((record-marker)
                                                                                                                                                                                                                                                                                                                   #((record-marker)
                                                                                                                                                                                                                                                                                                                     #f
                                                                                                                                                                                                                                                                                                                     (id args
                                                                                                                                                                                                                                                                                                                         body))
                                                                                                                                                                                                                                                                                                                   #(497
                                                                                                                                                                                                                                                                                                                     (k$707)
                                                                                                                                                                                                                                                                                                                     ((#((record-marker)
                                                                                                                                                                                                                                                                                                                         #((record-marker)
                                                                                                                                                                                                                                                                                                                           #f
                                                                                                                                                                                                                                                                                                                           (id args
                                                                                                                                                                                                                                                                                                                               body))
                                                                                                                                                                                                                                                                                                                         #(496
                                                                                                                                                                                                                                                                                                                           (r$708)
                                                                                                                                                                                                                                                                                                                           ((if r$708
                                                                                                                                                                                                                                                                                                                              (dotted?
                                                                                                                                                                                                                                                                                                                                k$707
                                                                                                                                                                                                                                                                                                                                sub$208)
                                                                                                                                                                                                                                                                                                                              (k$707 #f)))))
                                                                                                                                                                                                                                                                                                                       (pair? sub$208)))))
                                                                                                                                                                                                                                                                                                                 #((record-marker)
                                                                                                                                                                                                                                                                                                                   #((record-marker)
                                                                                                                                                                                                                                                                                                                     #f
                                                                                                                                                                                                                                                                                                                     (id args
                                                                                                                                                                                                                                                                                                                         body))
                                                                                                                                                                                                                                                                                                                   #(495
                                                                                                                                                                                                                                                                                                                     (r$704)
                                                                                                                                                                                                                                                                                                                     ((if r$704
                                                                                                                                                                                                                                                                                                                        (in-port:get-lnum
                                                                                                                                                                                                                                                                                                                          #((record-marker)
                                                                                                                                                                                                                                                                                                                            #((record-marker)
                                                                                                                                                                                                                                                                                                                              #f
                                                                                                                                                                                                                                                                                                                              (id args
                                                                                                                                                                                                                                                                                                                                  body))
                                                                                                                                                                                                                                                                                                                            #(494
                                                                                                                                                                                                                                                                                                                              (r$705)
                                                                                                                                                                                                                                                                                                                              ((in-port:get-cnum
                                                                                                                                                                                                                                                                                                                                 #((record-marker)
                                                                                                                                                                                                                                                                                                                                   #((record-marker)
                                                                                                                                                                                                                                                                                                                                     #f
                                                                                                                                                                                                                                                                                                                                     (id args
                                                                                                                                                                                                                                                                                                                                         body))
                                                                                                                                                                                                                                                                                                                                   #(493
                                                                                                                                                                                                                                                                                                                                     (r$706)
                                                                                                                                                                                                                                                                                                                                     ((parse-error
                                                                                                                                                                                                                                                                                                                                        k$703
                                                                                                                                                                                                                                                                                                                                        "Invalid vector syntax"
                                                                                                                                                                                                                                                                                                                                        r$705
                                                                                                                                                                                                                                                                                                                                        r$706))))
                                                                                                                                                                                                                                                                                                                                 ptbl$196))))
                                                                                                                                                                                                                                                                                                                          ptbl$196)
                                                                                                                                                                                                                                                                                                                        (k$703 (list->vector
                                                                                                                                                                                                                                                                                                                                 sub$208))))))))))
                                                                                                                                                                                                                                                                                                           #((record-marker)
                                                                                                                                                                                                                                                                                                             #((record-marker)
                                                                                                                                                                                                                                                                                                               #f
                                                                                                                                                                                                                                                                                                               (id args
                                                                                                                                                                                                                                                                                                                   body))
                                                                                                                                                                                                                                                                                                             #(492
                                                                                                                                                                                                                                                                                                               (r$702)
                                                                                                                                                                                                                                                                                                               ((add-tok
                                                                                                                                                                                                                                                                                                                  #((record-marker)
                                                                                                                                                                                                                                                                                                                    #((record-marker)
                                                                                                                                                                                                                                                                                                                      #f
                                                                                                                                                                                                                                                                                                                      (id args
                                                                                                                                                                                                                                                                                                                          body))
                                                                                                                                                                                                                                                                                                                    #(491
                                                                                                                                                                                                                                                                                                                      (r$701)
                                                                                                                                                                                                                                                                                                                      ((#((record-marker)
                                                                                                                                                                                                                                                                                                                          #((record-marker)
                                                                                                                                                                                                                                                                                                                            #f
                                                                                                                                                                                                                                                                                                                            (id args
                                                                                                                                                                                                                                                                                                                                body))
                                                                                                                                                                                                                                                                                                                          #(490
                                                                                                                                                                                                                                                                                                                            (r$699)
                                                                                                                                                                                                                                                                                                                            ((if all?$199
                                                                                                                                                                                                                                                                                                                               (#((record-marker)
                                                                                                                                                                                                                                                                                                                                  #((record-marker)
                                                                                                                                                                                                                                                                                                                                    #f
                                                                                                                                                                                                                                                                                                                                    (id args
                                                                                                                                                                                                                                                                                                                                        body))
                                                                                                                                                                                                                                                                                                                                  #(489
                                                                                                                                                                                                                                                                                                                                    (r$700)
                                                                                                                                                                                                                                                                                                                                    ((parse k$497
                                                                                                                                                                                                                                                                                                                                            fp$202
                                                                                                                                                                                                                                                                                                                                            r$700
                                                                                                                                                                                                                                                                                                                                            new-toks$209
                                                                                                                                                                                                                                                                                                                                            all?$199
                                                                                                                                                                                                                                                                                                                                            #f
                                                                                                                                                                                                                                                                                                                                            parens$197
                                                                                                                                                                                                                                                                                                                                            ptbl$196))))
                                                                                                                                                                                                                                                                                                                                '())
                                                                                                                                                                                                                                                                                                                               (k$497 (car new-toks$209))))))
                                                                                                                                                                                                                                                                                                                        (set! new-toks$209
                                                                                                                                                                                                                                                                                                                          r$701)))))
                                                                                                                                                                                                                                                                                                                  r$702
                                                                                                                                                                                                                                                                                                                  toks*$207))))))))
                                                                                                                                                                                                                                                                                                     #f))))
                                                                                                                                                                                                                                                                                               r$697
                                                                                                                                                                                                                                                                                               r$698))))
                                                                                                                                                                                                                                                                                         tok$201
                                                                                                                                                                                                                                                                                         toks$200))))
                                                                                                                                                                                                                                                                                  fp$202
                                                                                                                                                                                                                                                                                  r$709
                                                                                                                                                                                                                                                                                  r$710
                                                                                                                                                                                                                                                                                  #t
                                                                                                                                                                                                                                                                                  #f
                                                                                                                                                                                                                                                                                  r$711
                                                                                                                                                                                                                                                                                  ptbl$196))))
                                                                                                                                                                                                                                                                      (+ parens$197
                                                                                                                                                                                                                                                                         1)))))
                                                                                                                                                                                                                                                                '()))))
                                                                                                                                                                                                                                                          '())))))
                                                                                                                                                                                                                                                   (#((record-marker)
                                                                                                                                                                                                                                                      #((record-marker)
                                                                                                                                                                                                                                                        #f
                                                                                                                                                                                                                                                        (id args
                                                                                                                                                                                                                                                            body))
                                                                                                                                                                                                                                                      #(528
                                                                                                                                                                                                                                                        (r$712)
                                                                                                                                                                                                                                                        ((if r$712
                                                                                                                                                                                                                                                           (#((record-marker)
                                                                                                                                                                                                                                                              #((record-marker)
                                                                                                                                                                                                                                                                #f
                                                                                                                                                                                                                                                                (id args
                                                                                                                                                                                                                                                                    body))
                                                                                                                                                                                                                                                              #(511
                                                                                                                                                                                                                                                                ()
                                                                                                                                                                                                                                                                ((read-pound
                                                                                                                                                                                                                                                                   #((record-marker)
                                                                                                                                                                                                                                                                     #((record-marker)
                                                                                                                                                                                                                                                                       #f
                                                                                                                                                                                                                                                                       (id args
                                                                                                                                                                                                                                                                           body))
                                                                                                                                                                                                                                                                     #(510
                                                                                                                                                                                                                                                                       (r$715)
                                                                                                                                                                                                                                                                       ((#((record-marker)
                                                                                                                                                                                                                                                                           #((record-marker)
                                                                                                                                                                                                                                                                             #f
                                                                                                                                                                                                                                                                             (id args
                                                                                                                                                                                                                                                                                 body))
                                                                                                                                                                                                                                                                           #(508
                                                                                                                                                                                                                                                                             (new-toks$206)
                                                                                                                                                                                                                                                                             ((if all?$199
                                                                                                                                                                                                                                                                                (#((record-marker)
                                                                                                                                                                                                                                                                                   #((record-marker)
                                                                                                                                                                                                                                                                                     #f
                                                                                                                                                                                                                                                                                     (id args
                                                                                                                                                                                                                                                                                         body))
                                                                                                                                                                                                                                                                                   #(507
                                                                                                                                                                                                                                                                                     (r$714)
                                                                                                                                                                                                                                                                                     ((parse k$497
                                                                                                                                                                                                                                                                                             fp$202
                                                                                                                                                                                                                                                                                             r$714
                                                                                                                                                                                                                                                                                             new-toks$206
                                                                                                                                                                                                                                                                                             all?$199
                                                                                                                                                                                                                                                                                             #f
                                                                                                                                                                                                                                                                                             parens$197
                                                                                                                                                                                                                                                                                             ptbl$196))))
                                                                                                                                                                                                                                                                                 '())
                                                                                                                                                                                                                                                                                (k$497 (car new-toks$206))))))
                                                                                                                                                                                                                                                                         (cons r$715
                                                                                                                                                                                                                                                                               toks$200)))))
                                                                                                                                                                                                                                                                   fp$202
                                                                                                                                                                                                                                                                   ptbl$196)))))
                                                                                                                                                                                                                                                           (#((record-marker)
                                                                                                                                                                                                                                                              #((record-marker)
                                                                                                                                                                                                                                                                #f
                                                                                                                                                                                                                                                                (id args
                                                                                                                                                                                                                                                                    body))
                                                                                                                                                                                                                                                              #(527
                                                                                                                                                                                                                                                                (r$716)
                                                                                                                                                                                                                                                                ((if r$716
                                                                                                                                                                                                                                                                   (#((record-marker)
                                                                                                                                                                                                                                                                      #((record-marker)
                                                                                                                                                                                                                                                                        #f
                                                                                                                                                                                                                                                                        (id args
                                                                                                                                                                                                                                                                            body))
                                                                                                                                                                                                                                                                      #(523
                                                                                                                                                                                                                                                                        ()
                                                                                                                                                                                                                                                                        ((#((record-marker)
                                                                                                                                                                                                                                                                            #((record-marker)
                                                                                                                                                                                                                                                                              #f
                                                                                                                                                                                                                                                                              (id args
                                                                                                                                                                                                                                                                                  body))
                                                                                                                                                                                                                                                                            #(522
                                                                                                                                                                                                                                                                              (r$724)
                                                                                                                                                                                                                                                                              ((#((record-marker)
                                                                                                                                                                                                                                                                                  #((record-marker)
                                                                                                                                                                                                                                                                                    #f
                                                                                                                                                                                                                                                                                    (id args
                                                                                                                                                                                                                                                                                        body))
                                                                                                                                                                                                                                                                                  #(521
                                                                                                                                                                                                                                                                                    (r$725)
                                                                                                                                                                                                                                                                                    ((parse #((record-marker)
                                                                                                                                                                                                                                                                                              #((record-marker)
                                                                                                                                                                                                                                                                                                #f
                                                                                                                                                                                                                                                                                                (id args
                                                                                                                                                                                                                                                                                                    body))
                                                                                                                                                                                                                                                                                              #(520
                                                                                                                                                                                                                                                                                                (r$717)
                                                                                                                                                                                                                                                                                                ((#((record-marker)
                                                                                                                                                                                                                                                                                                    #((record-marker)
                                                                                                                                                                                                                                                                                                      #f
                                                                                                                                                                                                                                                                                                      (id args
                                                                                                                                                                                                                                                                                                          body))
                                                                                                                                                                                                                                                                                                    #(519
                                                                                                                                                                                                                                                                                                      (k$722)
                                                                                                                                                                                                                                                                                                      ((if all?$199
                                                                                                                                                                                                                                                                                                         (k$722 #f)
                                                                                                                                                                                                                                                                                                         (#((record-marker)
                                                                                                                                                                                                                                                                                                            #((record-marker)
                                                                                                                                                                                                                                                                                                              #f
                                                                                                                                                                                                                                                                                                              (id args
                                                                                                                                                                                                                                                                                                                  body))
                                                                                                                                                                                                                                                                                                            #(518
                                                                                                                                                                                                                                                                                                              (r$723)
                                                                                                                                                                                                                                                                                                              ((not k$722
                                                                                                                                                                                                                                                                                                                    r$723))))
                                                                                                                                                                                                                                                                                                          (null? tok$201))))))
                                                                                                                                                                                                                                                                                                  #((record-marker)
                                                                                                                                                                                                                                                                                                    #((record-marker)
                                                                                                                                                                                                                                                                                                      #f
                                                                                                                                                                                                                                                                                                      (id args
                                                                                                                                                                                                                                                                                                          body))
                                                                                                                                                                                                                                                                                                    #(517
                                                                                                                                                                                                                                                                                                      (r$718)
                                                                                                                                                                                                                                                                                                      ((if r$718
                                                                                                                                                                                                                                                                                                         (#((record-marker)
                                                                                                                                                                                                                                                                                                            #((record-marker)
                                                                                                                                                                                                                                                                                                              #f
                                                                                                                                                                                                                                                                                                              (id args
                                                                                                                                                                                                                                                                                                                  body))
                                                                                                                                                                                                                                                                                                            #(515
                                                                                                                                                                                                                                                                                                              ()
                                                                                                                                                                                                                                                                                                              ((in-port:set-buf!
                                                                                                                                                                                                                                                                                                                 #((record-marker)
                                                                                                                                                                                                                                                                                                                   #((record-marker)
                                                                                                                                                                                                                                                                                                                     #f
                                                                                                                                                                                                                                                                                                                     (id args
                                                                                                                                                                                                                                                                                                                         body))
                                                                                                                                                                                                                                                                                                                   #(514
                                                                                                                                                                                                                                                                                                                     (r$719)
                                                                                                                                                                                                                                                                                                                     ((->tok #((record-marker)
                                                                                                                                                                                                                                                                                                                               #((record-marker)
                                                                                                                                                                                                                                                                                                                                 #f
                                                                                                                                                                                                                                                                                                                                 (id args
                                                                                                                                                                                                                                                                                                                                     body))
                                                                                                                                                                                                                                                                                                                               #(513
                                                                                                                                                                                                                                                                                                                                 (r$721)
                                                                                                                                                                                                                                                                                                                                 ((add-tok
                                                                                                                                                                                                                                                                                                                                    #((record-marker)
                                                                                                                                                                                                                                                                                                                                      #((record-marker)
                                                                                                                                                                                                                                                                                                                                        #f
                                                                                                                                                                                                                                                                                                                                        (id args
                                                                                                                                                                                                                                                                                                                                            body))
                                                                                                                                                                                                                                                                                                                                      #(512
                                                                                                                                                                                                                                                                                                                                        (r$720)
                                                                                                                                                                                                                                                                                                                                        ((k$497 (car r$720)))))
                                                                                                                                                                                                                                                                                                                                    r$721
                                                                                                                                                                                                                                                                                                                                    toks$200))))
                                                                                                                                                                                                                                                                                                                             tok$201))))
                                                                                                                                                                                                                                                                                                                 ptbl$196
                                                                                                                                                                                                                                                                                                                 c$204)))))
                                                                                                                                                                                                                                                                                                         (#((record-marker)
                                                                                                                                                                                                                                                                                                            #((record-marker)
                                                                                                                                                                                                                                                                                                              #f
                                                                                                                                                                                                                                                                                                              (id args
                                                                                                                                                                                                                                                                                                                  body))
                                                                                                                                                                                                                                                                                                            #(516
                                                                                                                                                                                                                                                                                                              ()
                                                                                                                                                                                                                                                                                                              ((parse k$497
                                                                                                                                                                                                                                                                                                                      fp$202
                                                                                                                                                                                                                                                                                                                      tok$201
                                                                                                                                                                                                                                                                                                                      toks$200
                                                                                                                                                                                                                                                                                                                      all?$199
                                                                                                                                                                                                                                                                                                                      #f
                                                                                                                                                                                                                                                                                                                      parens$197
                                                                                                                                                                                                                                                                                                                      ptbl$196)))))))))))))
                                                                                                                                                                                                                                                                                            fp$202
                                                                                                                                                                                                                                                                                            r$724
                                                                                                                                                                                                                                                                                            r$725
                                                                                                                                                                                                                                                                                            #f
                                                                                                                                                                                                                                                                                            #f
                                                                                                                                                                                                                                                                                            0
                                                                                                                                                                                                                                                                                            ptbl$196))))
                                                                                                                                                                                                                                                                                '()))))
                                                                                                                                                                                                                                                                          '())))))
                                                                                                                                                                                                                                                                   (#((record-marker)
                                                                                                                                                                                                                                                                      #((record-marker)
                                                                                                                                                                                                                                                                        #f
                                                                                                                                                                                                                                                                        (id args
                                                                                                                                                                                                                                                                            body))
                                                                                                                                                                                                                                                                      #(526
                                                                                                                                                                                                                                                                        ()
                                                                                                                                                                                                                                                                        ((in-port:get-lnum
                                                                                                                                                                                                                                                                           #((record-marker)
                                                                                                                                                                                                                                                                             #((record-marker)
                                                                                                                                                                                                                                                                               #f
                                                                                                                                                                                                                                                                               (id args
                                                                                                                                                                                                                                                                                   body))
                                                                                                                                                                                                                                                                             #(525
                                                                                                                                                                                                                                                                               (r$726)
                                                                                                                                                                                                                                                                               ((in-port:get-cnum
                                                                                                                                                                                                                                                                                  #((record-marker)
                                                                                                                                                                                                                                                                                    #((record-marker)
                                                                                                                                                                                                                                                                                      #f
                                                                                                                                                                                                                                                                                      (id args
                                                                                                                                                                                                                                                                                          body))
                                                                                                                                                                                                                                                                                    #(524
                                                                                                                                                                                                                                                                                      (r$727)
                                                                                                                                                                                                                                                                                      ((parse-error
                                                                                                                                                                                                                                                                                         k$497
                                                                                                                                                                                                                                                                                         "Unhandled input sequence"
                                                                                                                                                                                                                                                                                         r$726
                                                                                                                                                                                                                                                                                         r$727))))
                                                                                                                                                                                                                                                                                  ptbl$196))))
                                                                                                                                                                                                                                                                           ptbl$196)))))))))
                                                                                                                                                                                                                                                            (eq? #\;
                                                                                                                                                                                                                                                                 next-c$205))))))
                                                                                                                                                                                                                                                    (eq? #\\
                                                                                                                                                                                                                                                         next-c$205))))))
                                                                                                                                                                                                                                            (eq? #\(
                                                                                                                                                                                                                                                 next-c$205))))))
                                                                                                                                                                                                                                    (eq? #\u
                                                                                                                                                                                                                                         next-c$205))))))
                                                                                                                                                                                                                            (eq? #\x
                                                                                                                                                                                                                                 next-c$205))))))
                                                                                                                                                                                                                    (eq? #\o
                                                                                                                                                                                                                         next-c$205))))))
                                                                                                                                                                                                            (eq? #\b
                                                                                                                                                                                                                 next-c$205))))))
                                                                                                                                                                                                    (eq? #\i
                                                                                                                                                                                                         next-c$205))))))
                                                                                                                                                                                            (eq? #\e
                                                                                                                                                                                                 next-c$205))))))
                                                                                                                                                                                    (eq? #\f
                                                                                                                                                                                         next-c$205))))))
                                                                                                                                                                            (eq? #\t
                                                                                                                                                                                 next-c$205))))))
                                                                                                                                                                    (eq? #\|
                                                                                                                                                                         next-c$205)))))
                                                                                                                                                              ptbl$196
                                                                                                                                                              r$728))))
                                                                                                                                                       (+ 1
                                                                                                                                                          r$729)))))
                                                                                                                                                 ptbl$196))))
                                                                                                                                          (read-char
                                                                                                                                            fp$202))
                                                                                                                                         (#((record-marker)
                                                                                                                                            #((record-marker)
                                                                                                                                              #f
                                                                                                                                              (id args
                                                                                                                                                  body))
                                                                                                                                            #(544
                                                                                                                                              (r$730)
                                                                                                                                              ((parse k$497
                                                                                                                                                      fp$202
                                                                                                                                                      r$730
                                                                                                                                                      toks$200
                                                                                                                                                      all?$199
                                                                                                                                                      #f
                                                                                                                                                      parens$197
                                                                                                                                                      ptbl$196))))
                                                                                                                                          (cons c$204
                                                                                                                                                tok$201))))))
                                                                                                                                  (null? tok$201))))))
                                                                                                                           (#((record-marker)
                                                                                                                              #((record-marker)
                                                                                                                                #f
                                                                                                                                (id args
                                                                                                                                    body))
                                                                                                                              #(550
                                                                                                                                (r$731)
                                                                                                                                ((if r$731
                                                                                                                                   (#((record-marker)
                                                                                                                                      #((record-marker)
                                                                                                                                        #f
                                                                                                                                        (id args
                                                                                                                                            body))
                                                                                                                                      #(547
                                                                                                                                        ()
                                                                                                                                        ((parse-literal-identifier
                                                                                                                                           k$497
                                                                                                                                           fp$202
                                                                                                                                           toks$200
                                                                                                                                           all?$199
                                                                                                                                           parens$197
                                                                                                                                           ptbl$196)))))
                                                                                                                                   (#((record-marker)
                                                                                                                                      #((record-marker)
                                                                                                                                        #f
                                                                                                                                        (id args
                                                                                                                                            body))
                                                                                                                                      #(549
                                                                                                                                        ()
                                                                                                                                        ((#((record-marker)
                                                                                                                                            #((record-marker)
                                                                                                                                              #f
                                                                                                                                              (id args
                                                                                                                                                  body))
                                                                                                                                            #(548
                                                                                                                                              (r$732)
                                                                                                                                              ((parse k$497
                                                                                                                                                      fp$202
                                                                                                                                                      r$732
                                                                                                                                                      toks$200
                                                                                                                                                      all?$199
                                                                                                                                                      #f
                                                                                                                                                      parens$197
                                                                                                                                                      ptbl$196))))
                                                                                                                                          (cons c$204
                                                                                                                                                tok$201))))))))))
                                                                                                                            (eq? c$204
                                                                                                                                 #\|))))))
                                                                                                                    (eq? c$204
                                                                                                                         #\#))))))
                                                                                                            (eq? c$204
                                                                                                                 #\"))))))
                                                                                                    (eq? c$204
                                                                                                         #\)))))))
                                                                                            (eq? c$204
                                                                                                 #\())))))
                                                                                    (eq? c$204
                                                                                         #\,))))))
                                                                            (eq? c$204
                                                                                 #\`))))))
                                                                    (eq? c$204
                                                                         #\'))))))
                                                            (eq? c$204 #\;))))))
                                                    c$204))))))
                                         (eof-object? c$204)))))))))
                             ptbl$196
                             r$735))))
                      (+ 1 r$736)))))
                ptbl$196)))))))))
 (define read-pound
   #((record-marker)
     #((record-marker) #f (id args body))
     #(266
       (k$438 fp$183 ptbl$182)
       ((#((record-marker)
           #((record-marker) #f (id args body))
           #(265
             (done$187 loop$186)
             ((#((record-marker)
                 #((record-marker) #f (id args body))
                 #(264
                   ()
                   ((#((record-marker)
                       #((record-marker) #f (id args body))
                       #(211
                         (r$456)
                         ((#((record-marker)
                             #((record-marker) #f (id args body))
                             #(210
                               (r$439)
                               ((#((record-marker)
                                   #((record-marker) #f (id args body))
                                   #(191
                                     (r$442)
                                     ((#((record-marker)
                                         #((record-marker) #f (id args body))
                                         #(190
                                           (r$440)
                                           ((#((record-marker)
                                               #((record-marker)
                                                 #f
                                                 (id args body))
                                               #(189
                                                 (r$441)
                                                 ((loop$186 k$438 r$441))))
                                             '()))))
                                       (set! loop$186 r$442)))))
                                 #((record-marker)
                                   #((record-marker) #f (id args body))
                                   #(209
                                     (k$443 buf$190)
                                     ((#((record-marker)
                                         #((record-marker) #f (id args body))
                                         #(207
                                           (c$191)
                                           ((#((record-marker)
                                               #((record-marker)
                                                 #f
                                                 (id args body))
                                               #(205
                                                 (tmp$192)
                                                 ((#((record-marker)
                                                     #((record-marker)
                                                       #f
                                                       (id args body))
                                                     #(204
                                                       (k$449)
                                                       ((if tmp$192
                                                          (k$449 tmp$192)
                                                          (#((record-marker)
                                                             #((record-marker)
                                                               #f
                                                               (id args body))
                                                             #(203
                                                               (k$453)
                                                               ((char-whitespace?
                                                                  #((record-marker)
                                                                    #((record-marker)
                                                                      #f
                                                                      (id args
                                                                          body))
                                                                    #(202
                                                                      (r$454)
                                                                      ((if r$454
                                                                         (#((record-marker)
                                                                            #((record-marker)
                                                                              #f
                                                                              (id args
                                                                                  body))
                                                                            #(201
                                                                              (r$455)
                                                                              ((k$453 (> r$455
                                                                                         0)))))
                                                                          (length
                                                                            buf$190))
                                                                         (k$453 #f)))))
                                                                  c$191))))
                                                           #((record-marker)
                                                             #((record-marker)
                                                               #f
                                                               (id args body))
                                                             #(199
                                                               (tmp$193)
                                                               ((if tmp$193
                                                                  (k$449 tmp$193)
                                                                  (#((record-marker)
                                                                     #((record-marker)
                                                                       #f
                                                                       (id args
                                                                           body))
                                                                     #(198
                                                                       (r$452)
                                                                       ((#((record-marker)
                                                                           #((record-marker)
                                                                             #f
                                                                             (id args
                                                                                 body))
                                                                           #(197
                                                                             (r$451)
                                                                             ((if r$451
                                                                                (k$449 (equal?
                                                                                         c$191
                                                                                         #\)))
                                                                                (k$449 #f)))))
                                                                         (> r$452
                                                                            0)))))
                                                                   (length
                                                                     buf$190)))))))))))
                                                   #((record-marker)
                                                     #((record-marker)
                                                       #f
                                                       (id args body))
                                                     #(196
                                                       (r$445)
                                                       ((if r$445
                                                          (#((record-marker)
                                                             #((record-marker)
                                                               #f
                                                               (id args body))
                                                             #(192
                                                               ()
                                                               ((done$187
                                                                  k$443
                                                                  buf$190)))))
                                                          (#((record-marker)
                                                             #((record-marker)
                                                               #f
                                                               (id args body))
                                                             #(195
                                                               ()
                                                               ((#((record-marker)
                                                                   #((record-marker)
                                                                     #f
                                                                     (id args
                                                                         body))
                                                                   #(194
                                                                     (r$447)
                                                                     ((#((record-marker)
                                                                         #((record-marker)
                                                                           #f
                                                                           (id args
                                                                               body))
                                                                         #(193
                                                                           (r$446)
                                                                           ((loop$186
                                                                              k$443
                                                                              r$446))))
                                                                       (cons r$447
                                                                             buf$190)))))
                                                                 (read-char
                                                                   fp$183))))))))))))))
                                             (eof-object? c$191)))))
                                       (peek-char fp$183)))))))))
                           (set! done$187 r$456)))))
                     #((record-marker)
                       #((record-marker) #f (id args body))
                       #(263
                         (k$457 raw-buf$194)
                         ((reverse
                            #((record-marker)
                              #((record-marker) #f (id args body))
                              #(261
                                (buf$195)
                                ((#((record-marker)
                                    #((record-marker) #f (id args body))
                                    #(260
                                      (r$494)
                                      ((#((record-marker)
                                          #((record-marker) #f (id args body))
                                          #(259
                                            (r$459)
                                            ((if r$459
                                               (#((record-marker)
                                                  #((record-marker)
                                                    #f
                                                    (id args body))
                                                  #(214
                                                    ()
                                                    ((in-port:get-lnum
                                                       #((record-marker)
                                                         #((record-marker)
                                                           #f
                                                           (id args body))
                                                         #(213
                                                           (r$460)
                                                           ((in-port:get-cnum
                                                              #((record-marker)
                                                                #((record-marker)
                                                                  #f
                                                                  (id args
                                                                      body))
                                                                #(212
                                                                  (r$461)
                                                                  ((parse-error
                                                                     k$457
                                                                     "missing character"
                                                                     r$460
                                                                     r$461))))
                                                              ptbl$182))))
                                                       ptbl$182)))))
                                               (#((record-marker)
                                                  #((record-marker)
                                                    #f
                                                    (id args body))
                                                  #(258
                                                    (r$493)
                                                    ((#((record-marker)
                                                        #((record-marker)
                                                          #f
                                                          (id args body))
                                                        #(257
                                                          (r$462)
                                                          ((if r$462
                                                             (#((record-marker)
                                                                #((record-marker)
                                                                  #f
                                                                  (id args
                                                                      body))
                                                                #(215
                                                                  ()
                                                                  ((k$457 (car buf$195))))))
                                                             (#((record-marker)
                                                                #((record-marker)
                                                                  #f
                                                                  (id args
                                                                      body))
                                                                #(256
                                                                  (r$492)
                                                                  ((#((record-marker)
                                                                      #((record-marker)
                                                                        #f
                                                                        (id args
                                                                            body))
                                                                      #(255
                                                                        (r$463)
                                                                        ((if r$463
                                                                           (#((record-marker)
                                                                              #((record-marker)
                                                                                #f
                                                                                (id args
                                                                                    body))
                                                                              #(216
                                                                                ()
                                                                                ((k$457 #\)))))
                                                                           (#((record-marker)
                                                                              #((record-marker)
                                                                                #f
                                                                                (id args
                                                                                    body))
                                                                              #(254
                                                                                (r$491)
                                                                                ((#((record-marker)
                                                                                    #((record-marker)
                                                                                      #f
                                                                                      (id args
                                                                                          body))
                                                                                    #(253
                                                                                      (r$464)
                                                                                      ((if r$464
                                                                                         (#((record-marker)
                                                                                            #((record-marker)
                                                                                              #f
                                                                                              (id args
                                                                                                  body))
                                                                                            #(217
                                                                                              ()
                                                                                              ((k$457 #\)))))
                                                                                         (#((record-marker)
                                                                                            #((record-marker)
                                                                                              #f
                                                                                              (id args
                                                                                                  body))
                                                                                            #(252
                                                                                              (r$490)
                                                                                              ((#((record-marker)
                                                                                                  #((record-marker)
                                                                                                    #f
                                                                                                    (id args
                                                                                                        body))
                                                                                                  #(251
                                                                                                    (r$465)
                                                                                                    ((if r$465
                                                                                                       (#((record-marker)
                                                                                                          #((record-marker)
                                                                                                            #f
                                                                                                            (id args
                                                                                                                body))
                                                                                                          #(218
                                                                                                            ()
                                                                                                            ((k$457 #\)))))
                                                                                                       (#((record-marker)
                                                                                                          #((record-marker)
                                                                                                            #f
                                                                                                            (id args
                                                                                                                body))
                                                                                                          #(250
                                                                                                            (r$489)
                                                                                                            ((#((record-marker)
                                                                                                                #((record-marker)
                                                                                                                  #f
                                                                                                                  (id args
                                                                                                                      body))
                                                                                                                #(249
                                                                                                                  (r$466)
                                                                                                                  ((if r$466
                                                                                                                     (#((record-marker)
                                                                                                                        #((record-marker)
                                                                                                                          #f
                                                                                                                          (id args
                                                                                                                              body))
                                                                                                                        #(219
                                                                                                                          ()
                                                                                                                          ((k$457 #\)))))
                                                                                                                     (#((record-marker)
                                                                                                                        #((record-marker)
                                                                                                                          #f
                                                                                                                          (id args
                                                                                                                              body))
                                                                                                                        #(248
                                                                                                                          (r$488)
                                                                                                                          ((#((record-marker)
                                                                                                                              #((record-marker)
                                                                                                                                #f
                                                                                                                                (id args
                                                                                                                                    body))
                                                                                                                              #(247
                                                                                                                                (r$467)
                                                                                                                                ((if r$467
                                                                                                                                   (#((record-marker)
                                                                                                                                      #((record-marker)
                                                                                                                                        #f
                                                                                                                                        (id args
                                                                                                                                            body))
                                                                                                                                      #(220
                                                                                                                                        ()
                                                                                                                                        ((k$457 #\newline)))))
                                                                                                                                   (#((record-marker)
                                                                                                                                      #((record-marker)
                                                                                                                                        #f
                                                                                                                                        (id args
                                                                                                                                            body))
                                                                                                                                      #(246
                                                                                                                                        (r$487)
                                                                                                                                        ((#((record-marker)
                                                                                                                                            #((record-marker)
                                                                                                                                              #f
                                                                                                                                              (id args
                                                                                                                                                  body))
                                                                                                                                            #(245
                                                                                                                                              (r$468)
                                                                                                                                              ((if r$468
                                                                                                                                                 (#((record-marker)
                                                                                                                                                    #((record-marker)
                                                                                                                                                      #f
                                                                                                                                                      (id args
                                                                                                                                                          body))
                                                                                                                                                    #(221
                                                                                                                                                      ()
                                                                                                                                                      ((k$457 #\)))))
                                                                                                                                                 (#((record-marker)
                                                                                                                                                    #((record-marker)
                                                                                                                                                      #f
                                                                                                                                                      (id args
                                                                                                                                                          body))
                                                                                                                                                    #(244
                                                                                                                                                      (r$486)
                                                                                                                                                      ((#((record-marker)
                                                                                                                                                          #((record-marker)
                                                                                                                                                            #f
                                                                                                                                                            (id args
                                                                                                                                                                body))
                                                                                                                                                          #(243
                                                                                                                                                            (r$469)
                                                                                                                                                            ((if r$469
                                                                                                                                                               (#((record-marker)
                                                                                                                                                                  #((record-marker)
                                                                                                                                                                    #f
                                                                                                                                                                    (id args
                                                                                                                                                                        body))
                                                                                                                                                                  #(222
                                                                                                                                                                    ()
                                                                                                                                                                    ((k$457 #\)))))
                                                                                                                                                               (#((record-marker)
                                                                                                                                                                  #((record-marker)
                                                                                                                                                                    #f
                                                                                                                                                                    (id args
                                                                                                                                                                        body))
                                                                                                                                                                  #(242
                                                                                                                                                                    (r$485)
                                                                                                                                                                    ((#((record-marker)
                                                                                                                                                                        #((record-marker)
                                                                                                                                                                          #f
                                                                                                                                                                          (id args
                                                                                                                                                                              body))
                                                                                                                                                                        #(241
                                                                                                                                                                          (r$470)
                                                                                                                                                                          ((if r$470
                                                                                                                                                                             (#((record-marker)
                                                                                                                                                                                #((record-marker)
                                                                                                                                                                                  #f
                                                                                                                                                                                  (id args
                                                                                                                                                                                      body))
                                                                                                                                                                                #(223
                                                                                                                                                                                  ()
                                                                                                                                                                                  ((k$457 #\space)))))
                                                                                                                                                                             (#((record-marker)
                                                                                                                                                                                #((record-marker)
                                                                                                                                                                                  #f
                                                                                                                                                                                  (id args
                                                                                                                                                                                      body))
                                                                                                                                                                                #(240
                                                                                                                                                                                  (r$484)
                                                                                                                                                                                  ((#((record-marker)
                                                                                                                                                                                      #((record-marker)
                                                                                                                                                                                        #f
                                                                                                                                                                                        (id args
                                                                                                                                                                                            body))
                                                                                                                                                                                      #(239
                                                                                                                                                                                        (r$471)
                                                                                                                                                                                        ((if r$471
                                                                                                                                                                                           (#((record-marker)
                                                                                                                                                                                              #((record-marker)
                                                                                                                                                                                                #f
                                                                                                                                                                                                (id args
                                                                                                                                                                                                    body))
                                                                                                                                                                                              #(224
                                                                                                                                                                                                ()
                                                                                                                                                                                                ((k$457 #\	)))))
                                                                                                                                                                                           (#((record-marker)
                                                                                                                                                                                              #((record-marker)
                                                                                                                                                                                                #f
                                                                                                                                                                                                (id args
                                                                                                                                                                                                    body))
                                                                                                                                                                                              #(238
                                                                                                                                                                                                (k$480)
                                                                                                                                                                                                ((#((record-marker)
                                                                                                                                                                                                    #((record-marker)
                                                                                                                                                                                                      #f
                                                                                                                                                                                                      (id args
                                                                                                                                                                                                          body))
                                                                                                                                                                                                    #(237
                                                                                                                                                                                                      (r$483)
                                                                                                                                                                                                      ((#((record-marker)
                                                                                                                                                                                                          #((record-marker)
                                                                                                                                                                                                            #f
                                                                                                                                                                                                            (id args
                                                                                                                                                                                                                body))
                                                                                                                                                                                                          #(236
                                                                                                                                                                                                            (r$481)
                                                                                                                                                                                                            ((if r$481
                                                                                                                                                                                                               (#((record-marker)
                                                                                                                                                                                                                  #((record-marker)
                                                                                                                                                                                                                    #f
                                                                                                                                                                                                                    (id args
                                                                                                                                                                                                                        body))
                                                                                                                                                                                                                  #(235
                                                                                                                                                                                                                    (r$482)
                                                                                                                                                                                                                    ((k$480 (equal?
                                                                                                                                                                                                                              r$482
                                                                                                                                                                                                                              #\x)))))
                                                                                                                                                                                                                (car buf$195))
                                                                                                                                                                                                               (k$480 #f)))))
                                                                                                                                                                                                        (> r$483
                                                                                                                                                                                                           1)))))
                                                                                                                                                                                                  (length
                                                                                                                                                                                                    buf$195)))))
                                                                                                                                                                                            #((record-marker)
                                                                                                                                                                                              #((record-marker)
                                                                                                                                                                                                #f
                                                                                                                                                                                                (id args
                                                                                                                                                                                                    body))
                                                                                                                                                                                              #(234
                                                                                                                                                                                                (r$472)
                                                                                                                                                                                                ((if r$472
                                                                                                                                                                                                   (#((record-marker)
                                                                                                                                                                                                      #((record-marker)
                                                                                                                                                                                                        #f
                                                                                                                                                                                                        (id args
                                                                                                                                                                                                            body))
                                                                                                                                                                                                      #(228
                                                                                                                                                                                                        ()
                                                                                                                                                                                                        ((#((record-marker)
                                                                                                                                                                                                            #((record-marker)
                                                                                                                                                                                                              #f
                                                                                                                                                                                                              (id args
                                                                                                                                                                                                                  body))
                                                                                                                                                                                                            #(227
                                                                                                                                                                                                              (r$475)
                                                                                                                                                                                                              ((#((record-marker)
                                                                                                                                                                                                                  #((record-marker)
                                                                                                                                                                                                                    #f
                                                                                                                                                                                                                    (id args
                                                                                                                                                                                                                        body))
                                                                                                                                                                                                                  #(226
                                                                                                                                                                                                                    (r$474)
                                                                                                                                                                                                                    ((#((record-marker)
                                                                                                                                                                                                                        #((record-marker)
                                                                                                                                                                                                                          #f
                                                                                                                                                                                                                          (id args
                                                                                                                                                                                                                              body))
                                                                                                                                                                                                                        #(225
                                                                                                                                                                                                                          (r$473)
                                                                                                                                                                                                                          ((k$457 (integer->char
                                                                                                                                                                                                                                    r$473)))))
                                                                                                                                                                                                                      (string->number
                                                                                                                                                                                                                        r$474
                                                                                                                                                                                                                        16)))))
                                                                                                                                                                                                                (list->string
                                                                                                                                                                                                                  r$475)))))
                                                                                                                                                                                                          (cdr buf$195))))))
                                                                                                                                                                                                   (#((record-marker)
                                                                                                                                                                                                      #((record-marker)
                                                                                                                                                                                                        #f
                                                                                                                                                                                                        (id args
                                                                                                                                                                                                            body))
                                                                                                                                                                                                      #(233
                                                                                                                                                                                                        ()
                                                                                                                                                                                                        ((#((record-marker)
                                                                                                                                                                                                            #((record-marker)
                                                                                                                                                                                                              #f
                                                                                                                                                                                                              (id args
                                                                                                                                                                                                                  body))
                                                                                                                                                                                                            #(232
                                                                                                                                                                                                              (r$479)
                                                                                                                                                                                                              ((#((record-marker)
                                                                                                                                                                                                                  #((record-marker)
                                                                                                                                                                                                                    #f
                                                                                                                                                                                                                    (id args
                                                                                                                                                                                                                        body))
                                                                                                                                                                                                                  #(231
                                                                                                                                                                                                                    (r$476)
                                                                                                                                                                                                                    ((in-port:get-lnum
                                                                                                                                                                                                                       #((record-marker)
                                                                                                                                                                                                                         #((record-marker)
                                                                                                                                                                                                                           #f
                                                                                                                                                                                                                           (id args
                                                                                                                                                                                                                               body))
                                                                                                                                                                                                                         #(230
                                                                                                                                                                                                                           (r$477)
                                                                                                                                                                                                                           ((in-port:get-cnum
                                                                                                                                                                                                                              #((record-marker)
                                                                                                                                                                                                                                #((record-marker)
                                                                                                                                                                                                                                  #f
                                                                                                                                                                                                                                  (id args
                                                                                                                                                                                                                                      body))
                                                                                                                                                                                                                                #(229
                                                                                                                                                                                                                                  (r$478)
                                                                                                                                                                                                                                  ((parse-error
                                                                                                                                                                                                                                     k$457
                                                                                                                                                                                                                                     r$476
                                                                                                                                                                                                                                     r$477
                                                                                                                                                                                                                                     r$478))))
                                                                                                                                                                                                                              ptbl$182))))
                                                                                                                                                                                                                       ptbl$182))))
                                                                                                                                                                                                                (string-append
                                                                                                                                                                                                                  "unable to parse character: "
                                                                                                                                                                                                                  r$479)))))
                                                                                                                                                                                                          (list->string
                                                                                                                                                                                                            buf$195)))))))))))))))
                                                                                                                                                                                    (equal?
                                                                                                                                                                                      buf$195
                                                                                                                                                                                      r$484)))))
                                                                                                                                                                              '(#\t
                                                                                                                                                                                #\a
                                                                                                                                                                                #\b))))))
                                                                                                                                                                      (equal?
                                                                                                                                                                        buf$195
                                                                                                                                                                        r$485)))))
                                                                                                                                                                '(#\s
                                                                                                                                                                  #\p
                                                                                                                                                                  #\a
                                                                                                                                                                  #\c
                                                                                                                                                                  #\e))))))
                                                                                                                                                        (equal?
                                                                                                                                                          buf$195
                                                                                                                                                          r$486)))))
                                                                                                                                                  '(#\r
                                                                                                                                                    #\e
                                                                                                                                                    #\t
                                                                                                                                                    #\u
                                                                                                                                                    #\r
                                                                                                                                                    #\n))))))
                                                                                                                                          (equal?
                                                                                                                                            buf$195
                                                                                                                                            r$487)))))
                                                                                                                                    '(#\n
                                                                                                                                      #\u
                                                                                                                                      #\l
                                                                                                                                      #\l))))))
                                                                                                                            (equal?
                                                                                                                              buf$195
                                                                                                                              r$488)))))
                                                                                                                      '(#\n
                                                                                                                        #\e
                                                                                                                        #\w
                                                                                                                        #\l
                                                                                                                        #\i
                                                                                                                        #\n
                                                                                                                        #\e))))))
                                                                                                              (equal?
                                                                                                                buf$195
                                                                                                                r$489)))))
                                                                                                        '(#\e
                                                                                                          #\s
                                                                                                          #\c
                                                                                                          #\a
                                                                                                          #\p
                                                                                                          #\e))))))
                                                                                                (equal?
                                                                                                  buf$195
                                                                                                  r$490)))))
                                                                                          '(#\d
                                                                                            #\e
                                                                                            #\l
                                                                                            #\e
                                                                                            #\t
                                                                                            #\e))))))
                                                                                  (equal?
                                                                                    buf$195
                                                                                    r$491)))))
                                                                            '(#\b
                                                                              #\a
                                                                              #\c
                                                                              #\k
                                                                              #\s
                                                                              #\p
                                                                              #\a
                                                                              #\c
                                                                              #\e))))))
                                                                    (equal?
                                                                      buf$195
                                                                      r$492)))))
                                                              '(#\a
                                                                #\l
                                                                #\a
                                                                #\r
                                                                #\m))))))
                                                      (= 1 r$493)))))
                                                (length buf$195))))))
                                        (= 0 r$494)))))
                                  (length buf$195)))))
                            raw-buf$194))))))))))))
         #f
         #f)))))
 (define read-str
   #((record-marker)
     #((record-marker) #f (id args body))
     #(188
       (k$426 fp$180 buf$179 ptbl$178)
       ((#((record-marker)
           #((record-marker) #f (id args body))
           #(186
             (c$181)
             ((#((record-marker)
                 #((record-marker) #f (id args body))
                 #(185
                   (r$428)
                   ((if r$428
                      (#((record-marker)
                         #((record-marker) #f (id args body))
                         #(176
                           ()
                           ((in-port:get-lnum
                              #((record-marker)
                                #((record-marker) #f (id args body))
                                #(175
                                  (r$429)
                                  ((in-port:get-cnum
                                     #((record-marker)
                                       #((record-marker) #f (id args body))
                                       #(174
                                         (r$430)
                                         ((parse-error
                                            k$426
                                            "missing closing double-quote"
                                            r$429
                                            r$430))))
                                     ptbl$178))))
                              ptbl$178)))))
                      (#((record-marker)
                         #((record-marker) #f (id args body))
                         #(184
                           (r$431)
                           ((if r$431
                              (#((record-marker)
                                 #((record-marker) #f (id args body))
                                 #(178
                                   ()
                                   ((read-str-esc
                                      #((record-marker)
                                        #((record-marker) #f (id args body))
                                        #(177
                                          (r$432)
                                          ((read-str
                                             k$426
                                             fp$180
                                             r$432
                                             ptbl$178))))
                                      fp$180
                                      buf$179
                                      ptbl$178)))))
                              (#((record-marker)
                                 #((record-marker) #f (id args body))
                                 #(183
                                   (r$433)
                                   ((if r$433
                                      (#((record-marker)
                                         #((record-marker) #f (id args body))
                                         #(180
                                           ()
                                           ((reverse
                                              #((record-marker)
                                                #((record-marker)
                                                  #f
                                                  (id args body))
                                                #(179
                                                  (r$434)
                                                  ((k$426 (list->string
                                                            r$434)))))
                                              buf$179)))))
                                      (#((record-marker)
                                         #((record-marker) #f (id args body))
                                         #(182
                                           ()
                                           ((#((record-marker)
                                               #((record-marker)
                                                 #f
                                                 (id args body))
                                               #(181
                                                 (r$435)
                                                 ((read-str
                                                    k$426
                                                    fp$180
                                                    r$435
                                                    ptbl$178))))
                                             (cons c$181 buf$179))))))))))
                               (equal? #\" c$181))))))
                       (equal? #\\ c$181))))))
               (eof-object? c$181)))))
         (read-char fp$180))))))
 (define read-str-esc
   #((record-marker)
     #((record-marker) #f (id args body))
     #(173
       (k$410 fp$175 buf$174 ptbl$173)
       ((#((record-marker)
           #((record-marker) #f (id args body))
           #(171
             (c$176)
             ((#((record-marker)
                 #((record-marker) #f (id args body))
                 #(170
                   (r$412)
                   ((if r$412
                      (#((record-marker)
                         #((record-marker) #f (id args body))
                         #(156
                           ()
                           ((in-port:get-lnum
                              #((record-marker)
                                #((record-marker) #f (id args body))
                                #(155
                                  (r$413)
                                  ((in-port:get-cnum
                                     #((record-marker)
                                       #((record-marker) #f (id args body))
                                       #(154
                                         (r$414)
                                         ((parse-error
                                            k$410
                                            "missing escaped character within string"
                                            r$413
                                            r$414))))
                                     ptbl$173))))
                              ptbl$173)))))
                      (#((record-marker)
                         #((record-marker) #f (id args body))
                         #(168
                           (tmp$177)
                           ((#((record-marker)
                               #((record-marker) #f (id args body))
                               #(167
                                 (k$423)
                                 ((if tmp$177
                                    (k$423 tmp$177)
                                    (k$423 (equal? #\\ c$176))))))
                             #((record-marker)
                               #((record-marker) #f (id args body))
                               #(166
                                 (r$415)
                                 ((if r$415
                                    (#((record-marker)
                                       #((record-marker) #f (id args body))
                                       #(157
                                         ()
                                         ((k$410 (cons c$176 buf$174))))))
                                    (#((record-marker)
                                       #((record-marker) #f (id args body))
                                       #(165
                                         (r$416)
                                         ((if r$416
                                            (#((record-marker)
                                               #((record-marker)
                                                 #f
                                                 (id args body))
                                               #(158
                                                 ()
                                                 ((k$410 (cons #\newline
                                                               buf$174))))))
                                            (#((record-marker)
                                               #((record-marker)
                                                 #f
                                                 (id args body))
                                               #(164
                                                 ()
                                                 ((list #((record-marker)
                                                          #((record-marker)
                                                            #f
                                                            (id args body))
                                                          #(163
                                                            (r$421)
                                                            ((#((record-marker)
                                                                #((record-marker)
                                                                  #f
                                                                  (id args
                                                                      body))
                                                                #(162
                                                                  (r$420)
                                                                  ((#((record-marker)
                                                                      #((record-marker)
                                                                        #f
                                                                        (id args
                                                                            body))
                                                                      #(161
                                                                        (r$417)
                                                                        ((in-port:get-lnum
                                                                           #((record-marker)
                                                                             #((record-marker)
                                                                               #f
                                                                               (id args
                                                                                   body))
                                                                             #(160
                                                                               (r$418)
                                                                               ((in-port:get-cnum
                                                                                  #((record-marker)
                                                                                    #((record-marker)
                                                                                      #f
                                                                                      (id args
                                                                                          body))
                                                                                    #(159
                                                                                      (r$419)
                                                                                      ((parse-error
                                                                                         k$410
                                                                                         r$417
                                                                                         r$418
                                                                                         r$419))))
                                                                                  ptbl$173))))
                                                                           ptbl$173))))
                                                                    (string-append
                                                                      "invalid escape character ["
                                                                      r$420
                                                                      "] in string")))))
                                                              (list->string
                                                                r$421)))))
                                                        c$176)))))))))
                                     (equal? #\n c$176))))))))))
                       (equal? #\" c$176))))))
               (eof-object? c$176)))))
         (read-char fp$175))))))
 (define sign?
   #((record-marker)
     #((record-marker) #f (id args body))
     #(153
       (k$406 c$171)
       ((#((record-marker)
           #((record-marker) #f (id args body))
           #(151
             (tmp$172)
             ((if tmp$172
                (k$406 tmp$172)
                (k$406 (equal? c$171 #\-))))))
         (equal? c$171 #\+))))))
 (define token-numeric?
   #((record-marker)
     #((record-marker) #f (id args body))
     #(150
       (k$386 a$167)
       ((#((record-marker)
           #((record-marker) #f (id args body))
           #(149
             (r$403)
             ((char-numeric?
                #((record-marker)
                  #((record-marker) #f (id args body))
                  #(147
                    (tmp$168)
                    ((if tmp$168
                       (k$386 tmp$168)
                       (#((record-marker)
                          #((record-marker) #f (id args body))
                          #(146
                            (k$397)
                            ((#((record-marker)
                                #((record-marker) #f (id args body))
                                #(145
                                  (r$402)
                                  ((#((record-marker)
                                      #((record-marker) #f (id args body))
                                      #(144
                                        (r$398)
                                        ((if r$398
                                           (#((record-marker)
                                              #((record-marker)
                                                #f
                                                (id args body))
                                              #(143
                                                (r$401)
                                                ((#((record-marker)
                                                    #((record-marker)
                                                      #f
                                                      (id args body))
                                                    #(142
                                                      (r$399)
                                                      ((if r$399
                                                         (#((record-marker)
                                                            #((record-marker)
                                                              #f
                                                              (id args body))
                                                            #(141
                                                              (r$400)
                                                              ((char-numeric?
                                                                 k$397
                                                                 r$400))))
                                                          (cadr a$167))
                                                         (k$397 #f)))))
                                                  (eq? #\. r$401)))))
                                            (car a$167))
                                           (k$397 #f)))))
                                    (> r$402 1)))))
                              (length a$167)))))
                        #((record-marker)
                          #((record-marker) #f (id args body))
                          #(139
                            (tmp$169)
                            ((if tmp$169
                               (k$386 tmp$169)
                               (#((record-marker)
                                  #((record-marker) #f (id args body))
                                  #(138
                                    (r$396)
                                    ((#((record-marker)
                                        #((record-marker) #f (id args body))
                                        #(137
                                          (r$389)
                                          ((if r$389
                                             (#((record-marker)
                                                #((record-marker)
                                                  #f
                                                  (id args body))
                                                #(136
                                                  (r$395)
                                                  ((char-numeric?
                                                     #((record-marker)
                                                       #((record-marker)
                                                         #f
                                                         (id args body))
                                                       #(134
                                                         (tmp$170)
                                                         ((#((record-marker)
                                                             #((record-marker)
                                                               #f
                                                               (id args body))
                                                             #(133
                                                               (k$393)
                                                               ((if tmp$170
                                                                  (k$393 tmp$170)
                                                                  (#((record-marker)
                                                                     #((record-marker)
                                                                       #f
                                                                       (id args
                                                                           body))
                                                                     #(132
                                                                       (r$394)
                                                                       ((k$393 (eq? #\.
                                                                                    r$394)))))
                                                                   (cadr a$167))))))
                                                           #((record-marker)
                                                             #((record-marker)
                                                               #f
                                                               (id args body))
                                                             #(131
                                                               (r$390)
                                                               ((if r$390
                                                                  (#((record-marker)
                                                                     #((record-marker)
                                                                       #f
                                                                       (id args
                                                                           body))
                                                                     #(130
                                                                       (r$391)
                                                                       ((sign? k$386
                                                                               r$391))))
                                                                   (car a$167))
                                                                  (k$386 #f)))))))))
                                                     r$395))))
                                              (cadr a$167))
                                             (k$386 #f)))))
                                      (> r$396 1)))))
                                (length a$167)))))))))))
                r$403))))
         (car a$167))))))
 (define parse-atom
   #((record-marker)
     #((record-marker) #f (id args body))
     #(129
       (k$380 a$166)
       ((token-numeric?
          #((record-marker)
            #((record-marker) #f (id args body))
            #(128
              (r$381)
              ((if r$381
                 (#((record-marker)
                    #((record-marker) #f (id args body))
                    #(125
                      ()
                      ((#((record-marker)
                          #((record-marker) #f (id args body))
                          #(124 (r$382) ((k$380 (string->number r$382)))))
                        (list->string a$166))))))
                 (#((record-marker)
                    #((record-marker) #f (id args body))
                    #(127
                      ()
                      ((#((record-marker)
                          #((record-marker) #f (id args body))
                          #(126 (r$383) ((k$380 (string->symbol r$383)))))
                        (list->string a$166))))))))))
          a$166)))))
 (define get-next-char
   #((record-marker)
     #((record-marker) #f (id args body))
     #(123
       (k$376 fp$165 ptbl$164)
       ((in-port:get-buf
          #((record-marker)
            #((record-marker) #f (id args body))
            #(122
              (r$377)
              ((if r$377
                 (in-port:read-buf! k$376 ptbl$164)
                 (k$376 (read-char fp$165))))))
          ptbl$164)))))
 (define read-block-comment
   #((record-marker)
     #((record-marker) #f (id args body))
     #(121
       (k$371 fp$162 ptbl$161)
       ((get-next-char
          #((record-marker)
            #((record-marker) #f (id args body))
            #(119
              (c$163)
              ((#((record-marker)
                  #((record-marker) #f (id args body))
                  #(118
                    (r$373)
                    ((if r$373
                       (#((record-marker)
                          #((record-marker) #f (id args body))
                          #(116
                            ()
                            ((read-block-terminator k$371 fp$162 ptbl$161)))))
                       (#((record-marker)
                          #((record-marker) #f (id args body))
                          #(117
                            ()
                            ((read-block-comment k$371 fp$162 ptbl$161)))))))))
                (eq? #\| c$163)))))
          fp$162
          ptbl$161)))))
 (define read-block-terminator
   #((record-marker)
     #((record-marker) #f (id args body))
     #(115
       (k$365 fp$159 ptbl$158)
       ((get-next-char
          #((record-marker)
            #((record-marker) #f (id args body))
            #(113
              (c$160)
              ((#((record-marker)
                  #((record-marker) #f (id args body))
                  #(112
                    (r$367)
                    ((if r$367
                       (#((record-marker)
                          #((record-marker) #f (id args body))
                          #(108 () ((k$365 #t)))))
                       (#((record-marker)
                          #((record-marker) #f (id args body))
                          #(111
                            (r$368)
                            ((if r$368
                               (#((record-marker)
                                  #((record-marker) #f (id args body))
                                  #(109
                                    ()
                                    ((read-block-terminator
                                       k$365
                                       fp$159
                                       ptbl$158)))))
                               (#((record-marker)
                                  #((record-marker) #f (id args body))
                                  #(110
                                    ()
                                    ((read-block-comment
                                       k$365
                                       fp$159
                                       ptbl$158)))))))))
                        (eq? #\| c$160))))))
                (eq? #\# c$160)))))
          fp$159
          ptbl$158)))))
 (define parse-literal-identifier
   #((record-marker)
     #((record-marker) #f (id args body))
     #(107
       (k$358 fp$156
              toks$155
              all?$154
              parens$153
              ptbl$152)
       ((#((record-marker)
           #((record-marker) #f (id args body))
           #(106
             (r$362)
             ((parse-li-rec
                #((record-marker)
                  #((record-marker) #f (id args body))
                  #(104
                    (sym$157)
                    ((if all?$154
                       (#((record-marker)
                          #((record-marker) #f (id args body))
                          #(103
                            (r$360)
                            ((#((record-marker)
                                #((record-marker) #f (id args body))
                                #(102
                                  (r$361)
                                  ((parse k$358
                                          fp$156
                                          r$360
                                          r$361
                                          all?$154
                                          #f
                                          parens$153
                                          ptbl$152))))
                              (cons sym$157 toks$155)))))
                        '())
                       (k$358 sym$157)))))
                fp$156
                r$362
                ptbl$152))))
         '())))))
 (define parse-li-rec
   #((record-marker)
     #((record-marker) #f (id args body))
     #(101
       (k$343 fp$147 tok$146 ptbl$145)
       ((get-next-char
          #((record-marker)
            #((record-marker) #f (id args body))
            #(100
              (r$344)
              ((#((record-marker)
                  #((record-marker) #f (id args body))
                  #(97
                    (r$345)
                    ((#((record-marker)
                        #((record-marker) #f (id args body))
                        #(96
                          (c$150 next$149)
                          ((#((record-marker)
                              #((record-marker) #f (id args body))
                              #(95
                                (r$346)
                                ((if r$346
                                   (#((record-marker)
                                      #((record-marker) #f (id args body))
                                      #(89
                                        ()
                                        ((#((record-marker)
                                            #((record-marker) #f (id args body))
                                            #(88
                                              (k$348)
                                              ((#((record-marker)
                                                  #((record-marker)
                                                    #f
                                                    (id args body))
                                                  #(87
                                                    (r$349)
                                                    ((if r$349
                                                       (k$348 "||")
                                                       (reverse
                                                         #((record-marker)
                                                           #((record-marker)
                                                             #f
                                                             (id args body))
                                                           #(86
                                                             (r$350)
                                                             ((k$348 (list->string
                                                                       r$350)))))
                                                         tok$146)))))
                                                (null? tok$146)))))
                                          #((record-marker)
                                            #((record-marker) #f (id args body))
                                            #(84
                                              (str$151)
                                              ((k$343 (string->symbol
                                                        str$151))))))))))
                                   (#((record-marker)
                                      #((record-marker) #f (id args body))
                                      #(94
                                        (r$351)
                                        ((if r$351
                                           (#((record-marker)
                                              #((record-marker)
                                                #f
                                                (id args body))
                                              #(92
                                                ()
                                                ((in-port:get-lnum
                                                   #((record-marker)
                                                     #((record-marker)
                                                       #f
                                                       (id args body))
                                                     #(91
                                                       (r$352)
                                                       ((in-port:get-cnum
                                                          #((record-marker)
                                                            #((record-marker)
                                                              #f
                                                              (id args body))
                                                            #(90
                                                              (r$353)
                                                              ((parse-error
                                                                 k$343
                                                                 "EOF encountered parsing literal identifier"
                                                                 r$352
                                                                 r$353))))
                                                          ptbl$145))))
                                                   ptbl$145)))))
                                           (#((record-marker)
                                              #((record-marker)
                                                #f
                                                (id args body))
                                              #(93
                                                ()
                                                ((next$149 k$343 c$150)))))))))
                                    (eof-object? c$150))))))
                            (eq? #\| c$150)))))
                      r$344
                      r$345))))
                #((record-marker)
                  #((record-marker) #f (id args body))
                  #(99
                    (k$354 c$148)
                    ((#((record-marker)
                        #((record-marker) #f (id args body))
                        #(98
                          (r$355)
                          ((parse-li-rec k$354 fp$147 r$355 ptbl$145))))
                      (cons c$148 tok$146)))))))))
          fp$147
          ptbl$145)))))
 (define parse-number
   #((record-marker)
     #((record-marker) #f (id args body))
     #(83
       (k$325 fp$141
              toks$140
              all?$139
              parens$138
              ptbl$137
              base$136
              tok->num$135)
       ((#((record-marker)
           #((record-marker) #f (id args body))
           #(82
             (r$340)
             ((parse-number-rec
                #((record-marker)
                  #((record-marker) #f (id args body))
                  #(80
                    (num$142)
                    ((#((record-marker)
                        #((record-marker) #f (id args body))
                        #(79
                          (k$333)
                          ((#((record-marker)
                              #((record-marker) #f (id args body))
                              #(78
                                (r$334)
                                ((if r$334
                                   (k$333 #f)
                                   (token-numeric?
                                     #((record-marker)
                                       #((record-marker) #f (id args body))
                                       #(76
                                         (tmp$144)
                                         ((if tmp$144
                                            (k$333 tmp$144)
                                            (#((record-marker)
                                               #((record-marker)
                                                 #f
                                                 (id args body))
                                               #(75
                                                 (r$339)
                                                 ((#((record-marker)
                                                     #((record-marker)
                                                       #f
                                                       (id args body))
                                                     #(74
                                                       (r$336)
                                                       ((if r$336
                                                          (#((record-marker)
                                                             #((record-marker)
                                                               #f
                                                               (id args body))
                                                             #(73
                                                               (r$337)
                                                               ((if r$337
                                                                  (#((record-marker)
                                                                     #((record-marker)
                                                                       #f
                                                                       (id args
                                                                           body))
                                                                     #(72
                                                                       (r$338)
                                                                       ((hex-digit?
                                                                          k$333
                                                                          r$338))))
                                                                   (car num$142))
                                                                  (k$333 #f)))))
                                                           (= base$136 16))
                                                          (k$333 #f)))))
                                                   (> r$339 0)))))
                                             (length num$142))))))
                                     num$142)))))
                            (null? num$142)))))
                      #((record-marker)
                        #((record-marker) #f (id args body))
                        #(71
                          (r$327)
                          ((if r$327
                             (#((record-marker)
                                #((record-marker) #f (id args body))
                                #(67
                                  ()
                                  ((tok->num$135
                                     #((record-marker)
                                       #((record-marker) #f (id args body))
                                       #(65
                                         (result$143)
                                         ((if all?$139
                                            (#((record-marker)
                                               #((record-marker)
                                                 #f
                                                 (id args body))
                                               #(64
                                                 (r$329)
                                                 ((#((record-marker)
                                                     #((record-marker)
                                                       #f
                                                       (id args body))
                                                     #(63
                                                       (r$330)
                                                       ((parse k$325
                                                               fp$141
                                                               r$329
                                                               r$330
                                                               all?$139
                                                               #f
                                                               parens$138
                                                               ptbl$137))))
                                                   (cons result$143
                                                         toks$140)))))
                                             '())
                                            (k$325 result$143)))))
                                     num$142)))))
                             (#((record-marker)
                                #((record-marker) #f (id args body))
                                #(70
                                  ()
                                  ((in-port:get-lnum
                                     #((record-marker)
                                       #((record-marker) #f (id args body))
                                       #(69
                                         (r$331)
                                         ((in-port:get-cnum
                                            #((record-marker)
                                              #((record-marker)
                                                #f
                                                (id args body))
                                              #(68
                                                (r$332)
                                                ((parse-error
                                                   k$325
                                                   "Illegal number syntax"
                                                   r$331
                                                   r$332))))
                                            ptbl$137))))
                                     ptbl$137)))))))))))))
                base$136
                fp$141
                r$340
                ptbl$137))))
         '())))))
 (define parse-number-rec
   #((record-marker)
     #((record-marker) #f (id args body))
     #(62
       (k$301 base$130 fp$129 tok$128 ptbl$127)
       ((get-next-char
          #((record-marker)
            #((record-marker) #f (id args body))
            #(61
              (r$302)
              ((#((record-marker)
                  #((record-marker) #f (id args body))
                  #(58
                    (r$303)
                    ((#((record-marker)
                        #((record-marker) #f (id args body))
                        #(57
                          (c$133 next$132)
                          ((sign? #((record-marker)
                                    #((record-marker) #f (id args body))
                                    #(56
                                      (r$304)
                                      ((if r$304
                                         (#((record-marker)
                                            #((record-marker) #f (id args body))
                                            #(34 () ((next$132 k$301 c$133)))))
                                         (#((record-marker)
                                            #((record-marker) #f (id args body))
                                            #(55
                                              (r$305)
                                              ((if r$305
                                                 (#((record-marker)
                                                    #((record-marker)
                                                      #f
                                                      (id args body))
                                                    #(35
                                                      ()
                                                      ((next$132
                                                         k$301
                                                         c$133)))))
                                                 (char-numeric?
                                                   #((record-marker)
                                                     #((record-marker)
                                                       #f
                                                       (id args body))
                                                     #(54
                                                       (r$306)
                                                       ((if r$306
                                                          (#((record-marker)
                                                             #((record-marker)
                                                               #f
                                                               (id args body))
                                                             #(47
                                                               ()
                                                               ((#((record-marker)
                                                                   #((record-marker)
                                                                     #f
                                                                     (id args
                                                                         body))
                                                                   #(46
                                                                     (k$308)
                                                                     ((#((record-marker)
                                                                         #((record-marker)
                                                                           #f
                                                                           (id args
                                                                               body))
                                                                         #(45
                                                                           (k$315)
                                                                           ((#((record-marker)
                                                                               #((record-marker)
                                                                                 #f
                                                                                 (id args
                                                                                     body))
                                                                               #(44
                                                                                 (r$316)
                                                                                 ((if r$316
                                                                                    (char>?
                                                                                      k$315
                                                                                      c$133
                                                                                      #\1)
                                                                                    (k$315 #f)))))
                                                                             (= base$130
                                                                                2)))))
                                                                       #((record-marker)
                                                                         #((record-marker)
                                                                           #f
                                                                           (id args
                                                                               body))
                                                                         #(42
                                                                           (tmp$134)
                                                                           ((#((record-marker)
                                                                               #((record-marker)
                                                                                 #f
                                                                                 (id args
                                                                                     body))
                                                                               #(41
                                                                                 (k$313)
                                                                                 ((if tmp$134
                                                                                    (k$313 tmp$134)
                                                                                    (#((record-marker)
                                                                                       #((record-marker)
                                                                                         #f
                                                                                         (id args
                                                                                             body))
                                                                                       #(40
                                                                                         (r$314)
                                                                                         ((if r$314
                                                                                            (char>?
                                                                                              k$313
                                                                                              c$133
                                                                                              #\7)
                                                                                            (k$313 #f)))))
                                                                                     (= base$130
                                                                                        8))))))
                                                                             #((record-marker)
                                                                               #((record-marker)
                                                                                 #f
                                                                                 (id args
                                                                                     body))
                                                                               #(39
                                                                                 (r$309)
                                                                                 ((if r$309
                                                                                    (in-port:get-lnum
                                                                                      #((record-marker)
                                                                                        #((record-marker)
                                                                                          #f
                                                                                          (id args
                                                                                              body))
                                                                                        #(38
                                                                                          (r$310)
                                                                                          ((in-port:get-cnum
                                                                                             #((record-marker)
                                                                                               #((record-marker)
                                                                                                 #f
                                                                                                 (id args
                                                                                                     body))
                                                                                               #(37
                                                                                                 (r$311)
                                                                                                 ((parse-error
                                                                                                    k$308
                                                                                                    "Illegal digit"
                                                                                                    r$310
                                                                                                    r$311))))
                                                                                             ptbl$127))))
                                                                                      ptbl$127)
                                                                                    (k$308 #f)))))))))))))
                                                                 #((record-marker)
                                                                   #((record-marker)
                                                                     #f
                                                                     (id args
                                                                         body))
                                                                   #(36
                                                                     (r$307)
                                                                     ((next$132
                                                                        k$301
                                                                        c$133)))))))))
                                                          (#((record-marker)
                                                             #((record-marker)
                                                               #f
                                                               (id args body))
                                                             #(53
                                                               (k$319)
                                                               ((#((record-marker)
                                                                   #((record-marker)
                                                                     #f
                                                                     (id args
                                                                         body))
                                                                   #(52
                                                                     (r$320)
                                                                     ((if r$320
                                                                        (hex-digit?
                                                                          k$319
                                                                          c$133)
                                                                        (k$319 #f)))))
                                                                 (= base$130
                                                                    16)))))
                                                           #((record-marker)
                                                             #((record-marker)
                                                               #f
                                                               (id args body))
                                                             #(51
                                                               (r$317)
                                                               ((if r$317
                                                                  (#((record-marker)
                                                                     #((record-marker)
                                                                       #f
                                                                       (id args
                                                                           body))
                                                                     #(48
                                                                       ()
                                                                       ((next$132
                                                                          k$301
                                                                          c$133)))))
                                                                  (#((record-marker)
                                                                     #((record-marker)
                                                                       #f
                                                                       (id args
                                                                           body))
                                                                     #(50
                                                                       ()
                                                                       ((in-port:set-buf!
                                                                          #((record-marker)
                                                                            #((record-marker)
                                                                              #f
                                                                              (id args
                                                                                  body))
                                                                            #(49
                                                                              (r$318)
                                                                              ((reverse
                                                                                 k$301
                                                                                 tok$128))))
                                                                          ptbl$127
                                                                          c$133))))))))))))))
                                                   c$133)))))
                                          (eq? #\. c$133))))))
                                  c$133))))
                      r$302
                      r$303))))
                #((record-marker)
                  #((record-marker) #f (id args body))
                  #(60
                    (k$321 c$131)
                    ((#((record-marker)
                        #((record-marker) #f (id args body))
                        #(59
                          (r$322)
                          ((parse-number-rec
                             k$321
                             base$130
                             fp$129
                             r$322
                             ptbl$127))))
                      (cons c$131 tok$128)))))))))
          fp$129
          ptbl$127)))))
 (define hex-digit?
   #((record-marker)
     #((record-marker) #f (id args body))
     #(33
       (k$294 c$125)
       ((#((record-marker)
           #((record-marker) #f (id args body))
           #(32
             (k$297)
             ((char>=?
                #((record-marker)
                  #((record-marker) #f (id args body))
                  #(31
                    (r$298)
                    ((if r$298 (char<=? k$297 c$125 #\f) (k$297 #f)))))
                c$125
                #\a))))
         #((record-marker)
           #((record-marker) #f (id args body))
           #(29
             (tmp$126)
             ((if tmp$126
                (k$294 tmp$126)
                (char>=?
                  #((record-marker)
                    #((record-marker) #f (id args body))
                    #(28
                      (r$296)
                      ((if r$296 (char<=? k$294 c$125 #\F) (k$294 #f)))))
                  c$125
                  #\A))))))))))
 (define cyc-read
   #((record-marker)
     #((record-marker) #f (id args body))
     #(27
       (k$285 . args$123)
       ((#((record-marker)
           #((record-marker) #f (id args body))
           #(26
             (k$290)
             ((#((record-marker)
                 #((record-marker) #f (id args body))
                 #(25
                   (r$291)
                   ((if r$291
                      (current-input-port k$290)
                      (k$290 (car args$123))))))
               (null? args$123)))))
         #((record-marker)
           #((record-marker) #f (id args body))
           #(23
             (fp$124)
             ((#((record-marker)
                 #((record-marker) #f (id args body))
                 #(22
                   (r$287)
                   ((#((record-marker)
                       #((record-marker) #f (id args body))
                       #(21
                         (r$288)
                         ((reg-port
                            #((record-marker)
                              #((record-marker) #f (id args body))
                              #(20
                                (r$289)
                                ((parse k$285
                                        fp$124
                                        r$287
                                        r$288
                                        #f
                                        #f
                                        0
                                        r$289))))
                            fp$124))))
                     '()))))
               '())))))))))
 (define read-all
   #((record-marker)
     #((record-marker) #f (id args body))
     #(19
       (k$272 . args$116)
       ((#((record-marker)
           #((record-marker) #f (id args body))
           #(18
             ()
             ((#((record-marker)
                 #((record-marker) #f (id args body))
                 #(17
                   (k$281)
                   ((#((record-marker)
                       #((record-marker) #f (id args body))
                       #(16
                         (r$282)
                         ((if r$282
                            (current-input-port k$281)
                            (k$281 (car args$116))))))
                     (null? args$116)))))
               #((record-marker)
                 #((record-marker) #f (id args body))
                 #(14
                   (fp$118)
                   ((#((record-marker)
                       #((record-marker) #f (id args body))
                       #(13
                         (loop$119)
                         ((#((record-marker)
                             #((record-marker) #f (id args body))
                             #(7
                               (r$276)
                               ((#((record-marker)
                                   #((record-marker) #f (id args body))
                                   #(6
                                     (r$274)
                                     ((#((record-marker)
                                         #((record-marker) #f (id args body))
                                         #(5
                                           (r$275)
                                           ((loop$119 k$272 fp$118 r$275))))
                                       '()))))
                                 (set! loop$119 r$276)))))
                           #((record-marker)
                             #((record-marker) #f (id args body))
                             #(12
                               (k$277 fp$121 result$120)
                               ((cyc-read
                                  #((record-marker)
                                    #((record-marker) #f (id args body))
                                    #(10
                                      (obj$122)
                                      ((#((record-marker)
                                          #((record-marker) #f (id args body))
                                          #(9
                                            (r$279)
                                            ((if r$279
                                               (reverse k$277 result$120)
                                               (#((record-marker)
                                                  #((record-marker)
                                                    #f
                                                    (id args body))
                                                  #(8
                                                    (r$280)
                                                    ((loop$119
                                                       k$277
                                                       fp$121
                                                       r$280))))
                                                (cons obj$122 result$120))))))
                                        (eof-object? obj$122)))))
                                  fp$121))))))))
                     #f)))))))))))))
 (define lib-init:schemeread
   #((record-marker)
     #((record-marker) #f (id args body))
     #(4
       (k$266)
       ((#((record-marker)
           #((record-marker) #f (id args body))
           #(3
             ()
             ((#((record-marker)
                 #((record-marker) #f (id args body))
                 #(2
                   (r$268)
                   ((#((record-marker)
                       #((record-marker) #f (id args body))
                       #(1
                         (r$269)
                         ((k$266 (set! *in-port-table* r$269)))))
                     '()))))
               (set! read cyc-read)))))))))))

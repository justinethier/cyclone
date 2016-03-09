
CHICKEN
(c)2008-2011 The Chicken Team
(c)2000-2007 Felix L. Winkelmann
Version 4.7.0 
linux-unix-gnu-x86 [ manyargs dload ptables ]
compiled 2011-10-17 on roseapple (Linux)

; loading tmp2.scm ...
((define c-compile-and-emit
   (lambda (k$178 input-program$73 lib-deps$72 src-file$71)
     ((lambda (globals$80
               imported-vars$79
               imports$78
               lib-exports$77
               lib-name$76
               module-globals$75
               program?$74)
        ((lambda (r$179) (call/cc k$178 r$179))
         (lambda (k$180 return$81)
           ((lambda (globals$88
                     module-globals$87
                     program?$86
                     imports$85
                     imported-vars$84
                     lib-name$83
                     lib-exports$82)
              ((lambda (r$329)
                 ((lambda (r$181)
                    ((lambda (r$328)
                       ((lambda (r$182)
                          ((lambda (r$183)
                             ((lambda (r$327)
                                ((lambda (r$184)
                                   ((lambda (r$326)
                                      ((lambda (r$185)
                                         ((lambda (r$325)
                                            ((lambda (r$186)
                                               ((lambda (r$324)
                                                  ((lambda (r$187)
                                                     (emit (lambda (r$188)
                                                             (trace:info
                                                               (lambda (r$189)
                                                                 (trace:info
                                                                   (lambda (r$190)
                                                                     ((lambda (k$290)
                                                                        ((lambda (r$323)
                                                                           (library?
                                                                             (lambda (r$291)
                                                                               (if r$291
                                                                                 ((lambda ()
                                                                                    ((lambda (r$316)
                                                                                       (lib:includes
                                                                                         (lambda (r$292)
                                                                                           ((lambda (includes$97)
                                                                                              ((lambda (r$293)
                                                                                                 ((lambda (r$315)
                                                                                                    (lib:name
                                                                                                      (lambda (r$314)
                                                                                                        ((lambda (r$294)
                                                                                                           (lib:name->symbol
                                                                                                             (lambda (r$311)
                                                                                                               ((lambda (r$313)
                                                                                                                  (lib:exports
                                                                                                                    (lambda (r$312)
                                                                                                                      ((lambda (r$310)
                                                                                                                         ((lambda (r$295)
                                                                                                                            ((lambda (r$309)
                                                                                                                               (lib:imports
                                                                                                                                 (lambda (r$308)
                                                                                                                                   ((lambda (r$296)
                                                                                                                                      ((lambda (r$307)
                                                                                                                                         (lib:body
                                                                                                                                           (lambda (r$306)
                                                                                                                                             ((lambda (r$297)
                                                                                                                                                ((lambda (r$305)
                                                                                                                                                   (not (lambda (r$298)
                                                                                                                                                          (if r$298
                                                                                                                                                            ((lambda (r$299)
                                                                                                                                                               (for-each k$290 r$299 includes$97))
                                                                                                                                                             (lambda (k$300 include$98)
                                                                                                                                                               (lib:import->path
                                                                                                                                                                 (lambda (r$304)
                                                                                                                                                                   ((lambda (r$303)
                                                                                                                                                                      (read-file
                                                                                                                                                                        (lambda (r$302)
                                                                                                                                                                          (append
                                                                                                                                                                            (lambda (r$301)
                                                                                                                                                                              (k$300 (set! input-program$73 r$301)))
                                                                                                                                                                            r$302
                                                                                                                                                                            input-program$73))
                                                                                                                                                                        r$303))
                                                                                                                                                                    (string-append r$304 include$98)))
                                                                                                                                                                 lib-name$83)))
                                                                                                                                                            (k$290 #f)))
                                                                                                                                                        r$305))
                                                                                                                                                 (null? includes$97)))
                                                                                                                                              (set! input-program$73 r$306)))
                                                                                                                                           r$307))
                                                                                                                                       (car input-program$73)))
                                                                                                                                    (set! imports$85 r$308)))
                                                                                                                                 r$309))
                                                                                                                             (car input-program$73)))
                                                                                                                          (set! lib-exports$82 r$310)))
                                                                                                                       (cons r$311 r$312)))
                                                                                                                    r$313))
                                                                                                                (car input-program$73)))
                                                                                                             lib-name$83))
                                                                                                         (set! lib-name$83 r$314)))
                                                                                                      r$315))
                                                                                                  (car input-program$73)))
                                                                                               (set! program?$86 #f)))
                                                                                            r$292))
                                                                                         r$316))
                                                                                     (car input-program$73))))
                                                                                 ((lambda (r$321)
                                                                                    ((lambda (r$322)
                                                                                       (tagged-list?
                                                                                         (lambda (r$317)
                                                                                           (if r$317
                                                                                             ((lambda ()
                                                                                                ((lambda (r$320)
                                                                                                   ((lambda (r$318)
                                                                                                      ((lambda (r$319)
                                                                                                         (k$290 (set! input-program$73 r$319)))
                                                                                                       (cdr input-program$73)))
                                                                                                    (set! imports$85 r$320)))
                                                                                                 (cdar input-program$73))))
                                                                                             (k$290 #f)))
                                                                                         r$321
                                                                                         r$322))
                                                                                     (car input-program$73)))
                                                                                  'import)))
                                                                             r$323))
                                                                         (car input-program$73)))
                                                                      (lambda (r$191)
                                                                        (trace:info
                                                                          (lambda (r$192)
                                                                            (trace:info
                                                                              (lambda (r$193)
                                                                                (lib:resolve-imports
                                                                                  (lambda (r$289)
                                                                                    ((lambda (r$194)
                                                                                       (trace:info
                                                                                         (lambda (r$195)
                                                                                           (trace:info
                                                                                             (lambda (r$196)
                                                                                               (lib:resolve-meta
                                                                                                 (lambda (r$285)
                                                                                                   ((lambda (meta$96)
                                                                                                      (append
                                                                                                        (lambda (r$288)
                                                                                                          ((lambda (r$286)
                                                                                                             (trace:info
                                                                                                               (lambda (r$287)
                                                                                                                 (trace:info
                                                                                                                   (lambda (r$197)
                                                                                                                     ((lambda (r$280)
                                                                                                                        ((lambda (r$281)
                                                                                                                           (filter
                                                                                                                             (lambda (r$278)
                                                                                                                               ((lambda (macros$95)
                                                                                                                                  (append
                                                                                                                                    (lambda (r$279)
                                                                                                                                      ((lambda (r$198)
                                                                                                                                         ((lambda (r$276)
                                                                                                                                            ((lambda (r$277)
                                                                                                                                               (create-environment
                                                                                                                                                 (lambda (r$275)
                                                                                                                                                   (macro:load-env!
                                                                                                                                                     (lambda (r$199)
                                                                                                                                                       ((lambda (k$274)
                                                                                                                                                          (if program?$86
                                                                                                                                                            (k$274 expand-lambda-body)
                                                                                                                                                            (k$274 expand)))
                                                                                                                                                        (lambda (r$272)
                                                                                                                                                          (macro:get-env
                                                                                                                                                            (lambda (r$273)
                                                                                                                                                              (r$272 (lambda (r$271)
                                                                                                                                                                       ((lambda (r$200)
                                                                                                                                                                          (trace:info
                                                                                                                                                                            (lambda (r$201)
                                                                                                                                                                              (trace:info
                                                                                                                                                                                (lambda (r$202)
                                                                                                                                                                                  (isolate-globals
                                                                                                                                                                                    (lambda (r$270)
                                                                                                                                                                                      ((lambda (r$203)
                                                                                                                                                                                         ((lambda (k$265)
                                                                                                                                                                                            ((lambda (r$269)
                                                                                                                                                                                               (has-global?
                                                                                                                                                                                                 (lambda (r$268)
                                                                                                                                                                                                   (not (lambda (r$266)
                                                                                                                                                                                                          (if r$266
                                                                                                                                                                                                            (filter-unused-variables
                                                                                                                                                                                                              (lambda (r$267)
                                                                                                                                                                                                                (k$265 (set! input-program$73 r$267)))
                                                                                                                                                                                                              input-program$73
                                                                                                                                                                                                              lib-exports$82)
                                                                                                                                                                                                            (k$265 #f)))
                                                                                                                                                                                                        r$268))
                                                                                                                                                                                                 input-program$73
                                                                                                                                                                                                 r$269))
                                                                                                                                                                                             'eval))
                                                                                                                                                                                          (lambda (r$204)
                                                                                                                                                                                            (trace:info
                                                                                                                                                                                              (lambda (r$205)
                                                                                                                                                                                                (trace:info
                                                                                                                                                                                                  (lambda (r$206)
                                                                                                                                                                                                    (global-vars
                                                                                                                                                                                                      (lambda (r$264)
                                                                                                                                                                                                        ((lambda (r$207)
                                                                                                                                                                                                           (append
                                                                                                                                                                                                             (lambda (r$263)
                                                                                                                                                                                                               ((lambda (r$208)
                                                                                                                                                                                                                  ((lambda (r$261)
                                                                                                                                                                                                                     (map (lambda (r$260)
                                                                                                                                                                                                                            ((lambda (r$209)
                                                                                                                                                                                                                               (trace:info
                                                                                                                                                                                                                                 (lambda (r$210)
                                                                                                                                                                                                                                   (trace:info
                                                                                                                                                                                                                                     (lambda (r$211)
                                                                                                                                                                                                                                       ((lambda (r$258)
                                                                                                                                                                                                                                          (map (lambda (r$245)
                                                                                                                                                                                                                                                 ((lambda (cps$92)
                                                                                                                                                                                                                                                    ((lambda (k$246)
                                                                                                                                                                                                                                                       ((lambda (k$256)
                                                                                                                                                                                                                                                          (if library?
                                                                                                                                                                                                                                                            ((lambda (r$257)
                                                                                                                                                                                                                                                               (k$256 (equal? lib-name$83 r$257)))
                                                                                                                                                                                                                                                             '(scheme base))
                                                                                                                                                                                                                                                            (k$256 #f)))
                                                                                                                                                                                                                                                        (lambda (r$247)
                                                                                                                                                                                                                                                          (if r$247
                                                                                                                                                                                                                                                            ((lambda ()
                                                                                                                                                                                                                                                               ((lambda (r$255)
                                                                                                                                                                                                                                                                  (append
                                                                                                                                                                                                                                                                    (lambda (r$254)
                                                                                                                                                                                                                                                                      ((lambda (r$248)
                                                                                                                                                                                                                                                                         ((lambda (r$253)
                                                                                                                                                                                                                                                                            (append
                                                                                                                                                                                                                                                                              (lambda (r$252)
                                                                                                                                                                                                                                                                                ((lambda (r$249)
                                                                                                                                                                                                                                                                                   ((lambda (r$251)
                                                                                                                                                                                                                                                                                      ((lambda (r$250)
                                                                                                                                                                                                                                                                                         (k$246 (set! input-program$73 r$250)))
                                                                                                                                                                                                                                                                                       (cons r$251 cps$92)))
                                                                                                                                                                                                                                                                                    '(define call/cc
                                                                                                                                                                                                                                                                                       (lambda (k f)
                                                                                                                                                                                                                                                                                         (f k (lambda (_ result) (k result)))))))
                                                                                                                                                                                                                                                                                 (set! module-globals$87 r$252)))
                                                                                                                                                                                                                                                                              r$253
                                                                                                                                                                                                                                                                              module-globals$87))
                                                                                                                                                                                                                                                                          '(call/cc)))
                                                                                                                                                                                                                                                                       (set! globals$88 r$254)))
                                                                                                                                                                                                                                                                    r$255
                                                                                                                                                                                                                                                                    globals$88))
                                                                                                                                                                                                                                                                '(call/cc))))
                                                                                                                                                                                                                                                            ((lambda ()
                                                                                                                                                                                                                                                               (k$246 (set! input-program$73 cps$92))))))))
                                                                                                                                                                                                                                                     (lambda (r$212)
                                                                                                                                                                                                                                                       (trace:info
                                                                                                                                                                                                                                                         (lambda (r$213)
                                                                                                                                                                                                                                                           (trace:info
                                                                                                                                                                                                                                                             (lambda (r$214)
                                                                                                                                                                                                                                                               ((lambda (r$241)
                                                                                                                                                                                                                                                                  (map (lambda (r$240)
                                                                                                                                                                                                                                                                         ((lambda (r$215)
                                                                                                                                                                                                                                                                            (trace:info
                                                                                                                                                                                                                                                                              (lambda (r$216)
                                                                                                                                                                                                                                                                                (trace:info
                                                                                                                                                                                                                                                                                  (lambda (r$217)
                                                                                                                                                                                                                                                                                    ((lambda (r$229)
                                                                                                                                                                                                                                                                                       (map (lambda (r$228)
                                                                                                                                                                                                                                                                                              ((lambda (r$218)
                                                                                                                                                                                                                                                                                                 (trace:info
                                                                                                                                                                                                                                                                                                   (lambda (r$219)
                                                                                                                                                                                                                                                                                                     (trace:info
                                                                                                                                                                                                                                                                                                       (lambda (r$220)
                                                                                                                                                                                                                                                                                                         ((lambda (k$225)
                                                                                                                                                                                                                                                                                                            (not (lambda (r$226)
                                                                                                                                                                                                                                                                                                                   (if r$226
                                                                                                                                                                                                                                                                                                                     ((lambda ()
                                                                                                                                                                                                                                                                                                                        (trace:error
                                                                                                                                                                                                                                                                                                                          (lambda (r$227) (k$225 (exit 0)))
                                                                                                                                                                                                                                                                                                                          "DEBUG, existing program")))
                                                                                                                                                                                                                                                                                                                     (k$225 #f)))
                                                                                                                                                                                                                                                                                                                 *do-code-gen*))
                                                                                                                                                                                                                                                                                                          (lambda (r$221)
                                                                                                                                                                                                                                                                                                            (trace:info
                                                                                                                                                                                                                                                                                                              (lambda (r$222)
                                                                                                                                                                                                                                                                                                                (mta:code-gen
                                                                                                                                                                                                                                                                                                                  (lambda (r$223)
                                                                                                                                                                                                                                                                                                                    ((lambda (r$224) (return$81 k$180 r$224)) '()))
                                                                                                                                                                                                                                                                                                                  input-program$73
                                                                                                                                                                                                                                                                                                                  program?$86
                                                                                                                                                                                                                                                                                                                  lib-name$83
                                                                                                                                                                                                                                                                                                                  lib-exports$82
                                                                                                                                                                                                                                                                                                                  imported-vars$84
                                                                                                                                                                                                                                                                                                                  module-globals$87
                                                                                                                                                                                                                                                                                                                  lib-deps$72
                                                                                                                                                                                                                                                                                                                  src-file$71))
                                                                                                                                                                                                                                                                                                              "---------------- C code:"))))
                                                                                                                                                                                                                                                                                                       input-program$73))
                                                                                                                                                                                                                                                                                                   "---------------- after closure-convert:"))
                                                                                                                                                                                                                                                                                               (set! input-program$73 r$228)))
                                                                                                                                                                                                                                                                                            r$229
                                                                                                                                                                                                                                                                                            input-program$73))
                                                                                                                                                                                                                                                                                     (lambda (k$230 expr$89)
                                                                                                                                                                                                                                                                                       (define?
                                                                                                                                                                                                                                                                                         (lambda (r$231)
                                                                                                                                                                                                                                                                                           (if r$231
                                                                                                                                                                                                                                                                                             ((lambda ()
                                                                                                                                                                                                                                                                                                ((lambda (r$232)
                                                                                                                                                                                                                                                                                                   (define->var
                                                                                                                                                                                                                                                                                                     (lambda (r$234)
                                                                                                                                                                                                                                                                                                       (define->exp
                                                                                                                                                                                                                                                                                                         (lambda (r$237)
                                                                                                                                                                                                                                                                                                           (closure-convert
                                                                                                                                                                                                                                                                                                             (lambda (r$236)
                                                                                                                                                                                                                                                                                                               ((lambda (r$235)
                                                                                                                                                                                                                                                                                                                  ((lambda (r$233) (k$230 (cons r$232 r$233)))
                                                                                                                                                                                                                                                                                                                   (cons r$234 r$235)))
                                                                                                                                                                                                                                                                                                                (caddr r$236)))
                                                                                                                                                                                                                                                                                                             r$237
                                                                                                                                                                                                                                                                                                             globals$88))
                                                                                                                                                                                                                                                                                                         expr$89))
                                                                                                                                                                                                                                                                                                     expr$89))
                                                                                                                                                                                                                                                                                                 'define)))
                                                                                                                                                                                                                                                                                             (define-c?
                                                                                                                                                                                                                                                                                               (lambda (r$238)
                                                                                                                                                                                                                                                                                                 (if r$238
                                                                                                                                                                                                                                                                                                   ((lambda () (k$230 expr$89)))
                                                                                                                                                                                                                                                                                                   ((lambda ()
                                                                                                                                                                                                                                                                                                      (closure-convert
                                                                                                                                                                                                                                                                                                        (lambda (r$239) (k$230 (caddr r$239)))
                                                                                                                                                                                                                                                                                                        expr$89
                                                                                                                                                                                                                                                                                                        globals$88)))))
                                                                                                                                                                                                                                                                                               expr$89)))
                                                                                                                                                                                                                                                                                         expr$89))))
                                                                                                                                                                                                                                                                                  input-program$73))
                                                                                                                                                                                                                                                                              "---------------- after wrap-mutables:"))
                                                                                                                                                                                                                                                                          (set! input-program$73 r$240)))
                                                                                                                                                                                                                                                                       r$241
                                                                                                                                                                                                                                                                       input-program$73))
                                                                                                                                                                                                                                                                (lambda (k$242 expr$90)
                                                                                                                                                                                                                                                                  (clear-mutables
                                                                                                                                                                                                                                                                    (lambda (r$243)
                                                                                                                                                                                                                                                                      (analyze-mutable-variables
                                                                                                                                                                                                                                                                        (lambda (r$244)
                                                                                                                                                                                                                                                                          (wrap-mutables k$242 expr$90 globals$88))
                                                                                                                                                                                                                                                                        expr$90))))))
                                                                                                                                                                                                                                                             input-program$73))
                                                                                                                                                                                                                                                         "---------------- after CPS:"))))
                                                                                                                                                                                                                                                  r$245))
                                                                                                                                                                                                                                               r$258
                                                                                                                                                                                                                                               input-program$73))
                                                                                                                                                                                                                                        (lambda (k$259 expr$91)
                                                                                                                                                                                                                                          (cps-convert k$259 expr$91))))
                                                                                                                                                                                                                                     input-program$73))
                                                                                                                                                                                                                                 "---------------- after alpha conversion:"))
                                                                                                                                                                                                                             (set! input-program$73 r$260)))
                                                                                                                                                                                                                          r$261
                                                                                                                                                                                                                          input-program$73))
                                                                                                                                                                                                                   (lambda (k$262 expr$93)
                                                                                                                                                                                                                     (alpha-convert
                                                                                                                                                                                                                       k$262
                                                                                                                                                                                                                       expr$93
                                                                                                                                                                                                                       globals$88
                                                                                                                                                                                                                       return$81))))
                                                                                                                                                                                                                (set! globals$88 r$263)))
                                                                                                                                                                                                             imported-vars$84
                                                                                                                                                                                                             module-globals$87))
                                                                                                                                                                                                         (set! module-globals$87 r$264)))
                                                                                                                                                                                                      input-program$73))
                                                                                                                                                                                                  input-program$73))
                                                                                                                                                                                              "---------------- after processing globals"))))
                                                                                                                                                                                       (set! input-program$73 r$270)))
                                                                                                                                                                                    input-program$73
                                                                                                                                                                                    program?$86
                                                                                                                                                                                    lib-name$83))
                                                                                                                                                                                input-program$73))
                                                                                                                                                                            "---------------- after macro expansion:"))
                                                                                                                                                                        (set! input-program$73 r$271)))
                                                                                                                                                                     input-program$73
                                                                                                                                                                     r$273))))))
                                                                                                                                                     *defined-macros*
                                                                                                                                                     r$275))
                                                                                                                                                 r$276
                                                                                                                                                 r$277))
                                                                                                                                             '()))
                                                                                                                                          '()))
                                                                                                                                       (set! *defined-macros* r$279)))
                                                                                                                                    macros$95
                                                                                                                                    *defined-macros*))
                                                                                                                                r$278))
                                                                                                                             r$280
                                                                                                                             r$281))
                                                                                                                         (Cyc-global-vars)))
                                                                                                                      (lambda (k$282 v$94)
                                                                                                                        ((lambda (r$284)
                                                                                                                           ((lambda (r$283) (k$282 (macro? r$283)))
                                                                                                                            (Cyc-get-cvar r$284)))
                                                                                                                         (cdr v$94)))))
                                                                                                                   meta$96))
                                                                                                               "resolved macros:"))
                                                                                                           (set! *defined-macros* r$288)))
                                                                                                        meta$96
                                                                                                        *defined-macros*))
                                                                                                    r$285))
                                                                                                 imports$85))
                                                                                             imported-vars$84))
                                                                                         "resolved imports:"))
                                                                                     (set! imported-vars$84 r$289)))
                                                                                  imports$85))
                                                                              imports$85))
                                                                          "imports:"))))
                                                                   input-program$73))
                                                               "---------------- input program:"))
                                                           *c-file-header-comment*))
                                                   (set! lib-exports$82 r$324)))
                                                '()))
                                             (set! lib-name$83 r$325)))
                                          '()))
                                       (set! imported-vars$84 r$326)))
                                    '()))
                                 (set! imports$85 r$327)))
                              '()))
                           (set! program?$86 #t)))
                        (set! module-globals$87 r$328)))
                     '()))
                  (set! globals$88 r$329)))
               '()))
            #f
            #f
            #f
            #f
            #f
            #f
            #f))))
      #f
      #f
      #f
      #f
      #f
      #f
      #f)))
 (define read-file
   (lambda (k$173 filename$69)
     ((lambda (r$174) (call-with-input-file k$173 filename$69 r$174))
      (lambda (k$175 port$70) (read-all k$175 port$70)))))
 (define run-compiler
   (lambda (k$130 args$53 cc?$52)
     ((lambda (r$131)
        ((lambda (in-file$54)
           (read-file
             (lambda (r$132)
               ((lambda (in-prog$55)
                  ((lambda (r$170)
                     (library?
                       (lambda (r$169)
                         (not (lambda (r$133)
                                ((lambda (program?$56)
                                   ((lambda (k$163)
                                      ((lambda (k$166)
                                         (if program?$56
                                           ((lambda (r$167)
                                              ((lambda (r$168) (tagged-list? k$166 r$167 r$168))
                                               (car in-prog$55)))
                                            'import)
                                           (k$166 #f)))
                                       (lambda (r$164)
                                         (if r$164
                                           ((lambda (r$165)
                                              (lib:get-all-import-deps k$163 r$165))
                                            (cdar in-prog$55))
                                           (k$163 '())))))
                                    (lambda (r$134)
                                      ((lambda (lib-deps$57)
                                         (basename
                                           (lambda (r$135)
                                             ((lambda (exec-file$58)
                                                ((lambda (r$136)
                                                   ((lambda (src-file$59)
                                                      ((lambda (r$137)
                                                         ((lambda (meta-file$60)
                                                            ((lambda (r$138)
                                                               ((lambda (create-c-file$62)
                                                                  (create-c-file$62
                                                                    (lambda (r$139)
                                                                      ((lambda (result$63)
                                                                         ((lambda ()
                                                                            (if program?$56
                                                                              ((lambda ()
                                                                                 ((lambda (objs-str$67 comp-prog-cmd$66 comp-objs-cmd$65)
                                                                                    ((lambda (r$150)
                                                                                       (map (lambda (r$149)
                                                                                              ((lambda (r$148)
                                                                                                 ((lambda (r$140)
                                                                                                    ((lambda (r$147)
                                                                                                       ((lambda (r$141)
                                                                                                          ((lambda (r$146)
                                                                                                             ((lambda (r$142)
                                                                                                                (if cc?$52
                                                                                                                  ((lambda ()
                                                                                                                     ((lambda (r$144)
                                                                                                                        ((lambda (r$143)
                                                                                                                           (if r$143
                                                                                                                             (k$130 (system comp-objs-cmd$65))
                                                                                                                             (k$130 #f)))
                                                                                                                         (equal? 0 r$144)))
                                                                                                                      (system comp-prog-cmd$66))))
                                                                                                                  ((lambda ()
                                                                                                                     (write (lambda (r$145) (write k$130 comp-objs-cmd$65))
                                                                                                                            comp-prog-cmd$66)))))
                                                                                                              (set! comp-objs-cmd$65 r$146)))
                                                                                                           (string-append
                                                                                                             "gcc "
                                                                                                             exec-file$58
                                                                                                             ".o "
                                                                                                             objs-str$67
                                                                                                             " -pthread -lcyclone -lck -lm -g -o "
                                                                                                             exec-file$58)))
                                                                                                        (set! comp-prog-cmd$66 r$147)))
                                                                                                     (string-append
                                                                                                       "gcc "
                                                                                                       src-file$59
                                                                                                       " -g -c -o "
                                                                                                       exec-file$58
                                                                                                       ".o")))
                                                                                                  (set! objs-str$67 r$148)))
                                                                                               (apply string-append r$149)))
                                                                                            r$150
                                                                                            lib-deps$57))
                                                                                     (lambda (k$151 i$68)
                                                                                       (lib:import->filename
                                                                                         (lambda (r$152)
                                                                                           (k$151 (string-append " " r$152 " ")))
                                                                                         i$68
                                                                                         ".o"))))
                                                                                  #f
                                                                                  #f
                                                                                  #f)))
                                                                              ((lambda ()
                                                                                 ((lambda (r$155)
                                                                                    (with-output-to-file
                                                                                      (lambda (r$153)
                                                                                        ((lambda (r$154)
                                                                                           ((lambda (comp-lib-cmd$64)
                                                                                              (if cc?$52
                                                                                                ((lambda () (k$130 (system comp-lib-cmd$64))))
                                                                                                ((lambda () (write k$130 comp-lib-cmd$64)))))
                                                                                            r$154))
                                                                                         (string-append
                                                                                           "gcc "
                                                                                           src-file$59
                                                                                           " -g -c -o "
                                                                                           exec-file$58
                                                                                           ".o")))
                                                                                      meta-file$60
                                                                                      r$155))
                                                                                  (lambda (k$156)
                                                                                    (display
                                                                                      (lambda (r$157)
                                                                                        (newline
                                                                                          (lambda (r$158)
                                                                                            (macro:get-defined-macros
                                                                                              (lambda (r$159) (write k$156 r$159))))))
                                                                                      ";; This file was automatically generated by the Cyclone Scheme compiler")))))))))
                                                                       r$139))
                                                                    in-prog$55))
                                                                r$138))
                                                             (lambda (k$160 program$61)
                                                               ((lambda (r$161)
                                                                  (with-output-to-file k$160 src-file$59 r$161))
                                                                (lambda (k$162)
                                                                  (c-compile-and-emit
                                                                    k$162
                                                                    program$61
                                                                    lib-deps$57
                                                                    in-file$54))))))
                                                          r$137))
                                                       (string-append exec-file$58 ".meta")))
                                                    r$136))
                                                 (string-append exec-file$58 ".c")))
                                              r$135))
                                           in-file$54))
                                       r$134))))
                                 r$133))
                              r$169))
                       r$170))
                   (car in-prog$55)))
                r$132))
             in-file$54))
         r$131))
      (car args$53))))
 ((lambda ()
    ((lambda (r$99)
       ((lambda (r$100)
          ((lambda (r$101)
             ((lambda (args$47)
                ((lambda (r$121)
                   (filter
                     (lambda (r$102)
                       ((lambda (non-opts$49)
                          ((lambda (compile?$50)
                             ((lambda ()
                                ((lambda (k$119)
                                   ((lambda (r$120)
                                      (if r$120
                                        (k$119 (set! *trace-level* 4))
                                        (k$119 #f)))
                                    (member "-t" args$47)))
                                 (lambda (r$103)
                                   ((lambda (k$117)
                                      ((lambda (r$118)
                                         (if r$118
                                           (k$117 (set! compile?$50 #f))
                                           (k$117 #f)))
                                       (member "-d" args$47)))
                                    (lambda (r$104)
                                      ((lambda (r$116)
                                         ((lambda (r$105)
                                            (if r$105
                                              ((lambda ()
                                                 (display
                                                   (lambda (r$106) (newline %halt))
                                                   "cyclone: no input file")))
                                              ((lambda (r$114)
                                                 ((lambda (tmp$51)
                                                    ((lambda (k$115)
                                                       (if tmp$51
                                                         (k$115 tmp$51)
                                                         (k$115 (member "--help" args$47))))
                                                     (lambda (r$107)
                                                       (if r$107
                                                         ((lambda ()
                                                            (display
                                                              (lambda (r$108) (newline %halt))
                                                              "\n -t              Show intermediate trace output in generated C files\n -d              Only generate intermediate C files, do not compile them\n -h, --help      Display usage information\n -v              Display version information\n --autogen       Cyclone developer use only, create autogen.out file\n")))
                                                         ((lambda (r$109)
                                                            (if r$109
                                                              ((lambda () (display %halt *version-banner*)))
                                                              ((lambda (r$110)
                                                                 (if r$110
                                                                   ((lambda ()
                                                                      (autogen
                                                                        (lambda (r$111) (newline %halt))
                                                                        "autogen.out")))
                                                                   ((lambda (r$112)
                                                                      (if r$112
                                                                        ((lambda () (display %halt *version-banner*)))
                                                                        ((lambda (r$113)
                                                                           (if r$113
                                                                             ((lambda () (autogen %halt "autogen.out")))
                                                                             ((lambda ()
                                                                                (run-compiler %halt non-opts$49 compile?$50)))))
                                                                         (member "--autogen" args$47))))
                                                                    (member "-v" args$47))))
                                                               (member "--autogen" args$47))))
                                                          (member "-v" args$47))))))
                                                  r$114))
                                               (member "-h" args$47))))
                                          (< r$116 1)))
                                       (length args$47)))))))))
                           #t))
                        r$102))
                     r$121
                     args$47))
                 (lambda (k$122 arg$48)
                   ((lambda (k$124)
                      ((lambda (r$127)
                         ((lambda (r$125)
                            (if r$125
                              ((lambda (r$126) (k$124 (equal? #\- r$126)))
                               (string-ref arg$48 0))
                              (k$124 #f)))
                          (> r$127 1)))
                       (string-length arg$48)))
                    (lambda (r$123) (not k$122 r$123))))))
              r$101))
           (command-line-arguments)))
        #f))
     0))))
#;1> 

(import (scheme base) (scheme write))
(define-syntax my-when
  (syntax-rules ()
    ((my-when test result1 result2 ...)
     (if test
         (begin result1 result2 ...)))))

(define-syntax my-when2
  (syntax-rules ()
    ((my-when test result1 result2 ...)
     (list test))))

(write
  (my-when2 #t 1))

 (define my-when2*
   (lambda (expr$28 rename$29 compare$30)
     (car ((lambda (tmp$42)
             (if tmp$42
               tmp$42
               (cons (error "no expansion for" expr$28) #f)))
           ((lambda (v.1$36)
              (if (pair? v.1$36)
                ((lambda (v.2$37)
                   ((lambda (test)
                      ((lambda (v.3$38)
                         (if (pair? v.3$38)
                           ((lambda (v.4$39)
                              ((lambda (result1)
                                 ((lambda (v.5$40)
                                    (if (list? v.5$40)
                                      ((lambda (result2)
                                         (cons (cons-source
                                                 (rename$29 'list)
                                                 (cons-source test '() '(test))
                                                 '(list test))
                                               #f))
                                       v.5$40)
                                      #f))
                                  (cdr v.3$38)))
                               v.4$39))
                            (car v.3$38))
                           #f))
                       (cdr v.1$36)))
                    v.2$37))
                 (car v.1$36))
                #f))
            (cdr expr$28))))))
(write
  (my-when2* '(my-when2* 't 1) (lambda (a) a) (lambda X #f)))

(import (scheme base)
        (scheme read))

(write `(read ,(+ 1 2 3)))
(write `(read ,(list 1 2 3)))
(write `(read ,@(list 1 2 3)))
;`(read ,
(write (make-vector 4 #t))
(write (string->list "abc"))
(write (apply append '((1) (2) (3))))

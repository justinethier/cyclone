(import (scheme base)
        (scheme read))

(write `(read ,(+ 1 2 3)))
(write `(read ,(list 1 2 3)))
(write `(read ,@(list 1 2 3)))
;`(read ,

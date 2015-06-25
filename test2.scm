(import (scheme base)
        (scheme file)
        (scheme write))

;; TODO: fails with an "unspecified" error unless there is an else clause. WTF?
;(write 
  (cond
    (#f #f)
    ;(else #t)
  )
;)


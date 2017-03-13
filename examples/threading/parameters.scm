;; A simple program demonstrating how parameter objects interact with threads
;;
;; Note this is poor code as it uses timing via sleeps instead of proper 
;; thread synchronization!!!
;;
(import (scheme base)
        (scheme read)
        (scheme write)
        (srfi 18)
        )

(thread-start!
  (make-thread
    (lambda ()
      (thread-sleep! 2000)
      (display "started thread, this should be written to console")
      (newline)
      (display "thread done")
      (newline)
      (flush-output-port (current-output-port)))))

(thread-sleep! 1000) ;; Prevent race condition replacing stdout before thread is spawned
(write `(1 2 3))
(define fp (open-output-file "tmp.txt"))
(parameterize
  ((current-output-port fp))
  (write `(4 5 6))
  (thread-sleep! 3000)
)
(close-port fp)
(write `(7 8 9))

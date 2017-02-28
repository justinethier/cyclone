;; A simple program demonstrating how parameter objects interact with threads
(import (scheme base)
        (scheme read)
        (scheme write)
        (srfi 18)
        )

(thread-start!
  (make-thread
    (lambda ()
      (thread-sleep! 1000)
      (display "started thread, this should be written to console")
      (newline)
      (display "thread done")
      (newline))))

(write `(1 2 3))
(define fp (open-output-file "tmp.txt"))
(parameterize
  ((current-output-port fp))
  (write `(4 5 6))
  (thread-sleep! 5000)
)
(close-port fp)
(write `(7 8 9))

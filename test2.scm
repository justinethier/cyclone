(import (scheme base)
        (scheme file)
        (scheme write))

(let loop ((i 10))
  (if (zero? i)
    (write 'done)
    (loop (- i 1))))


(write (when (lambda () #t) 'true))
(write (when (lambda () #f) 'false))

((lambda test (write test)) 1 2 3 4)

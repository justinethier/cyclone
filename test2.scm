(import (scheme base)
        (scheme file)
        (scheme write))

(let loop ((i 10))
  (if (zero? i)
    (write 'done)
    (loop (- i 1))))

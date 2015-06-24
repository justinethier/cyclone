(import (scheme base)
        (scheme file))

(with-output-to-file 
  "test.out" 
  (lambda ()
    (write 'hello)
    (display " ")
    (display 'world)))

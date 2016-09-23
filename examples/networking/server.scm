(import (scheme base) (scheme write) (srfi 106))

(define svr (make-server-socket "12345"))

(let* ((s (socket-accept svr))
       (data (socket-recv s 1024)))
  (display `("received: " ,(utf8->string data)))
  (newline)
  (socket-close s))

(socket-close svr)
  

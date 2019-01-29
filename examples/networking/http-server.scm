(import 
  (scheme base) 
  (scheme write) 
  ; (srfi 18)
  (srfi 106))

(define *port* "8080")
(define svr (make-server-socket *port*))

(let* ((s (socket-accept svr))
       (data (socket-recv s 2048)))
  (display `("received: " ,(utf8->string data)))
  (newline)
  (socket-send s
    (string->utf8
      "HTTP/1.1 200 OK\nContent-Type: text/plain\nContent-Length: 12\n\nHello world!"))
  (socket-close s))

(socket-close svr)
  

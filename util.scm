
(define (tagged-list? tag exp)
  (if (pair? exp)
      (equal? (car exp) tag)
      #f))

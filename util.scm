
(define (tagged-list? tag exp)
  (if (pair? exp)
      (equal? (car exp) tag)
      #f))

; char->natural : char -> natural
(define (char->natural c)
  (let ((i (char->integer c)))
    (if (< i 0)
        (* -2 i)
        (+ (* 2 i) 1))))

; integer->char-list : integer -> string
(define (integer->char-list n)
  (string->list (number->string n)))

;; Name-mangling.

;; We have to "mangle" Scheme identifiers into
;; C-compatible identifiers, because names like
;; foo-bar/baz are not identifiers in C.

; mangle : symbol -> string
(define (mangle symbol)
 (letrec
   ((m (lambda (chars)
      (if (null? chars)
        '()
        (if (or (and (char-alphabetic? (car chars)) (not (char=? (car chars) #\_)))
                (char-numeric? (car chars)))
            (cons (car chars) (m (cdr chars)))
            (cons #\_ (append (integer->char-list (char->natural (car chars)))
                              (m (cdr chars))))))))
    (ident (list->string (m (string->list (symbol->string symbol))))))
   (if (member (string->symbol ident) *c-keywords*)
     (string-append "_" ident)
     ident)))

(define (mangle-global symbol)
  (string-append "__glo_" (mangle symbol)))

(define *c-keywords* 
 '(auto _Bool break case char _Complex const continue default do double else
   enum extern float for goto if _Imaginary inline int long register restrict
   return short signed sizeof static struct switch typedef union unsigned
   void volatile while
   list  ;; Not a keyword but reserved type
   ))
;; END name mangling section


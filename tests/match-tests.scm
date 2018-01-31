(import 
  (scheme base) 
  (scheme write)
)
(cond-expand
  (cyclone
    (import
      (scheme cyclone match)
      (scheme cyclone test)))
  (chibi
    (import
      (chibi ast)
      (chibi match)
      (chibi test)))
)

;; Temporary test section
;; #;(display
;;   ;(match "test" ((? string? s) s) (else #f))
;;   ;
;;   ;(let ((v "test"))
;;   ;  (match-next v ("test" (set! "test")) ((? string? s) s) (else #f)))
;;   ;
;;   ;(let ((v "test"))
;;   ;  (let ((failure (lambda () (match-next v ("test" (set! "test")) (else #f)))))
;;   ;    (match-one v ((? string? s) s) ("test" (set! "test")) (match-drop-ids (begin . s)) (failure) ())))
;;   ;
;;   ;(let ((v "test"))
;;   ;  (let ((failure (lambda () (match-next v ("test" (set! "test")) (else #f)))))
;;   ;   (match-check-ellipsis
;;   ;    s
;;   ;    (match-extract-vars (? string? s) (match-gen-ellipsis v (? string? s) ()  ("test" (set! "test")) (match-drop-ids (begin . s)) (failure) ()) () ())
;;   ;    (match-two v ((? string? s) s) ("test" (set! "test")) (match-drop-ids (begin . s)) (failure) ()))))
;;   ;
;;   ;(let ((v "test"))
;;   ;  (let ((failure (lambda () (match-next v ("test" (set! "test")) (else #f)))))
;;   ;   (match-check-ellipsis
;;   ;    s
;;   ;    (match-extract-vars (? string? s) (match-gen-ellipsis v (? string? s) ()  ("test" (set! "test")) (match-drop-ids (begin . s)) (failure) ()) () ())
;;   ;    (match-two v ((? string? s) s) ("test" (set! "test")) (match-drop-ids (begin . s)) (failure) ()))))
;;   ;
;;   ;(let ((v "test"))
;;   ;  (let ((failure (lambda () (match-next v ("test" (set! "test")) (else #f)))))
;;   ;   (match-two v ((? string? s) s) ("test" (set! "test")) (match-drop-ids (begin . s)) (failure) ())))
;;   ; END expansions we are sure about, below is just WIP:
;; 
;; ;  (let ((v "test"))
;; ;    (let ((failure (lambda () (match-next v ("test" (set! "test")) (else #f)))))
;; ;     (if (string? v) 
;; ;       (match-one v (and s) ("test" (set! "test")) (match-drop-ids (begin s)) (failure) ())
;; ;       (failure))))
;; 
;; ;; Following two are broken when using "and" but if we replace "and" with "my-and" in 
;; ;; the lib's match-two macro and recompile, the following both work here with "my-and".
;; ;; Something funny going on here...
;; ;   (match-one "test" (and s) ("test" (set! "test")) (match-drop-ids (begin s)) (failure) ())
;; ; (match 1 ((and x) x))
;;   (match-two 1 (and x) (1 (set! 1)) (match-drop-ids (begin x)) (begin) ())
;; ;  (match-two "test" ((? string? s) s) ("test" (set! "test")) (match-drop-ids (begin . s)) (begin) ())
;; 
;; ;; I think there is some kind of interaction going on here with the "and" macro, where it
;; ;; is being expanded even though it is part of the syntax-rules literals and should not be.
;; ;; Just a guess, need to prove it, but it could explain why we fall into this case even though
;; ;; pattern should have been (and p) - though not 100% sure, just a guess at this point
;; ;    ((match-two v (p) g+s sk fk i)
;; ;     (if (and (pair? v) (null? (cdr v)))
;; ;         (let ((w (car v)))
;; ;           (match-one w p ((car v) (set-car! v)) sk fk i))
;; ;         fk))
;; 
;; )
;; 
;; ;(expand (match "test" ((? string? s) s) (else #f)))*/
;; ;(expand (match-two$171 v$1 (? string? s) ("test" (set! "test")) (match-drop-ids$9 (begin s)) (failure$5) ()))*/
;; ;(expand (match-one$266 v$1 (and$262 s) ("test" (set! "test")) (match-drop-ids$9 (begin s)) (failure$5) ()))*/
;; ;(expand (match-check-ellipsis$270 s (match-extract-vars$269 and$262 (match-gen-ellipsis$268 v$1 and$262 () ("test" (set! "test")) (match-drop-ids$9 (begin s)) (failure$5) ()) () ()) (match-two$267 v$1 (and$262 s) ("test" (set! "test")) (match-drop-ids$9 (begin s)) (failure$5) ())))*/
;; ;(expand (match-two$267 v$1 (and$262 s) ("test" (set! "test")) (match-drop-ids$9 (begin s)) (failure$5) ()))*/

;;TODO: this does not work, try expanding it manually like we did with the other failing macros. maybe we can discover what's going wrong...
;; NOTE there is a warning in chibi on this one. maybe this is not a big deal
;(display (match 1 ((or x 2) x)) )(newline)
#;(display
    (let ()
      (define-record-type employee
        (make-employee name title)
        employee?
        (name get-name)
        (title get-title))
      (match (make-employee "Bob" "Doctor")
        (($ employee n t) (list t n))))
)

(define lst 1)
(display
(match
   lst
   ((? string? s) s)
   (else 0)
   )
)
(define (calc-time lst)
  (match
   lst
   ;(() 0)
   (((? number? n) (or 's 'seconds 'sec) ) ;. rest)
    (+ 0 (* #e1 n) )) ;(calc-time rest)))
    ;; TODO: interesting compiler error with this line:
;    (+ (* #e1 n) )) ;(calc-time rest)))
   (((?  number? n) (or 'm 'min 'minutes) . rest)
    (+ (* #e60 n) (calc-time rest)))
;   (((and (?  number?) ?n) (or m min minutes) . ?rest)
;    (+ (* #e60 n) (calc-time rest)))
;   (((and (? number?) ?n) (or hours h) . ?rest)
;    (+ (* #e60 60 n) (calc-time rest)))
;   (((and (? number?) ?n) (or d days day) . ?rest)
;    (+ (* #e60 60 24 n) (calc-time rest)))
;   (((and (? number?) ?n) (or w week weeks) . ?rest)
;    (+ (* #e60 60 24 7 n) (calc-time rest)))
   (else 0)
))

(newline)
(display
  (list 
    (calc-time '(5 min 10 sec))
))

#;(test-group
  "predicates"
  (test "test" (match "test" ((? string? s) s) (else #f)))

  (test #(fromlist 1 2) (match '(1 2) ((a b) (vector 'fromlist a b))))
  (test #f (match 42 (X #f)))
)

#;(test-group
  "official tests"

  (test 2 (match (list 1 2 3) ((a b c) b)) )
  (test 2 (match (list 1 2 1) ((a a b) 1) ((a b a) 2)))
  (test 1 (match (list 1 2 1) ((_ _ b) 1) ((a b a) 2)) )
  (test 2 (match 'a ('b 1) ('a 2)) )

  (test '(2 3) (match (list 1 2 3) (`(1 ,b ,c) (list b c))))

  (test #t (match (list 1 2) ((1 2 3 ...) #t)) )
  (test #t (match (list 1 2 3) ((1 2 3 ...) #t)) )
  (test #t (match (list 1 2 3 3 3) ((1 2 3 ...) #t)) )
  (test '() (match (list 1 2) ((a b c ...) c)) )
  (test '(3) (match (list 1 2 3) ((a b c ...) c)) )
  (test '(3 4 5) (match (list 1 2 3 4 5) ((a b c ...) c)) )
  (test '() (match (list 1 2 3 4) ((a b c ... d e) c)) )
  (test '(3) (match (list 1 2 3 4 5) ((a b c ... d e) c)) )
  (test '(3 4 5) (match (list 1 2 3 4 5 6 7) ((a b c ... d e) c)) )

;; Next fails on cyclone and chibi, I believe intentionally
;;; Pattern not matched
;(display (match (list 1 2) ((a b c ..1) c)) )(newline)

  (test '(3) (match (list 1 2 3) ((a b c ..1) c)))

  (test #t (match 1 ((and) #t)))
  (test 1 (match 1 ((and x) x)))
  (test 1 (match 1 ((and x 1) x)))

  (test #f (match 1 ((or) #t) (else #f)) )

  (test 1 (match 1 ((or x) x)))
;; Next fails on cyclone but pass on chibi
;(display (match 1 ((or x 2) x)) )(newline)

  (test #t (match 1 ((not 2) #t)) )

  (test 1 (match 1 ((? odd? x) x)))
  (test 1 (match '(1 . 2) ((= car x) x)) )
  (test 16 (match 4 ((= square x) x)) )

;; TODO: Fails on cyclone but passes on chibi
;; expect '("Doctor" "Bob")
#;(display
    (let ()
      (define-record-type employee
        (make-employee name title)
        employee?
        (name get-name)
        (title get-title))
      (match (make-employee "Bob" "Doctor")
        (($ employee n t) (list t n))))
)

  (test '(1 . 3) (let ((x (cons 1 2))) (match x ((1 . (set! s)) (s 3) x))))
  (test 2 (match '(1 . 2) ((1 . (get! g)) (g))))
  (test '(a a a) (match '(a (a (a b))) ((x *** 'b) x)))
  (test '(a c f) (match '(a (b) (c (d e) (f g))) ((x *** 'g) x)))

)
(test-exit)

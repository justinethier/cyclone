(import (scheme base)
        (scheme char)
        (scheme file)
        (scheme lazy)
        (scheme read)
        (scheme write)
        (scheme eval)
        ;(srfi 9)
)

(define *num-passed* 0)
(define (assert:equal msg actual expected)
  (if (not (equal? actual expected))
      (error "Unit test failed [" msg "] actual [" actual "] expected [" expected "]")
      (set! *num-passed* (+ *num-passed* 1))))

(define (assert:not-equal msg x y)
  (assert:equal msg (not (equal? x y)) #t))

(define (assert msg val)
  (assert:equal msg (not val) #f))

(assert "Testing assert function" #t)
(assert "Testing assert function" 1)

;; Quotation / Quasi-quotes
(assert:equal "quasi-quote #1" `(read ,(+ 1 2 3)) '(read 6))
(assert:equal "quasi-quote #2" `(read ,(list 1 2 3)) '(read (1 2 3)))
(assert:equal "quasi-quote splicing" `(read ,@(list 1 2 3)) '(read 1 2 3))

;; Lists
(define l (list 'a 'b 'c))
; TODO: seems to break eval below, is there a GC problem with a circular list?
;(set-cdr! l l)
(set-cdr! l '(c b)) ; Above seems to break if it replaces this line
(assert:equal "list? on circular list" (list? l) #t)

;; Adder example
(define (make-adder x)
  (lambda (y) (+ x  y)))
(define increment (make-adder +1))
(assert:equal "Adder #1" (increment 41) 42)
(define decrement (make-adder -1))
(assert:equal "Adder #2" (decrement 42) 41)

(assert:equal "Application example"
  ((lambda (x) x) (+ 41 1))
  42)

;; Apply section
(assert:equal "" (apply length '((#t #f))) 2)
(assert:equal "" (apply cons '(#t #f)) '(#t . #f))
(assert:equal "" (apply cadr (list (list 1 2 3 4))) 2)
(assert:equal "" (apply null? (list '())) #t)
;; Varargs
(define (list2 a b . objs) objs)
(assert:equal "apply varargs" (list 42 1) '(42 1))
(assert:equal "apply varargs" (list 42 1 2) '(42 1 2))
(assert:equal "apply varargs" (list2 42 1) '())
(assert:equal "apply varargs" (list2 42 1 2) '(2))

(assert:equal "begin" (begin 1 2 (+ 1 2) (+ 3 4)) 7)

;; Continuation section
(assert:equal
    "simple call/cc"
    (call/cc
      (lambda (k)
        (k 2)))
    2)
(assert:equal "escape continuation"
    (call/cc
      (lambda (return)
        (begin
          (return 'return))))
    'return)

;; Closure section
(assert:equal "simple closure"
  (((lambda (x.1) 
    (lambda (y.2) 
      (cons x.1 y.2))) #t) #f)
 '(#t . #f))
(assert:equal "closure #2"
  ((lambda (x y)
    ((lambda () (- x y)))) 5 4)
  1)

;; Anonymous lambda's
(assert:equal "execute anonymous lambda" ((lambda (a . Y) Y) 'x) '())
(assert:equal "execute anonymous lambda" ((lambda (a . Y) Y) 'x 'y) '(y))
(assert:equal "execute anonymous lambda" ((lambda (a . Y) Y) 'x 'y 'z) '(y z)) 
(assert:equal "execute anonymous lambda" ((lambda (a b . Y) Y) 'x 'y 'z) '(z))
(assert:equal "execute anonymous lambda" ((lambda Y Y) 'x 'y 'z) '(x y z))
(assert:equal "execute anonymous lambda" ((lambda Y Y) ) '())

;; Factorial
(define (fac n) (if (= n 0) 1 (* n (fac (- n 1)))))
(assert:equal "Factorial example" (fac 10) 3628800)

;; If section
(assert:equal "if example" (if #t 1 2) 1)
(assert:equal "if example" (if #f 1 2) 2)
(assert:equal "if example" (if (+ 1 2) (+ 3 4) (* 3 4)) 7)
(assert:equal "if" (if ((lambda (x) (+ x 1)) 0) (+ 1 1) (* 0 0)) 2)
(assert:equal "no else clause" (if #t 'no-else-clause) 'no-else-clause)

(assert:equal "" (+ (+ 1 1) (* 3 4)) 14)

;; Set section
((lambda (x)
    (set! x #t) ; (+ 2 (* 3 4)))
    (assert:equal "set local x" x #t))
 #f)

(define a (list #f #f))
(define b (cons #f #f))

(set-car! a 1)
(set-cdr! a '(2))
(assert:equal "set car/cdr a" a '(1 2))
(set-cdr! a 2)
(set-car! b '(#t))
(set-cdr! b '#t)

(assert:equal "set a" a '(1 . 2))
(assert:equal "set b" b '((#t) . #t))

;; Scoping example
(define scope #f)
(assert:equal "outer scope" scope #f)
((lambda (scope)
    (assert:equal "inner scope" scope #t)
 ) #t)

;; Square example
(let ((x 10) 
      (y 20) 
      (square (lambda (x) (* x x)))) 
  (begin 
    (assert:equal "square x" (square x) 100) 
    (assert:equal "square y" (square y) 400)))

;; String section
(define a "a0123456789")
(assert:equal "string eq" a "a0123456789")
(assert:not-equal "string eq" a 'a0123456789)
(define b "abcdefghijklmnopqrstuvwxyz")
(define c "hello, world!")
(define d (list->string '(#\( #\" #\a #\b #\c #\" #\))))
(assert:equal "strings" d "(\"abc\")")
(assert:equal "strings" d "(\"abc\")") ;; Test GC
(assert:equal "strings" d "(\"abc\")") ;; Test GC
(set! a "hello 2")
(assert:equal "strings" a "hello 2")
(define (f) (make-string 3 #\*))
(define (g) "***")
(assert:equal "string-set!" (string-set! (f) 0 #\?) "?**")
(define a "12345")
(define b (string-copy "abcde"))
(assert:equal "string-copy!" (string-copy! b 1 a 0 2) "a12de")
(let ((v '()))
  (string-for-each
    (lambda (c) (set! v (cons (char->integer c) v)))
    "abcde")
  (assert:equal "string-for-each" v '(101 100 99 98 97)))

(assert:equal "interline whitespace" "1 2 \      
3 4" "1 2 3 4")

(assert:equal "interline whitespace" "1 2 \


3 4" "1 2 \n\n3 4")

;; UTF-8 / Strings
(assert:equal "UTF8 string length" (string-length (make-string 1 (integer->char 128))) 1)
(assert:equal "UTF8 bv length" (bytevector-length (string->utf8 (make-string 1 (integer->char 128)))) 2)
(assert:equal "UTF8 char" (string-ref (make-string 1 (integer->char 128)) 0) #\x80)

;; Recursion example:
(letrec ((fnc (lambda (i) 
                (begin
                    ;(display i)
                    (if (> i 0) (fnc (- i 1)) 0)))))
    (fnc 10))

(assert:equal "numeric small reverse" (reverse '(1 2)) '(2 1))
(assert:equal "small reverse" (reverse '(a b c)) '(c b a))
(assert:equal "larger reverse" (reverse '(1 2 3 4 5 6 7 8 9 10)) '(10 9 8 7 6 5 4 3 2 1))
;;  ;TODO: improper list, this is an error: (reverse '(1 . 2))
(assert:equal "char whitespace" (char-whitespace? #\space) #t)
(assert:equal "char whitespace" (char-whitespace? #\a) #f)
(assert:equal "char numeric" (char-numeric? #\1) #t)
(assert:equal "char numeric" (char-numeric? #\newline) #f)
(assert:equal "" (and 1 2 3) 3)
(assert:equal "" (and #t #f 'a 'b 'c) #f)
(assert:equal "" (or 1 2 3) 1)
(assert:equal "" (or #f 'a 'b 'c) 'a)
(assert:equal "" (string-append "") "")
;error - (string-append 1)
(assert:equal "" (string-append "test") "test")
(assert:equal "" (string-append "ab" "cdefgh ij" "klmno" "p" "q" "rs  " "tuv" "w" " x " "yz")
  "abcdefgh ijklmnopqrs  tuvw x yz")
(assert:equal "" (string->number "0") 0)
(assert:equal "" (string->number "42") 42)
;(assert:equal "" (string->number "343243243232") ;; Note no bignum support
(assert:equal "" (string->number "3.14159") 3.14159)
(assert:equal "" (list->string (list #\A #\B #\C)) "ABC")
(assert:equal "" (list->string (list #\A)) "A")
(assert:equal "" (list->string (list)) "") 
(assert:equal "" (integer->char 65) #\A)
(assert:equal "" (char->integer #\a) 97)

(assert:equal "" (number->string (+ 1 2)) "3")
(assert:equal "" (string->list "test") '(#\t #\e #\s #\t))
(assert:equal "" (string->symbol "a-b-c-d") 'a-b-c-d)
(assert:equal "" (symbol->string 'a/test-01) "a/test-01")
(assert:equal "" (eq? 'a-1 'a-1) #t)
(assert:equal "" (eq? (string->symbol "aa") 'aa) #t)
(assert:equal "" (eq? 0.0 0.0) #f)
(assert:equal "" (eq? 33333333333333 33333333333333) #f)
(assert:equal "" (eqv? 'a-1 'a-1) #t)
(assert:equal "" (eqv? (string->symbol "aa") 'aa) #t)
(assert:equal "" (eqv? 0.0 0.0) #t)
(assert:equal "" (eqv? 33333333333333 33333333333333) #t)
(assert:equal "" (equal? (string->symbol "aa") 'aa) #t)

;; Map
(assert:equal "map 1" (map (lambda (x) (car x)) '((a . b) (1 . 2) (#\h #\w))) '(a 1 #\h))
(assert:equal "map 2" (map car '((a . b) (1 . 2) (#\h #\w))) '(a 1 #\h))
(assert:equal "map 3" (map cdr '((a . b) (1 . 2) (#\h #\w))) '(b 2 (#\w)))
(assert:equal "map length"
  (map length '(() (1) (1 2) (1 2 3) (1 2 3 4)))
 '(0 1 2 3 4))

;; Prove internal defines are compiled properly
;;
;; Illustrates an old problem with compiling parser.
;; how to handle the internal define p?
;; trans was trying to wrap p with a lambda, which is not going to
;; work because callers want to pass a,b,c directly.
(define (glob a b c)
  (define (p d)
    (list a b c d))
  (p 4))
(assert:equal "internal defs for global funcs"
        (glob 1 2 3)
       '(1 2 3 4))

;; Global shadowing issue
;; Do not allow global define to shadow local ones
(define x 'global)
((lambda ()
  (define x 1)
  ((lambda ()
    (define x 2)
    (assert:equal "local define of x" x 2)))
  (assert:equal "another local define of x" x 1)))
(assert:equal "global define of x" x 'global)

; TODO: could add parser tests for these
;(
;123(list)
;1'b
;(write
;  (list
;  1;2
;  ))
;1;2
;3"four five"
;#\space
;)

;; EVAL section
(define x 1)
(define y 2)
(define *z* 3)
;(write (eval '(Cyc-global-vars)))
(assert:equal "eval compiled - x" (eval 'x) x)
(eval '(set! x 'mutated-x))
(assert:equal "Access var with a mangled name" (eval '*z*) *z*)
(assert:equal "Access compiled var mutated by eval" x 'mutated-x)
;; END eval


;; Vectors

(assert:equal "vector-ref" (vector-ref #(1 1 2 3 5 8 13 21) 5) 8)
(assert:equal "vector-set!" (vector-set! #(0 '(2 2 2 2) "anna") 1 "test") #(0 "test" "anna"))
(assert:equal "vector->list" (vector->list #(dah dah didah))
                            '(dah dah didah))
;(vector->list .#(dah dah didah) 1 2)
;=. (dah)
(assert:equal "list->vector" (list->vector '(dididit dah))
                             #(dididit dah))

(assert:equal "string->vector" (string->vector "ABC")  #(#\A #\B #\C))
(assert:equal "vector->string" (vector->string #(#\1 #\2 #\3)) "123")

(define a (vector 1 2 3 4 5))
(define b (vector 10 20 30 40 50))
(vector-copy! b 1 a 0 2)
(assert "vector-copy!" (equal? b #(10 1 2 40 50)))

(define a (vector 1 2 3 4 5))
(vector-fill! a 'smash 2 4)
(assert:equal "vector-fill!" a #(1 2 smash smash 5))
;; END vectors

;; I/O
(with-output-to-file 
  "test.out" 
  (lambda ()
    (write 'hello-world)))
;(write "done with output")
(with-input-from-file
  "test.out"
  (lambda ()
    (assert:equal "I/O with-*-file test" (read) 'hello-world)))
(call-with-output-file "test.txt" (lambda (port) (write 'ok port))) ;; Note: was a good test, did not always work in icyc
(call-with-input-file "test.txt" (lambda (port) (assert:equal "read input" (read port) 'ok)))
;(write "done with input")
;; TODO: (delete-file "test.out")
;; TODO: (delete-file "test.txt")
;; END I/O

;; Macros
(define-syntax test 
  (er-macro-transformer
     (lambda (expr rename compare)
       (cond ((null? (cdr expr)) #t)
             ((null? (cddr expr)) (cadr expr))
             (else (list (rename 'if) (cadr expr)
                         (cons (rename 'and) (cddr expr))
                         #f))))))

(define-syntax test2
  (er-macro-transformer
     (lambda (expr rename compare)
       (test 1 2)
       (test 1 2 3)
       (and ''test ''test2))))

(define-syntax indirect-def
 (syntax-rules ()
  ( (indirect-def x y)
    (begin
     (define z y)
     (define x z) ) ) ) )

(define x 42)
(assert:equal "macro: test2" (test2 1 2 3) 'test2)
(assert:equal "macro: test" (test 1 2 3) 3)
(assert:equal "macro: eval test" (eval '(test 1 2 x)) x)

(assert:equal "macro: define syntax indirect-def" 
  (indirect-def a 3)
  '(3))

(assert:equal "macro: let syntax indirect-def" 
 (let-syntax 
  ( (indirect-def
     (syntax-rules ()
      ( (indirect-def x y)
        (begin
         (define z y)
         (define x z) ) ) ) ) )
  (indirect-def a 3)
  a)
 '3)
;; END macros

;; Record types
(define-record-type <pare>
  (kons x y)
  pare?
  (x kar set-kar!)
  (y kdr))

(assert:equal "Records predicate (t)" (pare? (kons 1 2)) #t)
(assert:equal "Records predicate (f)" (pare? (cons 1 2)) #f)
(assert:equal "Records kar" (kar (kons 1 2)) 1)
(assert:equal "Records kdr" (kdr (kons 1 2)) 2)
(assert:equal "Records setter"
  (let ((k (kons 1 2)))
    (set-kar! k 3)
    (kar k))
  3)
(assert:equal "Record type predicate (t)" (record? (kons 1 2)) #t)
(assert:equal "Record type predicate (f)" (record? (cons 1 2)) #f)
;; END records

;; Lazy evaluation
(assert:equal "Basic lazy" (force (delay (+ 1 2))) 3)
(assert:equal "Lazy test #2"
  (let ((p (delay (+ 1 2))))
    (list (force p) (force p)))
  '(3 3))
((lambda ()
  (define integers
    (letrec ((next
              (lambda (n)
                (delay (cons n (next (+ n 1)))))))
      (next 0)))
  (define head
    (lambda (stream) (car (force stream))))
  (define tail
    (lambda (stream) (cdr (force stream))))
  (assert:equal "Lazy #3" (head (tail (tail integers))) 2)))


; TODO: use display, output without surrounding quotes
(display (list *num-passed* "tests passed with no errors"))
(newline)
;;

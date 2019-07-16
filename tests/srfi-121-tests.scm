#|
 | Copyright (c) 2017 Koz Ross 
 |
 | Permission is hereby granted, free of charge, to any person obtaining a copy of
 | this software and associated documentation files (the "Software"), to deal in
 | the Software without restriction, including without limitation the rights to
 | use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
 | the Software, and to permit persons to whom the Software is furnished to do so,
 | subject to the following conditions:
 |
 | The above copyright notice and this permission notice shall be included in all
 | copies or substantial portions of the Software.
 |
 | THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 | IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
 | FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
 | COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
 | IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 | CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 |#

(import 
  (scheme base)
  (srfi 121)
  (only (srfi 1) unfold)
  (cyclone test))

(define g
  (make-coroutine-generator
    (lambda (yield)
      (let loop ((i 0))
        (when (< i 3)
          (yield i)
          (loop (+ i 1)))))))

(define (for-each-digit proc n)
  (when (> n 0)
    (receive (div rem) (truncate/ n 10)
      (proc rem)
      (for-each-digit proc div))))

(test-group 
  "generator constructors"
  (test '() (generator->list (generator)))
  (test '(1 2 3) (generator->list (generator 1 2 3)))
  (test '(8 9 10) (generator->list (make-iota-generator 3 8)))
  (test '(8 10 12) (generator->list (make-iota-generator 3 8 2)))
  (test '(3 4 5 6) (generator->list (make-range-generator 3) 4))
  (test '(3 4 5 6 7) (generator->list (make-range-generator 3 8)))
  (test '(3 5 7) (generator->list (make-range-generator 3 8 2)))
  (test '(0 1 2) (generator->list g))
  (test '(1 2 3 4 5) 
        (generator->list (list->generator (list 1 2 3 4 5))))
  (test '(1 2 3 4 5) 
        (generator->list (vector->generator (vector 1 2 3 4 5))))
  (test '(5 4 3 2 1) 
        (generator->list (reverse-vector->generator (vector 1 2 3 4 5))))
  (test '(#\a #\b #\c #\d #\e)
        (generator->list (string->generator "abcde")))
  (test '(10 20 30) 
        (generator->list (bytevector->generator (bytevector 10 20 30))))
  (test '(5 4 3 2 1) 
        (generator->list (make-for-each-generator for-each-digit 12345)))
  (test '(0 2 4 6 8 10)
        (generator->list
          (make-unfold-generator (lambda (s) (> s 5))
                                 (lambda (s) (* s 2))
                                 (lambda (s) (+ s 1))
                                 0))))

(define g (make-range-generator 1 5))

(define g1 (generator 1 2 3))

(define g2 (generator 4 5 6 7))

(define (proc . args)
  (values (apply + args) (apply + args)))

(define (small? x) (< x 3))

(test-group
  "generator operators"
  (test '(a b 0 1) 
        (generator->list (gcons* 'a 'b (make-range-generator 0 2))))
  (test '(0 1 2 0 1)
        (generator->list (gappend (make-range-generator 0 3)
                                  (make-range-generator 0 2))))
  (test '() (generator->list (gappend)))
  (test '(15 22 31) (generator->list (gcombine proc 10 g1 g2)))
  (test '(1 3 5 7 9) 
        (generator->list (gfilter odd? (make-range-generator 1 11))))
  (test '(2 4 6 8 10)
        (generator->list (gremove odd? (make-range-generator 1 11))))
  (test '(1 2 3) (generator->list (gtake g 3)))
  (test '(4) (generator->list g))
  (test '(1 2) 
        (generator->list (gtake (make-range-generator 1 3) 3)))
  (test '(1 2 0)
        (generator->list (gtake (make-range-generator 1 3) 3 0)))
  (test '(3 4) 
        (generator->list (gdrop (make-range-generator 1 5) 2)))
  (set! g (make-range-generator 1 5))
  (test '(1 2) (generator->list (gtake-while small? g)))
  (set! g (make-range-generator 1 5))
  (test '(3 4) (generator->list (gdrop-while small? g)))
  (test '() 
        (generator->list (gdrop-while (lambda args #t) (generator 1 2 3))))
  (test '(0.0 1.0 0 2)
        (generator->list (gdelete 1 (generator 0.0 1.0 0 1 2))))
  (test '(0.0 0 2)
        (generator->list (gdelete 1 (generator 0.0 1.0 0 1 2) =)))
  (test '(a c e)
        (generator->list (gindex (list->generator '(a b c d e f))
                                 (list->generator '(0 2 4)))))
  (test '(a d e)
        (generator->list (gselect (list->generator '(a b c d e f))
                                  (list->generator '(#t #f #f #t #t #f)))))
  (test '(1 2 3)
        (generator->list (gdelete-neighbor-dups (generator 1 1 2 2 3 3) =)))
  (test '(1)
        (generator->list (gdelete-neighbor-dups (generator 1 2 3) 
                                                (lambda args #t)))))

(define n 0)

(test-group
  "generator consumers"
  (test '(1 2 3) (generator->list (generator 1 2 3 4 5) 3))
  (test '(5 4 3 2 1) (generator->reverse-list (generator 1 2 3 4 5)))
  (test #(1 2 3 4 5) (generator->vector (generator 1 2 3 4 5))) 
  (test #(1 2 3) (generator->vector (generator 1 2 3 4 5) 3))
  (test "abc" (generator->string (generator #\a #\b #\c)))
  (test 10 (generator-fold + 0 (generator 1 2 3 4)))
  (generator-for-each (lambda v (set! n (apply + v)))
                      (generator 1)
                      (generator 2)
                      (generator 3))
  (test 6 n)
  (test 3 (generator-find (lambda (x) (> x 2)) (make-range-generator 1 5)))
  (test 2 (generator-count odd? (make-range-generator 1 5)))
  (set! g (make-range-generator 2 5))
  (test-assert (generator-any odd? g))
  (test '(4) (generator->list g))
  (set! g (make-range-generator 2 5))
  (test-not (generator-every odd? g))
  (test '(3 4) (generator->list g))
  (test '(#\a #\b #\c)
        (generator-unfold (make-for-each-generator string-for-each
                                                   "abc") unfold)))

(test-exit)

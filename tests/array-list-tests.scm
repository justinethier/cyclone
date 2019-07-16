#|
 | Copyright (c) 2016 John Cowan
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
  (srfi 27)
  (scheme cyclone array-list)
  (cyclone test))

(define test-size 2000)

(test-group
  "array-list/basic"
  (define a (make-array-list 10))
  (test 0 (array-list-length a))
  (test-assert (array-list-empty? a))
  (array-list-insert! a 5)
  (test 1 (array-list-length a))
  (test-not (array-list-empty? a))
  (test 5 (array-list-ref a 0))
  (array-list-insert! a 0 12)
  (test 2 (array-list-length a))
  (test-not (array-list-empty? a))
  (test 12 (array-list-ref a 0))
  (test 5 (array-list-ref a 1))
  (array-list-insert! a 1 15)
  (test 3 (array-list-length a))
  (test-not (array-list-empty? a))
  (test 12 (array-list-ref a 0))
  (test 15 (array-list-ref a 1))
  (test 5 (array-list-ref a 2))
  (array-list-set! a 2 45)
  (test 3 (array-list-length a))
  (test-not (array-list-empty? a))
  (test 12 (array-list-ref a 0))
  (test 15 (array-list-ref a 1))
  (test 45 (array-list-ref a 2))
  (array-list-delete! a)
  (test 2 (array-list-length a))
  (test-not (array-list-empty? a))
  (test 12 (array-list-ref a 0))
  (test 15 (array-list-ref a 1))
  (array-list-delete! a 0)
  (test 1 (array-list-length a))
  (test-not (array-list-empty? a))
  (test 15 (array-list-ref a 0)))

(test-group
  "array-list/hammer"
  (define al (make-array-list 16))
  (do
    ((i 0 (+ i 1)))
    ((= i test-size))
    (test i (array-list-length al))
    (array-list-insert! al i)
    (test i (array-list-ref al i)))
  (do
    ((i test-size (- i 1)))
    ((= i 2))
    (let* ((r (random-integer (- (array-list-length al) 2)))
           (x (array-list-ref al r)))
      (test i (array-list-length al))
      (array-list-delete! al r)
      (test-not (= x (array-list-ref al r))))))

(test-exit)

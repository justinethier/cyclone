|
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

(define-record-type
  <array-list>
  (new-al x y z)
  array-list?
  (x store set-store!)
  (y size set-size!)
  (z capacity set-capacity!))

(define-syntax with-array-list
  (syntax-rules ()
    ((with-array-list (store-bind size-bind capacity-bind) arr-list body ...)
     (let ((store-bind (store arr-list))
           (size-bind (size arr-list))
           (capacity-bind (capacity arr-list)))
       (begin
         body ...)))))

(define min-size 16)

(define (check-positive-integer x)
  (unless (and (integer? x)
               (positive? x))
    (error "Expected positive integer argument.")))

(define (check-in-bounds lo hi x)
  (unless (and (integer? x)
               (>= x lo)
               (< x hi))
    (error "Expected positive integer, in bounds.")))

(define (make-array-list n)
  (check-positive-integer n)
  (let ((len (max min-size n)))
    (new-al (make-vector len) 0 len)))

(define (refit! al)
  (with-array-list 
    (s len cap) al
    (define (resize! x)
      (let ((new-al (make-vector x)))
        (vector-copy! new-al 0 s 0 len)
        (set-store! al new-al)
        (set-capacity! al (vector-length new-al))))
    (cond
      ((= len cap) (resize! (* 2 cap)))
      ((and (> cap min-size)
            (< len (/ cap 4))) (resize! (/ cap 2))))))

(define array-list-insert!
  (case-lambda
    ((al x) 
     (refit! al)
     (vector-set! (store al) (size al) x)
     (set-size! al (+ 1 (size al))))
    ((al i x)
     (check-in-bounds 0 (+ 1 (size al)) i)
     (if (= i (size al))
         (array-list-insert! al x)
         (begin
           (refit! al)
           (vector-copy! (store al) (+ 1 i) (store al) i (size al))
           (vector-set! (store al) i x)
           (set-size! al (+ 1 (size al))))))))

(define array-list-delete!
  (case-lambda
    ((al) 
     (refit! al)
     (vector-set! (store al) (- (size al) 1) #f)
     (set-size! al (- (size al) 1)))
    ((al i)
     (check-in-bounds 0 (size al) i)
     (if (= i (- (size al) 1))
         (array-list-delete! al)
         (begin
           (refit! al)
           (vector-copy! (store al) i (store al) (+ 1 i) (size al))
           (set-size! al (- (size al) 1)))))))`

(define (array-list . items)
  (define len (length items))
  (define al (make-array-list len))
  (for-each (lambda (x)
              (array-list-insert! al x)))
  al)

(define (array-list-empty? al)
  (zero? (size al)))

(define (array-list-ref al i)
  (vector-ref (store al) i))

(define (array-list-length al)
  (size al))

(define (array-list-set! al i x)
  (vector-set! (store al) i x))

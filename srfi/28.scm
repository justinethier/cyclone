#|
 | Copyright (C) 2002 Scott Miller
 | Copyright (C) 2017 Koz Ross
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

(define (format format-string . objects)
  (define buffer (open-output-string))
  (define (fmt-rec format-list objs)
    (define (escape-write how)
      (if (null? objs)
          (error "No value for escape sequence")
          (begin
            (how (car objs) buffer)
            (fmt-rec (cddr format-list) (cdr objs)))))
    (define (raw-write what next)
      (write-char what buffer)
      (fmt-rec (next format-list) objs))
    (cond
      ((null? format-list) (get-output-string buffer))
      ((char=? (car format-list) #\~) (if (null? (cdr format-list))
                                          (error "Incomplete escape sequence")
                                          (case (cadr format-list)
                                            ((#\a) (escape-write display))
                                            ((#\s) (escape-write write))
                                            ((#\%) (raw-write #\newline cddr)) 
                                            ((#\~) (raw-write #\~ cddr))
                                            (else (error "Unrecognized escape sequence")))))
      (else (raw-write (car format-list) cdr)))) 
  (fmt-rec (string->list format-string) objects))

(define-library (example life)
  (export life)
  (import (scheme base) ;TODO: (except (scheme base) set!)
          (scheme write)
          (example grid))
  (begin
    (define (life-count grid i j)
      (define (count i j)
        (if (ref grid i j) 1 0))
      (+ (count (- i 1) (- j 1))
         (count (- i 1) j)
         (count (- i 1) (+ j 1))
         (count i (- j 1))
         (count i (+ j 1))
         (count (+ i 1) (- j 1))
         (count (+ i 1) j)
         (count (+ i 1) (+ j 1))))
    (define (life-alive? grid i j)
      (case (life-count grid i j)
            ((3) #t)
            ((2) (ref grid i j))
            (else #f)))
;    (define (clear-vt100)
;        (display
;            (string 
;                (integer->char #x1B) 
;                #\[
;                #\H
;                (integer->char #x1B)
;                #\[
;                #\J)))
    (define (life-print grid iteration)
      ;(clear-vt100)
      (let ((img (png:init 100 100))
            (path (string-append "life-" (number->string iteration) ".png")))
        (each grid
          (lambda (i j v)
            ;(display (if v "*" " "))
            ;(when (= j (- (cols grid) 1))
            ;  (newline))
            (if v
              (png:set! i j 0 250 0))
          ))
        (png:save img path)
        (png:free img)
    ))
    (define (life grid iterations)
      (do ((i 0 (+ i 1))
           (grid0 grid grid1)
           (grid1 (make (rows grid) (cols grid))
                  grid0))
          ((= i iterations))
        (each grid0
          (lambda (j k v)
            (let ((a (life-alive? grid0 j k)))
              (put! grid1 j k a))))
              ;(set! grid1 j k a))))
        (life-print grid1 i)))
    (define-c png:init
      "(void *data, int argc, closure _, object width, object height)"
      " RGBBitmap *img = malloc(sizeof(RGBBitmap)); 
        make_c_opaque(opq, (void *)img);
        bitmap_init(img, (int)(unbox_number(width)), (int)(unbox_number(height)));
        return_closcall1(data, k, &opq);
      ")
    (define-c png:free
      "(void *data, int argc, closure _, object opq)"
      " RGBBitmap *img = opaque_ptr(opq);
        free(img->pixels);
        free(img);
        return_closcall1(data, k, boolean_t);
      ")
    (define-c png:set!
      "(void *data, int argc, closure _, object opq, object x, object y, object r, object g, object b)"
      " RGBBitmap *img = opaque_ptr(opq);
        bitmap_set(img, 
          ((int)(unbox_number(x))),
          ((int)(unbox_number(y))),
          ((int)(unbox_number(r))),
          ((int)(unbox_number(g))),
          ((int)(unbox_number(b))));
        return_closcall1(data, k, boolean_t); ")
    (define-c png:save
      "(void *data, int argc, closure _, object opq, object path)"
      " RGBBitmap *img = opaque_ptr(opq);
        bitmap_save_to_png(img, string_str(path));
        return_closcall1(data, k, boolean_t);
      ")
))

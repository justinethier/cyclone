(define-library (scheme inexact)
  (export 
    sin
  )
  (begin
    (define-c sin
      "(void *data, int argc, closure _, object k, object z)"
      " make_double(d, 0.0);
        Cyc_check_num(data, z);
        if (type_of(z) == integer_tag) {
          d.value = sin(((integer_type *)z)->value);
        } else {
          d.value = sin(((double_type *)z)->value);
        }
        return_closcall1(data, k, &d); ")
))

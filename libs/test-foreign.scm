;; Unit tests for the (cyclone foreign) module.
;;
(import 
  (scheme base) 
  (scheme write)
  (cyclone test)
  (cyclone foreign)
  (scheme cyclone util)
  (scheme cyclone pretty-print)
  )

(define *my-global* #f)

(test-group "foreign value"
  (test 3 (c-value "1 + 2" 'integer))
)

(test-group "foreign code"
  (test #f *my-global*)
  (c-code 
    "printf(\"test %d %d \\n\", 1, 2);"
    "printf(\"test %d %d %d\\n\", 1, 2, 3);"
    "__glo__85my_91global_85 = boolean_t;")
  (test #t *my-global*)
  (set! *my-global* 1)
  (test 1 *my-global*)
)

;; Must be top-level
(c-define scm-strlen int "strlen" string)
(c-define scm-strlend double "strlen" string)

(test-group "foreign lambda"
  (test 15 (scm-strlen "testing 1, 2, 3"))
  (test 15.0 (scm-strlend "testing 1, 2, 3"))
)
(test-exit)

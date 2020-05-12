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

(c-define-type my-integer integer)
(c-define-type my-integer-as-string integer string->number number->string)

(test-group "foreign value"
  (test 3 (c-value "1 + 2" integer))
  (test 4 (c-value "2 + 2" my-integer))
  (test "4" (c-value "2 + 2" my-integer-as-string))
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

;TODO: support custom types (arg and ret) for c-define.
;      Also need to be able to support arg/ret convert optional type arguments
;      Would need to generate scheme wrappers to handle these conversions

(c-define scm-strlen my-integer "strlen" string)
(c-define scm-strlen-str my-integer-as-string "strlen" string)
;(c-define scm-strlen "int" "strlen" string)
(c-define scm-strlend double "strlen" string)

(test-group "foreign lambda"
  (test 15 (scm-strlen "testing 1, 2, 3"))
  (test 15.0 (scm-strlend "testing 1, 2, 3"))
  (test "15" (scm-strlen-str "testing 1, 2, 3"))
)
(test-exit)

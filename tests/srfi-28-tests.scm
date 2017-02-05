(import
  (scheme base)
  (srfi 28)
  (scheme cyclone test))

(test-group
  "format"
  (test "Hello, World!" (format "Hello, ~a" "World!"))
  (test "Error, list is too short: (one \"two\" 3)
"
        (format "Error, list is too short: ~s~%" '(one "two" 3))))

(test-exit)

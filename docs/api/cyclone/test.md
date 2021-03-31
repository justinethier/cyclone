# Test Library

The `(cyclone test)` library contains a testing framework ported from `(chibi test)` which in turn was ported from CHICKEN.

## Exception Utilities
- [`warning`](#warning)

## Test Interface
- [`test`](#test)

- [`test-equal`](#test-equal)
- [`test-error`](#test-error)
- [`test-assert`](#test-assert)
- [`test-not`](#test-not)
- [`test-values`](#test-values)
- [`test-begin`](#test-begin)
- [`test-end`](#test-end)
- [`test-propagate-info`](#test-propagate-info)
- [`test-vars`](#test-vars)
- [`test-run`](#test-run)
- [`test-exit`](#test-exit)

## Group Interface
- [`test-group`](#test-group)
- [`test-group-inc!`](#test-group-inc)

## Utilities
- [`test-syntax-error`](#test-syntax-error)

## Parameters
- [`current-test-group`](#current-test-group)
- [`current-test-verbosity`](#current-test-verbosity)
- [`current-test-applier`](#current-test-applier)
- [`current-test-handler`](#current-test-handler)
- [`current-test-skipper`](#current-test-skipper)
- [`current-test-group-reporter`](#current-test-group-reporter)
- [`current-test-epsilon`](#current-test-epsilon)
- [`current-test-comparator`](#current-test-comparator)
- [`test-failure-count`](#test-failure-count)

# warning

    (warning msg . args)

# test

*Syntax*

    (test [name] expect expr)

Evaluate `expr` and check that it is `equal?` to `expect`. 

`name` is used in reporting, and defaults to a printed summary of `expr`.

# test-equal

;;> \macro{(test-equal equal [name] expect expr)}

;;> Equivalent to test, using \var{equal} for comparison instead of
;;> \scheme{equal?}.

# test-error

;;> \macro{(test-error [name] expr)}

;;> Like \scheme{test} but evaluates \var{expr} and checks that it
;;> raises an error.

# test-assert

;;> \macro{(test-assert [name] expr)}

;;> Like \scheme{test} but evaluates \var{expr} and checks that it's true.

# test-not

;;> \macro{(test-not [name] expr)}

;;> Like \scheme{test} but evaluates \var{expr} and checks that it's false.

# test-values

;;> \macro{(test-values [name] expect expr)}

;;> Like \scheme{test} but \var{expect} and \var{expr} can both
;;> return multiple values.

# test-begin

# test-end

# test-syntax-error

# test-propagate-info

# test-vars

# test-run

# test-exit

    (test-exit)

Exits with a failure status if any tests have failed, and a successful status otherwise.

# test-group

    (test-group body ...)

Wraps `body` as a single test group, which can be filtered and summarized separately.

# current-test-group

# current-test-verbosity

# current-test-applier

# current-test-handler

# current-test-skipper

# current-test-group-reporter

# test-failure-count

# current-test-epsilon

# current-test-comparator

# test-group-inc!


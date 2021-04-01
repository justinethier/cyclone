# Test Library

The `(cyclone test)` library contains a testing framework ported from `(chibi test)` which in turn was ported from CHICKEN.

## Testing
- [`test`](#test)
- [`test-equal`](#test-equal)
- [`test-error`](#test-error)
- [`test-assert`](#test-assert)
- [`test-not`](#test-not)
- [`test-values`](#test-values)
- [`test-propagate-info`](#test-propagate-info)
- [`test-run`](#test-run)

## Test Groups
- [`test-group`](#test-group)
- [`test-begin`](#test-begin)
- [`test-end`](#test-end)
- [`test-exit`](#test-exit)

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

# test

*Syntax*

    (test [name] expect expr)

Evaluate `expr` and check that it is `equal?` to `expect`. 

`name` is used in reporting, and defaults to a printed summary of `expr`.

# test-equal

*Syntax*

    (test-equal equal [name] expect expr)

Equivalent to test, using `equal` for comparison instead of `equal?`.

# test-error

*Syntax*

    (test-error [name] expr)

Like `test` but evaluates `expr` and checks that it raises an error.

# test-assert

*Syntax*

    (test-assert [name] expr)

Like `test` but evaluates `expr` and checks that it's true.

# test-not

*Syntax*

    (test-not [name] expr)

Like `test` but evaluates `expr` and checks that it's false.

# test-values

*Syntax*

    (test-values [name] expect expr)

Like `test` but `expect` and `expr` can both return multiple values.

# test-begin

    (test-begin)
    (test-begin name)

Begin testing a new group until the closing `(test-end)`.

# test-end

    (test-end)
    (test-end name)

Ends testing group introduced with `(test-begin)`, and summarizes the results.

# test-propagate-info

    (test-propagate-info name expect expr info)

Low-level macro to pass alist info to the underlying `test-run`.

# test-run

    (test-run expect expr info)

The procedural interface to testing. `expect` and `expr` should be thunks, and `info` is an alist of properties used in test reporting.

# test-exit

    (test-exit)

Exits with a failure status if any tests have failed, and a successful status otherwise.

# test-group

    (test-group body ...)

Wraps `body` as a single test group, which can be filtered and summarized separately.

# current-test-group

The current test group as started by `test-group` or `test-begin`.

# current-test-verbosity

# current-test-applier

# current-test-handler

# current-test-skipper

# current-test-group-reporter

# test-failure-count

# current-test-epsilon

# current-test-comparator


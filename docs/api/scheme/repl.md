# REPL Library

The `(scheme repl)` library provides functions related to Read-Eval-Print-Loops.

For more information see the [R<sup>7</sup>RS Scheme Specification](../../r7rs.pdf).

- [`interaction-environment`](#interaction-environment)
- [`repl`](#repl)

# interaction-environment

    (interaction-environment)

This procedure returns a specifier for a mutable environment that is the same as the one used by Cyclone's built-in REPL.

# repl

    (repl)

Starts an interactive REPL. This is not a standard function provided by R7RS.

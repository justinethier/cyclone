# Pretty Print Library

The `(scheme cyclone pretty-print)` library provides a pretty-printer for code formatting.

- [`pretty-print`](#pretty-print)

# pretty-print
    (pretty-print obj)
    (pretty-print obj port)
Outputs object to the given output port, or the current output port if none is given. The output is automatically indented just like it would be in a source code file, to make it easier to read.

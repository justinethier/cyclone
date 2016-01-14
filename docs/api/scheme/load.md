# Load Library

The `(scheme load)` library exports procedures for loading Scheme expressions from files.

- [`load`](#load)

#load

    (load filename)

The `load` procedure reads expressions and definitions from the file and evaluates them sequentially.

Note that when using `load` in a compiled program, the file will be processed at runtime using `eval`. The file contents will not be compiled.

# Next Release - Date TBD

Features:

- Added `parameterize` from section 4.2.6 of R7RS to the `(scheme base)` library.

Bug Fixes:

- Thanks to Mark Meyer, identified and fixed several segfaults in `write` and `display`.
- Updated `write` to display escaped character sequences (EG: `\t`) instead of literal characters.
- Prevent C compilation errors when building a program that only contains basic primitives or a constant at the top level.
- Fixed the compiler to allow application of a function that can take any number of arguments. For example:

         ((lambda test (write test)) 1 2 3 4)

- Updated `eval` to handle functions that take an optional number of arguments.

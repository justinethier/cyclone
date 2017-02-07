# Next Release (Tentatively 0.3.4) - TBD

Features

- Koz Ross added implementations of SRFI 28 and 60.
- Allow a program to have more than one `import` declaration. A program can now also use `cond-expand` to selectively expand `import` declarations.
- Added the `-A` and `-I` compiler options from SRFI 138 to `cyclone`:

  > `-A directory` 
  >
  > Append directory to the list of directories that are searched in order to locate imported libraries.
  >
  > `-I directory`
  >
  > Prepend directory to the list of directories that are searched in order to locate imported libraries.

- Added the `-CP`, `-CE`, and `-CL` compiler options to allow passing arbitrary flags to the C compiler:

 > `-CP directory`
 >
 > Specify a custom command line for the C compiler to compile a program module. See Makefile.config for an example of how to construct such a command line.
 >
 > `-CE directory`
 >
 > Specify a custom command line for the C compiler to compile an executable.
 >
 > `-CL directory`
 >
 > Specify a custom command line for the C compiler to compile a library module.

- Updated the garbage collector to enhance performance for programs with a high allocation rate, and to scale better to multiple concurrent threads.
- Improved error handling by `display` and `write`.
- Removed the `make_int` C macro which was deprecated and could cause problems when used in FFI functions.

Bug Fixes

- Thanks to Koz Ross, `equal?` has been updated to check bytevectors for deep equality. 
- Prevent crashes when allocating large bytevectors.
- Display characters such as `#\space` correctly when output via `write`.
- Thanks to Seth Alves, removed unnecessary include of `ck_string.h` which is not provided in older versions of `libck`.

# 0.3.3 - December 19, 2016

Features:

- Added SRFI 113 - sets and bags.  
- Improved performance by more aggressively inlining primitives that work with immutable objects, such as the numeric arithmetic and comparison functions.
- Allow the reader to recognize `#true` and `#false`.

Bug Fixes

- Prevent C compilation errors when compiling strings with special characters in them such as `#\tab`, `#\alarm`, and `#\return`.
- Do not generate invalid C code when compiling `+` or `*` without any arguments.
- Handle compilation of variable-argument anonymous lambdas. For example: `((lambda x x) 3 5)`.

# 0.3.2 - November 22, 2016

Features:

- Recognize escaped vertical line and hex scalar value characters when reading a string.
- Added `current-thread` to SRFI 18.
- Added the `include` and `include-ci` macros.

Bug Fixes

- Fixed a crash when running `cyclone` on 32-bit platforms.

# 0.3.1 - November 20, 2016

Features:

- Thanks to ecraven, added R7RS function `exact-integer-sqrt` to `(scheme base)`.
- Allow the reader to recognize `+inf.0`, `-inf.0`, `+nan.0`, and `-nan.0`.
- Allow `cond-expand` to test for whether a library exists using the form `(library {library name})`.
- Added command line option `-O` to set the optimization level. For now there is `-O0` to disable optimizations and the default setting to enable them. Going forward there will be more fine-grained control.
- Reduce size of compiled code by inlining constant values. This reduced the code size of various cyclone libraries by approximately 33%. Added other inlining as well to slightly improve performance.
- Increased allocation speed when objects are moved to the heap during a major GC.

Bug Fixes

- Only throw a divide by zero error for integer division. Floating point divide by zero is allowed and evaluates to `nan`.
- Modified SRFI 106 to be smarter about compiling `AI_V4MAPPED` and `AI_ALL`, which are not defined on OpenBSD.
- Fixed the `member` functions to always return the list when a value is found, instead of `#t`.
- Fixed `string->number` to return `#f` if the string cannot be converted.
- Fixed a bug where the optimizer would sometimes generate incorrect code when a mutating primitive (EG: `set-car!`) was passed an expression rather than an identifier as the variable argument.
- Prevent the runtime from crashing when a non-numeric value type is passed to a numeric function.

# 0.3 - October 22, 2016

Features:

- Improve performance by inlining numeric arithmetic and comparison operations.
- Reverted `assq`, `assv`, `memq`, and `memv` back to primitives for improved performance. In addition the compiler was modified to allow for more efficient compilation of `assoc` and `member`.
- Improved library support to recognize all of the import set forms: `only`, `except`, `prefix`, and `rename`.
- Allow explicit renaming macros to be declared interactively. This is the first limited support for calling `define-syntax` from `eval`. 
- Added the `get-environment-variables` function from R7RS.
- Added support for the following SRFI's:

     - SRFI 106: A basic socket interface
     - SRFI 128: Comparators

Bug Fixes

- Improved macro hygiene support to properly rename temporary variables in the `let-values` macro.
- Improve output of `error` by using `write` to output objects as they are represented in memory.
- Check for duplicate lambda parameters during compilation.
- Fixed an error that was being raised when calling `(random-source-randomize! default-random-source)` when using SRFI 27.
- Raise an error if `list->string` encounters a list element that is not a character.

# 0.2 - September 7, 2016

Features:

- Added the following libraries from R7RS-large red edition (data structures):
    - SRFI 1 list library
    - SRFI 133 vector library
    - SRFI 111 boxes
    - SRFI 117 mutable queues
    - SRFI 132 sorting library
- Added SRFI 2, `and-let*`
- Added `parameterize` from section 4.2.6 of R7RS to the `(scheme base)` library.
- Added ` let-values` and ` let*-values` to `(scheme base)`.
- Added `infinite?`, `finite?`, and `nan?` to `(scheme inexact)`.
- Added string ports to `(scheme base)` - `open-input-string`, `open-output-string`, and `get-output-string`.
- Added bytevector ports to `(scheme base)` - `get-output-bytevector`, `open-input-bytevector`, and `open-output-bytevector`.
- Modified `cyclone` to also search the current working directory for built-in headers and libraries.

Bug Fixes:

- Thanks to Mark Meyer, identified and fixed several segfaults in `write` and `display`. 
- Updated `write` to display escaped character sequences (EG: `\t`) instead of literal characters.
- Prevent C compilation errors when building a program that only contains basic primitives or a constant at the top level.
- Fixed the compiler to allow application of a function that can take any number of arguments. For example:

         ((lambda test (write test)) 1 2 3 4)

- Updated `eval` to handle functions that take an optional number of arguments.
- Updated `number->string` to omit leading zeros for binary conversions.
- Fixed `apply` to use the proper semantics when receiving more than one data argument.
- Changed the `assoc` and `member` functions to accept an optional comparison function, and modified both families of functions to be native Scheme functions instead of C primitives.
- Allow libraries to contain multiple `import`, `begin`, and `export` sections.

# Changelog

## 0.6.1 - August 22, 2017

Bug Fixes

- Fixed regression: there was the possibility of crashing when calling `close-port`.

## 0.6 - August 21, 2017

Features

- Added a faster version of `read`.
- Added SRFI 143 - Fixnums.

Bug Fixes

- Prevent `remainder` from crashing the runtime due to divide by zero.
- Avoid printing an unnecessary colon after certain error messages.

## 0.5.4 - August 3, 2017

Features

- Allow the `-A` and `-I` options to `icyc` for specifying additional library import directories.
- Perform constant folding as part of the CPS optimization phase.
- Statically allocate closures that do not contain any free variables, to avoid unnecessary heap allocations.

Bug Fixes

- Updated `string->number` to return `#f` for all bases when the conversion fails. Previously bases other than ten would return `0` instead.

## 0.5.3 - June 29, 2017

Features

- On Arthur Maciel's suggestion, modified Cyclone to always check its system folder for a library if an import is not found. In conjunction with this, the `CYCLONE_LIBRARY_PATH` environment variable may be set to force Cyclone to look in a specific place other than the system folder for libraries.

- Decrease minor GC time by only tracing globals when one of them has changed.

- Added the `thread-join!` function to `(srfi 18)`.

## 0.5.2 - June 5, 2017

Bug Fixes

- Fixed a bug in `for-each` that was causing `length` to be executed during each iteration.

## 0.5.1 - May 24, 2017

The main focus of `0.5.1` is performance but this is also the first release to provide installation instructions for both Windows and Mac.

Features

- Thanks to Kashyap for adding build instructions for Windows (MSYS) and Mac!

- Allow `define-c` function definitions to optionally provide an additional non-CPS form of the function. This form is more efficient and will be used by compiled code whenever possible.

- Improved the compiler's CPS optimization phase to eliminate more unnecessary function calls. 

- Modified the GC to allow a given number of "huge" allocations to trigger GC. Previously GC was only triggered when smaller heap regions were below a certain percentage of free memory.

- Compiled code now directly accesses boxed mutable variables instead of using a wrapper function.

- Added command line options to `icyc` for evaluating an S-expression from the command line and for running as a script without the Cyclone banner text.

Bug Fixes

- Prevent potential memory corruption when working with large vectors that cannot be allocated on the stack.

## 0.5 - April 14, 2017

Features

- Cyclone now has support in the interpreter for loading libraries via `import`. This is probably the most important change in this release and allows `icyc` to be used to its full potential.
- Store parameter objects in such a way that changes to a parameter object do not affect other threads that use the same parameter object.

    The specific requirement from R<sup>7</sup>RS is:

    > `parameterize` must not change the associated values of any parameters in any thread other than the current thread and threads created inside the `parameterize` body.

- Added bignum support to SRFI 60 - integers as bits.
- Normalize the result of `string->number` such that bignums are only returned if the result truly is a bignum.
- Allow Cyclone to find `(cyclone)` prefixed libraries installed in Cyclone's system folder.
- Allow a library to export identifiers that are exported by another library. Previously a library could only export identifiers that it defined directly.
- Raise an error if an unknown identifier is found in a library's `export` list.
- Aric Belsito removed a hardcoded call to `cyclone` from the Makefile when building `generate-c`. Thanks!
- Allow `cyclone` to inline character comparison functions (`char=?`, etc) when only two arguments are present, for a significant speed improvement.

Bug Fixes

- Prevent exception handlers from being accidentally collected when the collector cooperates on behalf of a mutator.
- Fixed a regression where `string->number` returns `0` on bad input instead of `#f`.

## 0.4 - March 9, 2017

- Added a new bignum type to support exact integers of practically unlimited size.
- As part of the bignum changes, modified the code for exact integer arithmetic to detect overflow.
- Added documentation for the [C API](http://justinethier.github.io/cyclone/c-api/modules.html).

## 0.3.4 - February 10, 2017

Features

- Koz Ross added implementations of the following SRFI's:

     - SRFI 28 - Basic format strings
     - SRFI 60 - Integers as bits
     - SRFI 121 - Generators

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

 > `-CP cc-commands`
 >
 > Specify a custom command line for the C compiler to compile a program module. See Makefile.config for an example of how to construct such a command line.
 >
 > `-CE cc-commands`
 >
 > Specify a custom command line for the C compiler to compile an executable.
 >
 > `-CL cc-commands`
 >
 > Specify a custom command line for the C compiler to compile a library module.

- Updated the garbage collector to enhance performance for programs with a high allocation rate, and to scale better to multiple concurrent threads.
- Improved error handling by `display` and `write`.
- Removed the `make_int` C macro which was deprecated and could cause problems when used in FFI functions.
- Added a `with-handler` exception handler form based on the syntax from Bigloo.

Bug Fixes

- Thanks to Koz Ross, `equal?` has been updated to check bytevectors for deep equality. 
- Prevent crashes when allocating large bytevectors.
- Display characters such as `#\space` correctly when output via `write`.
- Thanks to Seth Alves, removed unnecessary include of `ck_string.h` which is not provided in older versions of `libck`.

## 0.3.3 - December 19, 2016

Features:

- Added SRFI 113 - sets and bags.  
- Improved performance by more aggressively inlining primitives that work with immutable objects, such as the numeric arithmetic and comparison functions.
- Allow the reader to recognize `#true` and `#false`.

Bug Fixes

- Prevent C compilation errors when compiling strings with special characters in them such as `#\tab`, `#\alarm`, and `#\return`.
- Do not generate invalid C code when compiling `+` or `*` without any arguments.
- Handle compilation of variable-argument anonymous lambdas. For example: `((lambda x x) 3 5)`.

## 0.3.2 - November 22, 2016

Features:

- Recognize escaped vertical line and hex scalar value characters when reading a string.
- Added `current-thread` to SRFI 18.
- Added the `include` and `include-ci` macros.

Bug Fixes

- Fixed a crash when running `cyclone` on 32-bit platforms.

## 0.3.1 - November 20, 2016

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

## 0.3 - October 22, 2016

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

## 0.2 - September 7, 2016

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

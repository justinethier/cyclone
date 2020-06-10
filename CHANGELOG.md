# Changelog

## 0.18 - June 8, 2020

The main focus of this release is Arthur Maciel's Cyclone-Winds package manager, the official package manager for Cyclone Scheme! The `cyclone-winds` binary is built and installed as part of our [bootstrap](https://github.com/justinethier/cyclone-bootstrap) installation. Alternatively it can be built and installed separately if you already have a copy of Cyclone installed, though version 0.18 or higher is required.

Please visit the [Cyclone-Winds] https://github.com/cyclone-scheme/cyclone-winds) site for more information.

Features

- Added the `(cyclone foreign)` library to make it easier to integrate with C code using the FFI.
- Arthur Maciel improved how `icyc` handles the "-p" flag, to now read/evaluate all expressions passed.

Bug Fixes

- Added a void type and a new supporting function `(void)`. A void object is also now returned in situations where the return value is undefined: `for-each` and `if` expressions with out an else clause.
- Allow `open-binary-input-file` and `open-binary-output-file` to be accessible via the REPL.
- Fix `bytevector?` predicate which was accidentally aliased to `vector?`.
- Fix `list-copy` to return a non-list object instead of raising an error, per R7RS.
- Fixed `eqv?` to use R7RS semantics to ensure equality of different instances of the same numeric value. The function was previously just an alias of `eq?`.
- Support two-argument version of `atan`.
- Support `start` and `end` arguments to `write-string`.
- Updated the string comparison procedures (EG: `string=?`) to support more than two arguments.

## 0.17 - April 6, 2020

Bug Fixes

- Modified `binary-port?` and `textual-port?` to correctly differentiate between binary and textual ports.

## 0.16 - March 11, 2020

Features

- Updated the C API to optionally allow Cyclone's GC to free memory pointed to by an Opaque object. 

  For example:

      my_c_obj = calloc(1, sizeof(*my_c_obj_type));
      make_c_opaque(opq, my_c_obj);
      opaque_collect_ptr(&opq) = 1; // Cyclone's GC will free this memory

Bug Fixes

- Fixed a bug in `read-bytevector` where an extra byte could be introduced when reading multiple chunks of data.
- Fixed a bug where variables defined within `define-syntax` and `let-syntax` are reported as unbound by the compiler.

## 0.15 - February 26, 2020

Features

- Added the `c-linker-options` library directive to allow a library to specify options to pass to the C linker. This prevent compiler errors when linking the final executable.
- Added the `-CLNK` compiler option to specify a custom command to provide as a linker option. For example: `"-lcurl"`.

Bug Fixes

- Fixed the division (`/`) operator to ensure exact arguments will produce inexact results rather than incorrect fixnum results. The example given in R7RS is that `(/ 3 4)` must not return a mathematically incorrect result of `0`. This was previously broken in compiled code though `icyc` would return the correct result.

## 0.14 - February 11, 2020

Cyclone now automatically relocates any stack objects when performing a mutation. This prevents a whole range of race conditions that had previously been possible in multithreaded application code. And since this work is done by the Cyclone runtime no special code needs to be added to your applications.

Special thanks to Daniel Mendler, whose discussions were the inspiration for these changes.

Some background: 

There was a long-standing issue where a mutation (via `set-car!`, `vector-set!`, `set!`, etc) could allow a global object on the heap to reference objects on a thread's local stack. This is problematic because threads periodically relocate objects from their stack, and for performance reasons these objects are moved without any coordination between threads. Thus it is critical that objects on the stack are only used by the thread that owns them.

In the past we provided functions such as `make-shared` that could be called from application code to guarantee safety. However, this approach is error-prone and asks too much of anyone using Cyclone for multithreaded development. The proper solution is for Cyclone to avoid this situation in the first place.

Other Features

 - Added `CYC_HIGH_RES_TIMERS` to the runtime code to allow logging of timer information for the GC. Note this can be passed to the C compiler via the `-D` option.

## 0.12 - January 17, 2020

Features

- When compiling a program with `cyclone` any library dependencies are automatically built as well.
  Note:
  - A library will only be built if an object file does not exist for it or if any of the source files are newer than the object file.
  - Only "local" libraries will be built. Cyclone will not automatically attempt to build libraries in system directories.
  - This capability can be disabled by the `-no-batch` option.
- Added `define-values` from R7RS.

Bug Fixes
- Prevent compilation errors when optimizations are disabled via `-O0`.
- The compiler now omits the continuation argument when printing the expected number of arguments to a function, to avoid confusing error messages.

## 0.11.8 - December 30, 2019

Features

- Added `read-bytevector`, `read-bytevector!`, and `write-bytevector` I/O functions from R7RS.

Bug Fixes

- Fixed Windows build using MSYS2 and setup a continuous integration job for this platform to prevent breaking this build in the future.
- Tweaked build flags to prevent spamming of compiler warnings when using Clang.
- Modified the compiler to ensure that a variable is not assigned to itself in the generated C code.

## 0.11.7 - December 5, 2019

Bug Fixes

- Fixed an issue with the library path when building on Mac OS.

## 0.11.6 - December 2, 2019

Features

- Faster record type constructors.
- During compilation, validate the number of arguments passed to local function calls.
- Added the `-use-unsafe-prims` compiler option to generate faster code for certain primitives by eliminating runtime safety checks.

Bug Fixes

- Fixed `integer?` such that if `x` is an inexact real number, then `(integer? x)` is true if and only if `(= x (round x))`, per R7RS.
- @extrasharp fixed issues with definitions in `(scheme cxr)`.
- @nymacro fixed a bug in the bootstrap Makefile that prevented builds from working on FreeBSD.

Internals

 - When including an internal `.scm` file used by the compiler, check the current directory before the system directory. This makes it easier to work on certain compiler modules.

## 0.11.5 - October 30, 2019

Features

 - Added support for LibTomMath 1.2.0 and integrated that library into our source tree to avoid users having to install the library as a separate dependency.
 - Added the `-no-call-history` flag to potentially allow for faster executables at the expense of call history.

Bug Fixes

- Fix `read-u8`, `peek-u8`, and `write-u8` to work with integers (bytes) instead of characters.

## 0.11.4 - September 27, 2019

Bug Fixes

- Modified the minor GC write barrier to only track mutations where a heap variable is modified to point to an object on the stack. This significantly improves performance of mutation operations: `set!`, `set-car!`, etc.
- Fixed an issue with `make-string` and UTF-8 chars. Thanks to Lassi Kortela for the bug report!
- Added `open-binary-input-file` and `open-binary-output-file` from R7RS.
- Validate the number of arguments passed to `if` expressions.
- Raise a useful error instead of aborting the whole program (!) when apply attempts to execute an object of the wrong type.
- Better handling of edge cases where an object of the wrong type is executed instead of a closure. Previously there were cases where this would cause the runtime to crash.

## 0.11.3 - August 14, 2019

Features

- Arthur Maciel added instructions for building Cyclone on FreeBSD.
- Added support for delays and promises to `(cyclone concurrent)`. Note functions/macros for both types of objects are prefixed with `shared-` to differentiate them from R7RS definitions from `(scheme lazy)`.
- Added platform (linux, bsd, etc) to the list of emitted by `(features)` and to the features recognized by `cond-expand`.
- Optimize compilation of `vector` for five arguments or less, and compilation of `map` / `for-each` with two list arguments.

Bug Fixes

- Fixed the MSYS2 build script and instructions, so it is possible to build on Windows again!
- Modified `(srfi 27)` to use the timer to seed the pseudorandom number generator.
- Fixed `exit` to return the appropriate status code when a boolean is passed, per R7RS:
    > If no argument is supplied, or if obj is #t, the exit procedure should communicate to the operating system that the program exited normally. If obj is #f, the exit procedure should communicate to the operating system that the program exited abnormally.
- Enhanced the interpreter (`eval`) to properly support splicing in definitions introduced by `begin`.
- Fixed `define-record-type` so now it works in the interpreter.

## 0.11.2 - July 7, 2019

Features

- Added a new library `(cyclone concurrent)` with support for atomics, futures, shared queues, and thread pools. As well as helpers for immutability and shared objects. Overall the goal is for this library to make it much easier to write multithreaded programs using Cyclone.
- Relocated existing libraries to `(cyclone match)` and `(cyclone test)`.
- Extended `mutex-lock!` to support an optional `timeout` parameter, per the SRFI 18 spec.

Bug Fixes

- Fixed `thread-join!` to wait properly both for running threads and new threads that have not yet been started.

## 0.11.1 - May 25, 2019

Features

- Added support for immutable objects. Any quoted pairs, vectors, bytevectors, or strings will now be flagged as immutable, per R7RS.

Bug Fixes

- Fixed a bug where the compiler would not always validate the number of arguments passed to a locally-defined function.

## 0.11 - April 16, 2019

Features

- During compilation validate the number of arguments passed to locally-defined functions.
- Improve performance of compiled code a bit by inlining code that tracks call history instead of using a dedicated function in the runtime.
- Updated build instructions for Mac, thanks Adam Feuer!

Bug Fixes

- Allow `exit` to return an integer value as the return code to the OS.

## 0.10 - March 28, 2019

Features

- Improve performance in generated C code by allocating data for closures statically where possible, instead of via `alloca`.
- Speed up `case` expressions by using nested `if` expressions instead of the `memv` primitive to evaluate conditions with more than one constant. The nested expressions have better cache locality and also avoid any additional object allocation or initialization.
- Allow passing the `'bin` symbol to `Cyc-installation-dir` to return the location of the installation directory for binaries.

Bug Fixes

- Prevent the possibility of a segmentation fault when passing am improper list to the `member` and `assoc` family of functions.

## 0.9.10 - March 5, 2019

Features

- Faster initialization of objects create via `define-record-type`.
- Generate faster compiled code for calls to `vector` that contain less than five arguments.

## 0.9.9 - February 19, 2019

Bug Fixes

- Fix from Petr Pražák to avoid compilation errors when using newer versions of LibTomMath.
- Avoid cases where bignums are not initialized properly by the runtime and incorrectly retain a value of zero.
- Handle the following edge case from R7RS:

  > If `z` is a complex number, then `(real? z)` is true if and only if `(zero? (imag-part z))` is true. 

## 0.9.8 - February 16, 2019

Features

- Memoize recursive calls to pure functions where possible.

Bug Fixes

- Arthur Maciel fixed a bug in the compiler back-end where a terminating semi-colon would not be emitted in the C code generated for a short program.

Internal Compiler Enhancements

- Arthur Maciel updated the `cgen` module to follow the [Riastradh style rules](https://mumble.net/~campbell/scheme/style.txt) for updating comments. Thanks Arthur!

## 0.9.7 - January 19, 2019

Features

- Faster version of `list?`.
- Faster compilation of large source files.
- Do not inline `member` or `assoc` to avoid looping over the same list multiple times.

Bug Fixes

- Do not inline primitive calls when arguments to the call are mutated in the function body or mutated elsewhere via `set!`.
- Modified generated code for `(inline)` functions to eliminate the possibility of out-of-order execution.
- Fix a bug where beta expansion of an `if` expression can lead to compilation errors.

## 0.9.6 - December 9, 2018

Bug Fixes

- Cleaned up generated code to ensure calls to primitives from functions that are combined are not executed out of order.
- Fixed the `return_copy` macro in the runtime to handle being passed an expression instead of an object.

## 0.9.5 - November 28, 2018

Features

- Improve performance of many internal C runtime functions by declaring them `static`.

## 0.9.4 - November 25, 2018

Compiler Optimizations

- Optimize recursive functions by expressing the recursive calls using C iteration. This optimization is more effective when combined with the others listed below as they collectively increase the chances that a higher-level Scheme loop may be compiled down to a single C function. These C functions can then be "called" repeatedly using a `while` loop which is more efficient at a low level than repeated function calls.
- Combine lambda functions that are only called for side effects.
- Improve inlining of primitives that work with immutable objects.
- Eliminate functions that are only used to define local variables.

Features

- Added a new feature `program` to `cond-expand` that is only defined when compiling a program. This allows, for example, a `.scm` file to contain a section of code that can be used to run unit tests when the file is compiled as a program. The same file can then be used in production to import code into a library. This is similar to using the `__main__` scope in a python program.

Bug Fixes

- Prevent GC segmentation fault on ARM platforms (Raspberry Pi 2).

## 0.9.3 - October 1, 2018

Features

- Allow pretty printing of bytevectors.
- Internal change to the compiler - preserve lambda AST forms throughout compilation process. This should make it easier to implement certain optimizations in future releases.

Bug Fixes

- Fix `input-port?`, `output-port?`, `input-port-open?`, and `output-port-open?` to return `#f` instead of raising an error when a non-port object is passed.
- Fix overflow detection when performing fixnum multiplication to avoid undefined behavior in the C runtime.
- Prevent warnings from the C compiler regarding `shifting a negative signed value is undefined` and `absolute value function 'abs' given an argument of type 'long' but has parameter of type 'int'`.

## 0.9.2 - August 26, 2018

Features

- During CPS optimization allow inlining of operations on global variables that are mutated in other top-level functions.
- Improved loop detection during CPS optimization phase.
- Allow optimizing-out of basic `if` expressions of the form `(if (pred? ...) #t #f)` into `(pred? ...)`.
- Perform slightly faster type checking for string, vector, and bytevector access functions.

## 0.9.1 - August 9, 2018

Bug Fixes

- Fixed a nasty bug where, while a mutator is blocked after calling `set_thread_blocked`, the collector may copy an object the mutator is using from the stack to the heap. In this case we need to ensure the object is not corrupted when it is copied and that we sync the object's fields back up once the mutator is unblocked. Currently this only affects port objects in the runtime. Generally `read` was affected more than other I/O functions.
- Fixed a handful of small garbage collection bugs from the previous round of development.

## 0.9 - July 31, 2018

This release significantly improves garbage collection performance by using [lazy sweeping](docs/Garbage-Collection-Using-Lazy-Sweeping.md).

## 0.8.1 - July 2, 2018

Features

- Improved garbage collector performance for large heaps.
- Generate faster compiled code for:
  - `car`, `cdr`, and most built-in predicates.
  - Calls to `list` that contain less than five arguments.
  - Calls to `map` and `for-each` that only pass a single list.
- Allow optimization of some simple self-recursive functions.
- Allow the optimizer to beta expand a wider range of function calls. 

Bug Fixes

- Fixed a bug where `current-jiffy` was returning total clock time of the process. Such an approach cannot be used to measure time accurately when more than one thread is executing concurrently.
- Prevent the possibility of an infinite loop by not beta expanding recursive function calls.

## 0.8 - May 30, 2018

Features

- Added support for complex numbers.
- When printing intermediate forms during debugging via `-t` Cyclone will now emit less verbose S-expressions for code in CPS form. To support this effort and make debugging easier, added helper functions `ast:ast->sexp`, `ast:sexp->ast`, and `ast:ast->pp-sexp` to `(scheme cyclone ast)`.
- Avoid inlining function calls into named let loops to improve performance of compiled code.

## 0.7.3 - May 7, 2018

Features

- Made several performance improvements to SRFI 69 hash tables, including:
    - Massively improved lookup performance for symbols.
    - Increased the max bound of hash tables to `(2 ^ 30) - 1`.
    - Changed `hash-by-identity` to a high-performance builtin.
- Added a `repl` function to `(scheme repl)` to make it easy to use a REPL from your own code.
- Added basic support for square and curly brackets in place of parentheses.

Bug Fixes

- Fixed an off-by-one error in `read-line` where the function erroneously reported an extra character was read from `stdin`. Thanks to wasamasa for the bug report.
- Fixed a CPS optimization issue where multiple copies of the same lambda are introduced during beta expansion, which causes the optimizer to potentially pick up the wrong value when optimizing-out function calls later on. Thanks to @Chant on Github for providing the report and a test program demonstrating the issue.
- Updated the parser to recognize mnemonic escapes (EG: `\n`, `\a`, etc) and inline hex escapes as part of a symbol.

## 0.7.2 - February 15, 2018

Features

- Added a `(scheme cyclone match)` library based on Alex Shinn's `match.scm` portable hygienic pattern matcher.
- The compiler now emits a faster version of `apply` in cases where only two arguments are received.

Bug Fixes

- Made several improvements to macro hygiene by renaming local variable bindings during expansion. Added a unit test module covering many test cases.
- Fixed many functions including `utf8->string`, `string->utf8`, `bytevector-copy`, `list->vector`, and `list->string` to heap allocate objects that exceed the maximum size for objects on the stack.
- Prevent a compiler error when there is only one argument to `+` or `*`.

## 0.7.1 - December 21, 2017

Features

- Added support for `let-syntax` and `letrec-syntax`.
- Added the `(scheme repl)` library and `interaction-environment` function from R7RS.
- Allow `eval` to recognize `syntax-rules` macros.
- Added single-byte oriented I/O functions `read-u8`, `peek-u8`, and `write-u8` from R7RS.

Internal Changes

- Relocated all macro expansion code to the `(scheme eval)` module. Cyclone's `(scheme cyclone macros)` library is now obsolete.

Bug Fixes

- Added the `full-unicode` feature since Unicode is supported as of the 0.7 release.

## 0.7 - November 17, 2017

Features

- Finally added Unicode support using UTF-8!
- Allow a program to have macros expand into a top-level `import` expression.
- Added continuous integration support thanks to Alex Arslan.

Bug Fixes

- Incorporated a patch from [0-8-15](https://github.com/0-8-15) to pass seconds to `thread-sleep!` instead of milliseconds. Fractional seconds are accepted as well for high-resolution timers.

## 0.6.3 - September 16, 2017

Features

- Allow the compiler to optimize calls to `+`, `-`, `*`, and `/` that accept more than 2 arguments.
- Added support for bignums to `bitwise-if` from SRFI 60.

Bug Fixes

- Fix `read-line` to remove trailing carriage return and/or newline characters. Thanks to wasamasa for the bug report!
- String ports created by `open-input-string` returned an extra garbage byte. This has been fixed by a patch from wasamasa.
- Prevent segfaults when allocating large strings using `make-string`.
- Added a fix from wasamasa to escape double quotation marks in strings when output via `write`.
- wasamasa patched `read-string` to return EOF when no characters can be read.

## 0.6.2 - August 25, 2017

Features

- Made additional speed increases to `read`.
- Streamlined type checking to make the `remainder` function faster.
- Relocated `string-join` to `(scheme cyclone util)` and added a corresponding `string-split` function.
- Allow optimization of `define-c` functions marked as `inline` even if an alternative non-CPS version of the function is not defined.

Bug Fixes

- Prevent the possibility of segmentation faults when working with large vectors. When a vector is large enough it is allocated directly on the heap, but prior to this fix it was possible that any vector elements on the stack were not moved to the heap during minor GC. This then opens up the possibility of memory corruption when any of those elements are modified, including marking by the runtime's GC.

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

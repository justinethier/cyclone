---
layout: post
title: Released Cyclone Scheme 0.3.4
excerpt: Cyclone Scheme 0.3.4 is now available.
---

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


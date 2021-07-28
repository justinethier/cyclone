---
layout: post
title: Released Cyclone Scheme 0.31.0
excerpt: This release fixes a number of critical bugs.
---

### Bug Fixes

#### Compiler

- Properly handle vectors literals at the top level of compiled code. Previously this could lead to segmentation faults (!!) at runtime.
- Fixed an off-by-one error unpacking arguments when calling a primitive as the continuation after a garbage collection.

#### Base Library

- Fixed `read-line` to prevent data loss when used in conjunction with other I/O functions (such as `read-char`) to read data from the same port. This was because the previous version of `read-line` used a different internal buffer than our other I/O functions.
- Fixed a bug in `make-list` that consumed all available memory when passing a negative list length.
- Allow a record type to contain fields that are not initialized by the constructor.
- Built out `numerator` and `denominator` with code conforming to R7RS.

#### SRFI 18 - Multithreading Library

- Updated `thread-start!` to return the given thread object, per SRFI 18.
- `thread-join!` now returns the result of the thread it was waiting on, per SRFI 18.

#### C Compiler Warnings

- Eliminate clang compiler warnings referencing `EOF` when building the runtime.
- Updated runtime so the C compiler will no longer generate warnings regarding the string comparisons in `Cyc_st_add`. Previously this could result in these warnings being spammed to the console when compiling code using Cyclone.
- Properly escape question marks within strings in generated C code to avoid trigraphs.
- Avoid an "unused variable" warning from the C compiler when compiling certain recursive functions.


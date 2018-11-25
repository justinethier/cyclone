---
layout: post
title: Released Cyclone Scheme 0.9.4
excerpt: This release includes many new compiler optimizations.
---

Compiler Optimizations

- Optimize recursive functions by expressing the recursive calls using C iteration. This optimization is more effective when combined with the others listed below as they collectively increase the chances that a higher-level Scheme loop may be compiled down to a single C function. These C functions can then be "called" repeatedly using a `while` loop which is more efficient at a low level than repeated function calls.
- Combine lambda functions that are only called for side effects.
- Improve inlining of primitives that work with immutable objects.
- Eliminate functions that are only used to define local variables.

Features

- Added a new feature `program` to `cond-expand` that is only defined when compiling a program. This allows, for example, a `.scm` file to contain a section of code that can be used to run unit tests when the file is compiled as a program. The same file can then be used in production to import code into a library. This is similar to using the `__main__` scope in a python program.

Bug Fixes

- Prevent GC segmentation fault on ARM platforms (Raspberry Pi 2).


---
layout: post
title: Released Cyclone Scheme 0.11.3
excerpt: Build instructions for FreeBSD, as well as many bug fixes and enhancements, are provided by this release.
---

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

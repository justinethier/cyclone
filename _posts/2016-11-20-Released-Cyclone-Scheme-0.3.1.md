---
layout: post
title: Released Cyclone Scheme 0.3.1
excerpt: Cyclone Scheme 0.3.1 is now available.
---

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

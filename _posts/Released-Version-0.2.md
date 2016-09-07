---
layout: post
title: Released Cyclone Scheme 0.2
excerpt: Cyclone Scheme 0.2 is now available.
---

Cyclone Scheme 0.2 is now available:

[http://justinethier.github.io/cyclone/](http://justinethier.github.io/cyclone/)

Cyclone consists of a compiler and interpreter targeting as well as written in R7RS Scheme. A garbage collector inspired by Cheney on the M.T.A. is also provided, with support for native threads. 

New features in this release:

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

Bug fixes:

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

Thanks, 

Justin 

----------------- 
User Manual 

[http://justinethier.github.io/cyclone/docs/User-Manual](http://justinethier.github.io/cyclone/docs/User-Manual)

R7RS Compatibility 

[http://justinethier.github.io/cyclone/docs/Scheme-Language-Compliance](http://justinethier.github.io/cyclone/docs/Scheme-Language-Compliance)

Bug Tracker 

[https://github.com/justinethier/cyclone/issues](https://github.com/justinethier/cyclone/issues)


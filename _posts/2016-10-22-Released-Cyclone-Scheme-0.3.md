---
layout: post
title: Released Cyclone Scheme 0.3
excerpt: Cyclone Scheme 0.3 is now available.
---

New features in this release:

- Improve performance by inlining numeric arithmetic and comparison operations.
- Reverted `assq`, `assv`, `memq`, and `memv` back to primitives for improved performance. In addition the compiler was modified to allow for more efficient compilation of `assoc` and `member`.
- Improved library support to recognize all of the import set forms: `only`, `except`, `prefix`, and `rename`.
- Allow explicit renaming macros to be declared interactively. This is the first limited support for calling `define-syntax` from `eval`. 
- Added the `get-environment-variables` function from R7RS.
- Added support for the following SRFI's:

     - SRFI 106: A basic socket interface
     - SRFI 128: Comparators

Bug fixes:

- Improved macro hygiene support to properly rename temporary variables in the `let-values` macro.
- Improve output of `error` by using `write` to output objects as they are represented in memory.
- Check for duplicate lambda parameters during compilation.
- Fixed an error that was being raised when calling `(random-source-randomize! default-random-source)` when using SRFI 27.
- Raise an error if `list->string` encounters a list element that is not a character.


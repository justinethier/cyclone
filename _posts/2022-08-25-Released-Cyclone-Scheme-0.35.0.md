---
layout: post
title: Released Cyclone Scheme 0.35.0
excerpt: Various bug fixes to core functionality
---

Features

- Arthur Maciel added `make-opaque` to `(cyclone foreign)`.
- Add `memory-streams` to the list of symbols that `(features)` can return, indicating that the current installation supports in-memory streams.

Bug Fixes

- Prevent an error when evaluating a `begin` expression that contains both a macro definition and an application of that macro. For example:

      begin (define-syntax foo (syntax-rules () ((foo) 123))) (foo))

- Fix a regression where `c-compiler-options` was not recognized as a top level form by programs.
- Enforce a maximum recursion depth when printing an object via `display` or `write`, and when comparing objects via `equal?`. This prevents segmentation faults when working with circular data structures.
- Added proper implementations of `assv` and `memv`. Both were previously implemented in terms of `assq` and `memq`, respectively.

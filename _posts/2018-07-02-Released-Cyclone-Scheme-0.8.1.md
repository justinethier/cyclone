---
layout: post
title: Released Cyclone Scheme 0.8.1
excerpt: This release includes many performance enhancements and compiler optimizations.
---

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

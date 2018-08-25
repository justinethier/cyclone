---
layout: post
title: Released Cyclone Scheme 0.9.2
excerpt: This release includes various performance enhancements.
---

Features

- During CPS optimization allow inlining of operations on global variables that are mutated in other top-level functions.
- Improved loop detection during CPS optimization phase.
- Allow optimizing-out of basic `if` expressions of the form `(if (pred? ...) #t #f)` into `(pred? ...)`.
- Perform slightly faster type checking for string, vector, and bytevector access functions.

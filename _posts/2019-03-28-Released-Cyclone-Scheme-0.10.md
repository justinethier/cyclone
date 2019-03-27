---
layout: post
title: Released Cyclone Scheme 0.10
excerpt: This release improves performance of most compiled code.
---

Features

- Improve performance in generated C code by allocating data for closures statically where possible, instead of via `alloca`.
- Speed up `case` expressions by using nested `if` expressions instead of the `memv` primitive to evaluate conditions with more than one constant. The nested expressions have better cache locality and also avoid any additional object allocation or initialization.
- Allow passing the `'bin` symbol to `Cyc-installation-dir` to return the location of the installation directory for binaries.

Bug Fixes

- Prevent the possibility of a segmentation fault when passing am improper list to the `member` and `assoc` family of functions.

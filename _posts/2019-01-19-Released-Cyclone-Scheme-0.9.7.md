---
layout: post
title: Released Cyclone Scheme 0.9.7
excerpt: This release contains many compiler optimization fixes and speeds up compilation of large programs. 
---

Features

- Faster version of `list?`.
- Faster compilation of large source files.
- Do not inline `member` or `assoc` to avoid looping over the same list multiple times.

Bug Fixes

- Do not inline primitive calls when arguments to the call are mutated in the function body or mutated elsewhere via `set!`.
- Modified generated code for `(inline)` functions to eliminate the possibility of out-of-order execution.
- Fix a bug where beta expansion of an `if` expression can lead to compilation errors.

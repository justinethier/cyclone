---
layout: post
title: Released Cyclone Scheme 0.29.0
excerpt: This release provides incremental improvements and an implementation of (char-ready?)
---

Features

- Improve performance of runtime by more efficiently unboxing known fixnums. 
- Improve performance of compiled code slightly by using more efficient closure calls when possible.
- Add support for R7RS `#d` decimal specifier for numbers.
- Added `char-ready?` to `(scheme base)`

Bug Fixes

- Avoid generating C code containing unused variables. In addition to generating better code this also prevents the C compiler from raising associated warnings.

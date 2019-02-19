---
layout: post
title: Released Cyclone Scheme 0.9.9
excerpt: This release contains several bug fixes for numeric operations.
---

Bug Fixes

- Fix from Petr Pražák to avoid compilation errors when using newer versions of LibTomMath.
- Avoid cases where bignums are not initialized properly by the runtime and incorrectly retain a value of zero.
- Handle the following edge case from R7RS:

  > If `z` is a complex number, then `(real? z)` is true if and only if `(zero? (imag-part z))` is true. 

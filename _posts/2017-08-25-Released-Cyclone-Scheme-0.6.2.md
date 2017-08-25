---
layout: post
title: Released Cyclone Scheme 0.6.2
excerpt: This release provides additional performance improvements and fixes a potential crash when working with large vectors.
---

Features

- Made additional speed increases to `read`.
- Streamlined type checking to make the `remainder` function faster.
- Relocated `string-join` to `(scheme cyclone util)` and added a corresponding `string-split` function.
- Allow optimization of `define-c` functions marked as `inline` even if an alternative non-CPS version of the function is not defined.

Bug Fixes

- Prevent the possibility of segmentation faults when working with large vectors. When a vector is large enough it is allocated directly on the heap, but prior to this fix it was possible that any vector elements on the stack were not moved to the heap during minor GC. This then opens up the possibility of memory corruption when any of those elements are modified, including marking by the runtime's GC.

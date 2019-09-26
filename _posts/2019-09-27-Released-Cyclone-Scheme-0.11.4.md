---
layout: post
title: Released Cyclone Scheme 0.11.4
excerpt: Significant speedup for mutable operations as well as many critical bug fixes.
---

Bug Fixes

- Modified the minor GC write barrier to only track mutations where a heap variable is modified to point to an object on the stack. This significantly improves performance of mutation operations: `set!`, `set-car!`, etc.
- Fixed an issue with `make-string` and UTF-8 chars. Thanks to Lassi Kortela for the bug report!
- Added `open-binary-input-file` and `open-binary-output-file` from R7RS.
- Validate the number of arguments passed to `if` expressions.
- Raise a useful error instead of aborting the whole program (!) when apply attempts to execute an object of the wrong type.
- Better handling of edge cases where an object of the wrong type is executed instead of a closure. Previously there were cases where this would cause the runtime to crash.


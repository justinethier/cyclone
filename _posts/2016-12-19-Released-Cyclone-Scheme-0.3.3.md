---
layout: post
title: Released Cyclone Scheme 0.3.3
excerpt: Cyclone Scheme 0.3.3 is now available.
---

Features:

- Added SRFI 113 - sets and bags.  
- Improved performance by more aggressively inlining primitives that work with immutable objects, such as the numeric arithmetic and comparison functions.
- Allow the reader to recognize `#true` and `#false`.

Bug Fixes

- Prevent C compilation errors when compiling strings with special characters in them such as `#\tab`, `#\alarm`, and `#\return`.
- Do not generate invalid C code when compiling `+` or `*` without any arguments.
- Handle compilation of variable-argument anonymous lambdas. For example: `((lambda x x) 3 5)`.


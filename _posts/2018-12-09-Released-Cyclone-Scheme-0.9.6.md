---
layout: post
title: Released Cyclone Scheme 0.9.6
excerpt: This release contains bug fixes for the most recent set of compiler optimizations.
---

Bug Fixes

- Cleaned up generated code to ensure calls to primitives from functions that are combined are not executed out of order.
- Fixed the `return_copy` macro in the runtime to handle being passed an expression instead of an object.


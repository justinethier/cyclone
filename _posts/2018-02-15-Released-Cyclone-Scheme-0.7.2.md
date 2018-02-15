---
layout: post
title: Released Cyclone Scheme 0.7.2
excerpt: Cyclone now includes a built-in pattern match library! This release also provides bug fixes for macro hygiene and allocation of large objects.
---

Features

- Added a `(scheme cyclone match)` library based on Alex Shinn's `match.scm` portable hygienic pattern matcher.
- The compiler now emits a faster version of `apply` in cases where only two arguments are received.

Bug Fixes

- Made several improvements to macro hygiene by renaming local variable bindings during expansion. Added a unit test module covering many test cases.
- Fixed many functions including `utf8->string`, `string->utf8`, `bytevector-copy`, `list->vector`, and `list->string` to heap allocate objects that exceed the maximum size for objects on the stack.
- Prevent a compiler error when there is only one argument to `+` or `*`.


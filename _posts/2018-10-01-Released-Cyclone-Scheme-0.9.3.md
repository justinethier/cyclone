---
layout: post
title: Released Cyclone Scheme 0.9.3
excerpt: This release includes various bug fixes.
---

Features

- Allow pretty printing of bytevectors.
- Internal change to the compiler - preserve lambda AST forms throughout compilation process. This should make it easier to implement certain optimizations in future releases.

Bug Fixes

- Fix `input-port?`, `output-port?`, `input-port-open?`, and `output-port-open?` to return `#f` instead of raising an error when a non-port object is passed.
- Fix overflow detection when performing fixnum multiplication to avoid undefined behavior in the C runtime.
- Prevent warnings from the C compiler regarding `shifting a negative signed value is undefined` and `absolute value function 'abs' given an argument of type 'long' but has parameter of type 'int'`.

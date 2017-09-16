---
layout: post
title: Released Cyclone Scheme 0.6.3
excerpt: This release contains many bugfixes from wasamasa and a compiler optimization for numerical operations based on a post from the CHICKEN hackers mailing list.
---

Features

- Allow the compiler to optimize calls to `+`, `-`, `*`, and `/` that accept more than 2 arguments.
- Added support for bignums to `bitwise-if` from SRFI 60.

Bug Fixes

- Fix `read-line` to remove trailing carriage return and/or newline characters. Thanks to wasamasa for the bug report!
- String ports created by `open-input-string` returned an extra garbage byte. This has been fixed by a patch from wasamasa.
- Prevent segfaults when allocating large strings using `make-string`.
- Added a fix from wasamasa to escape double quotation marks in strings when output via `write`.
- wasamasa patched `read-string` to return EOF when no characters can be read.

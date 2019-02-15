---
layout: post
title: Released Cyclone Scheme 0.9.8
excerpt: This release adds a compiler optimization to perform limited memoization of pure functions. 
---

Features

- Added a compiler optimization to memoize recursive calls to pure functions, where possible.

Bug Fixes

- Arthur Maciel fixed a bug in the compiler back-end where a terminating semi-colon would not be emitted in the C code generated for a short program.

Internal Compiler Enhancements

- Arthur Maciel updated the `cgen` module to follow the [Riastradh style rules](https://mumble.net/~campbell/scheme/style.txt) for updating comments. Thanks Arthur!

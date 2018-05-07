---
layout: post
title: Released Cyclone Scheme 0.7.3
excerpt: This release includes many bug fixes as well as performance enhancements for hash tables.
---

Features

- Made several performance improvements to SRFI 69 hash tables, including:
    - Massively improved lookup performance for symbols.
    - Increased the max bound of hash tables to `(2 ^ 30) - 1`.
    - Changed `hash-by-identity` to a high-performance builtin.
- Added a `repl` function to `(scheme repl)` to make it easy to use a REPL from your own code.
- Added basic support for square and curly brackets in place of parentheses.

Bug Fixes

- Fixed an off-by-one error in `read-line` where the function erroneously reported an extra character was read from `stdin`. Thanks to wasamasa for the bug report.
- Fixed a CPS optimization issue where multiple copies of the same lambda are introduced during beta expansion, which causes the optimizer to potentially pick up the wrong value when optimizing-out function calls later on. Thanks to @Chant on Github for providing the report and a test program demonstrating the issue.
- Updated the parser to recognize mnemonic escapes (EG: `\n`, `\a`, etc) and inline hex escapes as part of a symbol.


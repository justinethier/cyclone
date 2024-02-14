---
layout: post
title: Released Cyclone Scheme 0.36.0
excerpt: Long time coming with many fixes to numeric operations.
---

Features

- Enhanced the reader to parse rationals and store them as inexact numbers.
- Add a stub for `(rationalize x y)` to `(scheme base)`.

Bug Fixes

- Yorick Hardy provided a fix to `round` so that Cyclone will round to even when x is halfway between two integers, as required by R7RS.
- Updated various numeric functions to properly handle numeric type conversions, including `quotient`, `remainder`, `numerator`, `denominator`, `truncate`, `truncate-quotient`, and `/`.
- Fix `exact` to properly handle complex numbers, including raising an error when passed `nan` or `inf` double values.
- Ensure the runtime properly differentiates between `+inf.0` and `-inf.0`. Thanks to jpellegrini for the bug report.
- jpellegrini reported that Cyclone returns `#f` when comparing complex numbers using operators other than `=`. Instead it is better to raise an error in these situations.
- lassik and jpellegrini reported that `abs` was incorrectly returning the real part of a complex number argument. Modified `abs` to properly handle complex numbers.
- jpellegrini fixed `(srfi 143)` so that the following are constants instead of procedures: `fx-width`, `fx-greatest`, and `fx-least`.
- Raise an error if `odd?` or `even?` is passed a decimal number. Thanks to jpellegrini for the bug report.
- Fix `read-line` to read entire lines that consist of more than 1022 bytes. Previously the function would only return partial data up to this limit. Thanks to Robby Zambito for the bug report.
- `(include "body.scm")` inside a file `path/to/lib.sld` will look for `path/to/body.scm`, then fallback to the legacy behavior, and look for `$(pwd)/body.scm`.
- Pass append and prepend directories when compiling dependent libraries of a program. This prevents issues where the directories are not made available to any `include` directives within such libraries.
- Updated the reader to throw an error if a number cannot be parsed, rather than returning `#f`.

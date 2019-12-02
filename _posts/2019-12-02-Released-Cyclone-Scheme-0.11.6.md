---
layout: post
title: Released Cyclone Scheme 0.11.6
excerpt: This release contains a wide range of fixes and enhancements.
---

Features

 - Faster record type constructors.
 - During compilation, validate the number of arguments passed to local function calls.
 - Added the `-use-unsafe-prims` compiler option to generate faster code for certain primitives by eliminating runtime safety checks.

Bug Fixes

- Fixed `integer?` such that if `x` is an inexact real number, then `(integer? x)` is true if and only if `(= x (round x))`, per R7RS.
- @extrasharp fixed issues with definitions in `(scheme cxr)`.
- @nymacro fixed a bug in the bootstrap Makefile that prevented builds from working on FreeBSD.

Internals

 - When including an internal `.scm` file used by the compiler, check the current directory before the system directory. This makes it easier to work on certain compiler modules.

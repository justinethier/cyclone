---
layout: post
title: Released Cyclone Scheme 0.15
excerpt: This release makes it easier to integrate with C libraries
---

Features

- Added the `c-linker-options` library directive to allow a library to specify options to pass to the C linker. This prevent compiler errors when linking the final executable.
- Added the `-CLNK` compiler option to specify a custom command to provide as a linker option. For example: `"-lcurl"`.

Bug Fixes

- Fixed the division (`/`) operator to ensure exact arguments will produce inexact results rather than incorrect fixnum results. The example given in R7RS is that `(/ 3 4)` must not return a mathematically incorrect result of `0`. This was previously broken in compiled code though `icyc` would return the correct result.


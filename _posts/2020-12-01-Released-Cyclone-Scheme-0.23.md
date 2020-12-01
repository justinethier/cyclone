---
layout: post
title: Released Cyclone Scheme 0.23
excerpt: This release contains compiler bug fixes for ARM and import expressions added during macro expansion.
---

Bug Fixes

- Fixed compilation error in `runtime.c` on Raspberry Pi / ARM.
- Have the compiler recompute library dependencies for a program if additional import expressions are encountered during macro expansion.

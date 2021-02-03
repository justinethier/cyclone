---
layout: post
title: Released Cyclone Scheme 0.26
excerpt: Added type checking to (cyclone foreign) and fixed regressions related to type checking of records/vectors.
---

Features

- Enhanced `c-define` to emit type checks for parameters. 

Bug Fixes

- Allow `pretty-print` to display contents of a record type.
- Re-enable optimizations that were broken when `vector?` was modified to no longer return true for record types.


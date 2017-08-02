---
layout: post
title: Released Cyclone Scheme 0.5.4
excerpt: This release allows `icyc` to specify additional library import directories and also includes minor performance improvements.
---

Features

- Allow the `-A` and `-I` options to `icyc` for specifying additional library import directories.
- Perform constant folding as part of the CPS optimization phase.
- Statically allocate closures that do not contain any free variables, to avoid unnecessary heap allocations.

Bug Fixes

- Updated `string->number` to return `#f` for all bases when the conversion fails. Previously bases other than ten would return `0` instead.

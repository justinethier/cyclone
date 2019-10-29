---
layout: post
title: Released Cyclone Scheme 0.11.5
excerpt: As of this release, LibTomMath is bundled with Cyclone and no longer a separate dependency.
---

Features

 - Added support for LibTomMath 1.2.0 and integrated that library into our source tree to avoid users having to install the library as a separate dependency.
 - Added the `-no-call-history` flag to potentially allow for faster executables at the expense of call history.

Bug Fixes

- Fix `read-u8`, `peek-u8`, and `write-u8` to work with integers (bytes) instead of characters.

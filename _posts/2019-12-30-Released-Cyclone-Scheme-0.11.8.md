---
layout: post
title: Released Cyclone Scheme 0.11.8
excerpt: Improved support on various platforms and added missing bytevector functions.
---

Features

- Added `read-bytevector`, `read-bytevector!`, and `write-bytevector` I/O functions from R7RS.

Bug Fixes

- Fixed Windows build using MSYS2 and setup a continuous integration job for this platform to prevent breaking this build in the future.
- Tweaked build flags to prevent spamming of compiler warnings when using Clang.
- Modified the compiler to ensure that a variable is not assigned to itself in the generated C code.

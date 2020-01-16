---
layout: post
title: Released Cyclone Scheme 0.12
excerpt: Happy New Year! Cyclone can now automatically build libraries, without the need for makefiles.
---

Features

- When compiling a program with `cyclone` any library dependencies are automatically built as well.
  Note:
  - A library will only be built if an object file does not exist for it or if any of the source files are newer than the object file.
  - Only "local" libraries will be built. Cyclone will not automatically attempt to build libraries in system directories.
  - This capability can be disabled by the `-no-batch` option.
- Added `define-values` from R7RS.

Bug Fixes
- Prevent compilation errors when optimizations are disabled via `-O0`.
- The compiler now omits the continuation argument when printing the expected number of arguments to a function, to avoid confusing error messages.


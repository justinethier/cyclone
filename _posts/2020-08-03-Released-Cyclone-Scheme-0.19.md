---
layout: post
title: Released Cyclone Scheme 0.19
excerpt: This release improves error reporting and includes many bug fixes.
---

Features

- Improved error messages with filename and line numbers are now provided for a wide range of syntax errors.
- Added `c-void` type into `(cyclone foreign)`.

Bug Fixes

- Fixed a bug in the optimized numeric comparison operators when comparing a double with a bignum, that could lead to undefined behavior.
- Fixed `make_empty_bytevector` and `make_c_opaque` parameters on `(cyclone foreign)`.
- Avoid compilation errors when a program ends with a `set!` expression. Thanks to Yorick Hardy for the bug report!
- Fixed a bug with the interpreter such that running an `import` will no longer remove definitions from the global environment.
- Fixed another interpreter bug to allow `import` to load renamed identifiers.
- Prevent compiler errors when calling an anonymous lambda that accepts an optional argument list.

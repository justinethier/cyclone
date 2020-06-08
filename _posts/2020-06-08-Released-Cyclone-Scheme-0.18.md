---
layout: post
title: Released Cyclone Scheme 0.18
excerpt: With this release we proudly present our new package manager, Cyclone-Winds!
---

The main focus of this release is Arthur Maciel's Cyclone-Winds package manager, the official package manager for Cyclone Scheme! Cyclone-Winds contains a growing list of packages and is an easy way to distribute programs and libraries built using Cyclone.

The `cyclone-winds` binary is installed as part of our [bootstrap](https://github.com/justinethier/cyclone-bootstrap) installation. Alternatively it can be built and installed separately if you already have a copy of Cyclone installed, though version 0.18 or higher is required.

Please visit the [Cyclone-Winds](https://github.com/cyclone-scheme/cyclone-winds) site for more information.

Features

- Added the `(cyclone foreign)` library to make it easier to integrate with C code using the FFI.
- Arthur Maciel improved how `icyc` handles the "-p" flag, to now read/evaluate all expressions passed.

Bug Fixes

- Added a void type and a new supporting function `(void)`. A void object is also now returned in situations where the return value is undefined: `for-each` and `if` expressions with out an else clause.
- Allow `open-binary-input-file` and `open-binary-output-file` to be accessible via the REPL.
- Fix `bytevector?` predicate which was accidentally aliased to `vector?`.
- Fix `list-copy` to return a non-list object instead of raising an error, per R7RS.
- Fixed `eqv?` to use R7RS semantics to ensure equality of different instances of the same numeric value. The function was previously just an alias of `eq?`.
- Support two-argument version of `atan`.
- Support `start` and `end` arguments to `write-string`.
- Updated the string comparison procedures (EG: `string=?`) to support more than two arguments.

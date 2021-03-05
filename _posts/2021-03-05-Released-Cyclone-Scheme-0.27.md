---
layout: post
title: Released Cyclone Scheme 0.27
excerpt: This release contains enhancements to our C integration as well as various bug fixes.
---

Features

- Arthur Maciel added `opaque?` and `opaque-null?` predicates to `(cyclone foreign)`.
- Added `import-shared-object` to `(scheme eval)` to allow loading a third party C shared library.
- Allow C compiler/linker options from a library to be expanded via `cond-expand`.

Bug Fixes

- Updated the runtime to avoid a race condition when creating new symbols. Thanks to Skye Soss for the bug report and patch.
- Prevent the compiler from inlining calls to primitives that open ports, avoiding a range of issues such as an open file operation being inlined across multiple places in the intermediate code. Thanks to Andy Arvanitis for the bug report.
- Arthur Maciel updated `current-jiffy` to use `clock_gettime`.

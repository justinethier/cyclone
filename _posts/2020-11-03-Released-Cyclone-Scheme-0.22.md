---
layout: post
title: Released Cyclone Scheme 0.22
excerpt: Many improvements focused on compiler usability and interoperability.
---

Features
- Added definitions from SRFI 162 (comparators sublibrary) to the `(srfi 128)` library.
- Cleaned up printing of call history to make it more obvious which functions were called most recently.

Bug Fixes

- The compiler now displays a helpful error message to the user when compilation of a program fails due to an error building a dependent library.
- Enhanced `cond-expand` support in a library declaration to
  - Properly handle cases where there is more than one `cond-expand` sub-expression. EG: two `begin` expressions, a `begin` / `import`, etc.
  - Properly handle the case where `export` is defined by `cond-expand`. Previously such exports would not be recognized by code importing the library.


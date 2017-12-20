---
layout: post
title: Released Cyclone Scheme 0.7.1
excerpt: This release improves support for macros by (finally) including `let-syntax` and `letrec-syntax` as well as enhancing `eval` to allow defining `syntax-rules` macros. The release also adds a few minor features from R7RS that had been missing.
---

Features

- Added support for `let-syntax` and `letrec-syntax`.
- Added the `(scheme repl)` library and `interaction-environment` function from R7RS.
- Allow `eval` to recognize `syntax-rules` macros.
- Added single-byte oriented I/O functions `read-u8`, `peek-u8`, and `write-u8` from R7RS.

Internal Changes

- Relocated all macro expansion code to the `(scheme eval)` module. Cyclone's `(scheme cyclone macros)` library is now obsolete.

Bug Fixes

- Added the `full-unicode` feature since Unicode is supported as of the 0.7 release.


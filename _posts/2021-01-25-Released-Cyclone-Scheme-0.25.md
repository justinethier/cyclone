---
layout: post
title: Released Cyclone Scheme 0.25
excerpt: Our package manager has been renamed to Winds.
---

Features

- Our package manager has been renamed to `winds`. Thanks Arthur Maciel! Please upgrade to this release to continue using the package manager as many of the links have been modified to point to the newly-renamed repository.
- Added the `c-compiler-options` expression and `-COPT` Cyclone compiler option to allow specifying options for the C compiler.
- Allow `c-linker-options` to work as a top-level expression in a program.

Bug Fixes

- Fix import of library dependencies from the interpreter, when the dependencies are imported via `cond-expand`.
- Arthur Maciel fixed `when` and `unless` to no longer return `#f` in cases where the expression body is not executed.
